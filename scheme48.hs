module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Control.Monad.Error
import Numeric (readHex, readOct, readFloat)


-- PARSING --

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~\\"

spaces :: Parser ()
spaces = skipMany1 space


readBin :: String -> Integer
readBin = foldl binRead 0
        where binRead acc c = 2 * acc + (read [c]) -- ugh

unwrappingReader :: ReadS a -> (String -> a) -- we know more about the type of b than this, try with Num typeclass (Num a =>)
unwrappingReader reader = fst . (!! 0) . reader

newline2 :: Parser Char
newline2 = (char '\\') >> (char 'n')

-- TODO: make this a datatype or something easily extensible
escapeParser :: Parser Char
escapeParser = (char '\\') >> ((char '"')
               <|> (char 'n' >> return '\n')
               <|> (char 'r' >> return '\r')
               <|> (char 't' >> return '\t')
               <|> (char '\\'))

fancyString :: Parser Char
fancyString =   choice [escapeParser, (noneOf "\"")]

instance Show LispVal where show = showVal
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Char Char
             | Float Double


showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (Float contents) = show contents

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many fancyString
                char '"'
                return $ String x

parseAtom :: Parser LispVal
parseAtom = do 
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ Atom atom

parseBool :: Parser LispVal
parseBool = do
  c <- oneOf "tf"
  notFollowedBy (letter <|> digit <|> symbol)
  return $ case c of
    't' -> Bool True
    'f' -> Bool False

parseNumberLiteral :: Parser LispVal
parseNumberLiteral = do
  base <- oneOf "dxob"
  case base of
    'd' -> parseDec
    'x' -> parseHex
    'o' -> parseOct
    'b' -> parseBin

parseCharLiteral :: Parser LispVal
parseCharLiteral = do
  char '\\'
  x <- anyChar
  notFollowedBy (letter <|> digit <|> symbol)
  return $ Char x
  
parseSpecial :: Parser LispVal
parseSpecial = do
  char '#'
  parseBool <|> parseNumberLiteral <|> parseCharLiteral


-- TODO: Exercise 7, chap 2: full numeric tower (complex, rational, etc)

parseDec :: Parser LispVal
parseDec = many1 digit >>= (return . Number . read)

parseHex :: Parser LispVal
parseHex = liftM (Number . (unwrappingReader readHex)) $ many1 (digit <|> oneOf "abcdefABCDEF")

parseOct :: Parser LispVal
parseOct = liftM (Number . fst . (!! 0) . readOct) $ many1 (oneOf "01234567")


parseBin :: Parser LispVal
parseBin = liftM (Number . readBin) $ many1 (oneOf "01")

parseFloat :: Parser LispVal
parseFloat = do
  integral <- many digit
  char '.'
  integrand <- many1 digit
  let floatString = integral ++ "." ++ integrand
  let float = fst $ (readFloat floatString) !! 0
  return $ Float float

parseExpr :: Parser LispVal
parseExpr = (try parseSpecial)
          <|> parseAtom
          <|> parseString
          <|> try parseFloat
          <|> parseDec
          <|> parseQuoted
          <|> do
                char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

readExpr :: String -> ThrowsError LispVal -- String -> (Either LispError LispVal)
readExpr input = case parse parseExpr "lisp" input of
  Left err ->  throwError $ Parser err
  Right val -> return val  

-- ERRORS --
data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected 
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr
 
instance Show LispError where show = showError

instance Error LispError where
    noMsg = Default "An error has occurred"
    strMsg = Default

type ThrowsError = Either LispError
   
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
               
-- EVALUATING --

eval :: LispVal -> ThrowsError LispVal -- (LispVal -> Either LispError LispVal)
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval val@(Float _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm  "Unrecognized special form" badForm
               

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

class (Num a) => LispNum a where
  makeLispVal :: a -> LispVal

instance LispNum Integer where
    makeLispVal n = Number n

instance LispNum Double where
    makeLispVal d = Float d

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
             
              ("symbol?", isSymbol),
              ("list?",   isList),
              ("dotted-list?", isDottedList),
              ("number?", isNumber),
              ("string?", isString),
              ("bool?", isBool),
              ("char?", isChar),
              ("float?", isFloat),

              ("symbol->string", (return . String . show . head)),
              ("string->symbol", (return . Atom . show . head))]
                        
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op            [] = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params >>=  return . Number . foldl1 op


numericBinop op [(Float f) (Float f) ...]

numericBinop op (Float f) (Float f') = Float (op f f')

                                       
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
--unpackNum (Float n) = return n
unpackNum (String n) = let parsed = reads n in 
                           if null parsed 
                             then throwError $ TypeMismatch "number" $ String n
                             else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

                       
isSymbol :: [LispVal] -> ThrowsError LispVal
isSymbol [(Atom _)] = return $ Bool True
isSymbol [_] = return $ Bool False
isSymbol params = throwError $ NumArgs 1 params

isList :: [LispVal] -> ThrowsError LispVal
isList [(List _)] = return $ Bool True
isList [_] = return $ Bool False
isList params = throwError $ NumArgs 1 params
                  
isDottedList :: [LispVal] -> ThrowsError LispVal
isDottedList [(DottedList _ _)] = return $ Bool True
isDottedList [_] = return $ Bool False
isDottedList params = throwError $ NumArgs 1 params

                   
isNumber :: [LispVal] -> ThrowsError LispVal
isNumber [(Number _)] = return $ Bool True
isNumber [_] = return $ Bool False
isNumber params = throwError $ NumArgs 1 params

isString :: [LispVal] -> ThrowsError LispVal
isString [(String _)] = return $ Bool True
isString [_] = return $ Bool False
isString params = throwError $ NumArgs 1 params

isBool :: [LispVal] -> ThrowsError LispVal
isBool [(Bool _)] = return $ Bool True
isBool [_] = return $ Bool False
isBool params = throwError $ NumArgs 1 params
          
isChar :: [LispVal] -> ThrowsError LispVal
isChar [(Char _)] = return $ Bool True
isChar [_] = return $ Bool False
isChar params = throwError $ NumArgs 1 params

isFloat :: [LispVal] -> ThrowsError LispVal
isFloat [(Float _)] = return $ Bool True
isFloat [_] = return $ Bool False         
isFloat params = throwError $ NumArgs 1 params

 
-- Main interpreter

main :: IO ()
main =  do
  args <- getArgs
  evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
  putStrLn $ extractValue $ trapError evaled
                          
          
        
