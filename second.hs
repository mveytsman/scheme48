module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Numeric (readHex, readOct, readFloat)


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

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match " ++ show err
  Right val -> "Found value: " ++ (showVal val)  


 
main :: IO ()
main = do
  args <- getArgs
  putStrLn $ readExpr (args !! 0)
        
