module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Numeric (readHex, readOct)


symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space


readBin :: String -> Integer
readBin = foldl binRead 0
        where binRead acc c = 2 * acc + (read [c]) -- ugh 


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

showLispVal :: LispVal -> String
showLispVal input = case input of
  Atom atom -> "Atom: " ++ atom
  List list ->  "List: " ++ map showLispVal list !! 0
  Number num -> "Num: " ++ show num
  String str -> "String: " ++ "\"" ++ str ++ "\""
  Bool bool -> "Bool: " ++ show bool
  Char chr -> "Char: " ++ show chr

  


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

parseSpecial :: Parser LispVal
parseSpecial = do
  char '#'
  parseBool <|> parseNumberLiteral -- <|> parseCharLiteral


parseDec :: Parser LispVal
parseDec = many1 digit >>= (return . Number . read)

parseHex :: Parser LispVal
parseHex = liftM (Number . fst . (!! 0) . readHex) $ many1 (digit <|> oneOf "abcdefABCDEF")

parseOct :: Parser LispVal
parseOct = liftM (Number . fst . (!! 0) . readOct) $ many1 (oneOf "01234567")


parseBin :: Parser LispVal
parseBin = liftM (Number . readBin) $ many1 (oneOf "01")

           
parseExpr :: Parser LispVal
parseExpr = (try parseSpecial)
          <|> parseAtom
          <|> parseString
          <|> parseDec

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match " ++ show err
  Right val -> "Found value: " ++ (showLispVal val)  


 
main :: IO ()
main = do
  args <- getArgs
  putStrLn $ readExpr (args !! 0)
        
