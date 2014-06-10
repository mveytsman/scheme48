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

showLispVal :: LispVal -> String
showLispVal input = case input of
  Atom atom -> atom
  List list ->  map showLispVal list !! 0
  Number num -> show num
  String str -> "\"" ++ str ++ "\""
  Bool bool -> show bool

  


parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many fancyString
                     
                --char '"'
                return $ String x


parseAtom :: Parser LispVal
parseAtom = do 
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of 
                         "#t" -> Bool True
                         "#f" -> Bool False
                         -- this is gross! NOTE: ensure that dec,hex,oct,bin are restricted to the right charset
                         '#':'d':dec -> Number (read dec)
                         '#':'x':hex -> Number $ fst (readHex hex !! 0)
                         '#':'o':oct -> Number $ fst (readOct oct !! 0)
                         '#':'b':bin -> Number $ readBin bin 
                         _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = do
  base <- ((char '#') >> (char 'd')) <|> digit
  rest <- many digit
  --return $ case base of
  --  'd' -> Number (read rest)
  --  _ -> Number (read $ base:rest)
  many1 digit >>= (return . Number . read)
                
parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match " ++ show err
  Right val -> "Found value: " ++ (showLispVal val)  


 
main :: IO ()
main = do
  args <- getArgs
  putStrLn (readExpr (args !! 0))
        
