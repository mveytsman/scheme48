module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad


symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

quotedString :: Parser Char
quotedString =  ((char '\\') >> (char '"')) <|> (noneOf "\"")

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
                x <- many quotedString
                     
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
                         _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = many1 digit >>= (return . Number . read)
                
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
        
