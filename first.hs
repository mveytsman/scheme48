 module Main where
 import System.Environment
 
 main :: IO ()
 main = do
   putStrLn "What's your name?"
   name <- getLine
   putStrLn ("Hello, " ++ name)
     
