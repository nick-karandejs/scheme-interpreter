module Main where

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
-- import Lib

data LispVal = Atom String
             | List [ListVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

parseString :: Parser LispVal
parseString = do
    -- absorb the double quote
    char '"'
    -- read upto the first double quote
    x <- many (noneOf "\"")
    char '"'
    -- apply the Lisp constructor
    return $ String x


symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
-- gives us a parser that recognizes one or more spaces
spaces = skipMany1 space


readExpr :: String -> String
-- `>>` is a bind operator; behaves differently for each Monad
-- Here is will try to match spaces and then try matching the rest with symbols
readExpr input = case parse (spaces >> symbol) "lisp" input of
    Left err -> "No match " ++ show err
    Right val -> "Found value " ++ (show val)

main :: IO ()
main = do
    (expr:_) <- getArgs
    putStrLn $ readExpr expr
    {- putStrLn $ "Got" ++ (args!!0) -}
-- main = someFunc
