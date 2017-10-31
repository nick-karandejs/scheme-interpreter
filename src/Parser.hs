module Parser
    ( parseExpr
    , schemeParse
    ) where

import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             deriving Show

type ParserVal = Parser LispVal

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

escQuote = do
    char '\\'
    char '"'

spaces :: Parser ()
-- gives us a parser that recognizes one or more spaces
spaces = skipMany1 space

parseString :: ParserVal
parseString = do
    -- absorb the double quote
    char '"'
    -- read upto the first double quote
    x <- many $ escQuote <|> (noneOf "\"")
    char '"'
    -- apply the Lisp constructor
    return $ String x


parseAtom :: ParserVal
parseAtom = do
    -- try to parse letter, if failed try parsing symbol; `<|>` - choice op
    first <- letter <|> symbol
    rest <- many $ letter <|> digit <|> symbol
    let atom = first:rest
    return $ case atom of
                "#t" -> Bool True
                "#f" -> Bool False
                _ -> Atom atom

parseNumber :: ParserVal
parseNumber = liftM (Number . read) $ many1 digit

parseList :: ParserVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: ParserVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: ParserVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]


parseExpr :: ParserVal
parseExpr =
    parseAtom
    <|> parseString
    <|> parseNumber
    <|> parseQuoted
    <|> do char '('
           -- `try` attempts to run a parser and if it fails puts state back 
           x <- try parseList <|> parseDottedList
           char ')'
           return x

readExpr :: String -> String
-- `>>` is a bind operator; behaves differently for each Monad
-- Here is will try to match spaces and then try matching the rest with symbols
-- readExpr input = case parse (spaces >> symbol) "lisp" input of
readExpr input =
    case parse parseExpr "lisp" input of
        Left err -> "No match " ++ show err
        Right val -> "Value matched " ++ (show val)

schemeParse :: IO ()
schemeParse = do
    (expr:_) <- getArgs
    putStrLn $ readExpr expr
