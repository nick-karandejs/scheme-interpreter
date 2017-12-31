module ParserInternal where

import Control.Monad
import Control.Monad.Except
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Common


type ParserVal = Parser LispVal


symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

escQuote :: Parser Char
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
-- Parses expressions separated by spaces (str, num, brackets, ...)
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: ParserVal
parseDottedList = do
    -- Parse expressions that have space at the end
    head <- endBy parseExpr spaces
    -- `>>` is a bind operator; is equivalent to multiple do statements
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: ParserVal
parseQuoted = do
    x <- char '\'' >> parseExpr
    return $ List [Atom "quote", x]

-- | Returns a Lisp value from input string
readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input =
    case parse parser "lisp" input of
        Left err -> throwError $ Parser err
        Right val -> return val

-- | Parser for a Lisp expression
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

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow (endBy parseExpr spaces)
