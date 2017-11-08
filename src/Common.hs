module Common (
    LispVal(..)
  , ThrowsError(..)
  , LispError(..)
  , extractValue
  , trapError
) where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad.Except

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool


instance Show LispVal where
    show (String contents) = "\"" ++ contents ++ "\""
    show (Atom name) = name
    show (Number contents) = show contents
    show (Bool True) = "#t"
    show (Bool False) = "#f"
    show (List contents) = "(" ++ (unwords . map show) contents ++ ")"
    show (DottedList head tail) =
        "(" ++ (unwords . map show) head ++ " . " ++ show tail ++ ")"



data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | RuntimeError String LispVal
               | Default String

instance Show LispError where
    show (NumArgs expected found) =
        unwords ["Expected", show expected,
                 "args; found values", unwords $ map show found]
    show (TypeMismatch expected found) =
        unwords ["Invalid type: expected", expected, "found", show found]
    show (Parser parseErr) = "Parse error at " ++ show parseErr
    show (BadSpecialForm message form) = unwords [message, ":", show form]
    show (NotFunction message fun) = unwords [message, ":", show fun]
    show (UnboundVar message varname) = unwords [message, ":", varname]
    show (RuntimeError message val) = unwords [message, ":", show val]


type ThrowsError = Either LispError

trapError action = catchError action (return . show)
extractValue :: ThrowsError a -> a
-- Not defining Left on purpose
extractValue (Right val) = val
