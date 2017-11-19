module Common (
    LispVal(..)
  , ThrowsError(..)
  , LispError(..)
  , Env
  , IOThrowsError
  , extractValue
  , trapError
) where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad.Except
import Data.IORef

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | PrimitiveFun ([LispVal] -> ThrowsError LispVal)
             -- closure is the environment the function was created in
             | Fun {params :: [String], vararg :: (Maybe String),
                    body :: [LispVal], closure :: Env}


instance Show LispVal where
    show (String contents) = "\"" ++ contents ++ "\""
    show (Atom name) = name
    show (Number contents) = show contents
    show (Bool True) = "#t"
    show (Bool False) = "#f"
    show (List contents) = "(" ++ (unwords . map show) contents ++ ")"
    show (DottedList head tail) =
        "(" ++ (unwords . map show) head ++ " . " ++ show tail ++ ")"
    show (PrimitiveFun _) = "<primitive>"
    show (Fun {params=args, vararg=varargs, body=body, closure=env}) =
        let textArgs = map show args
            varargsStr = case varargs of
                Nothing -> ""
                Just s -> " . " ++ s
        in "(lambda (" ++ unwords textArgs ++ varargsStr ++ ") ...)"



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

-- IORef specifies a mutable variable inside the IO monad
type Env = IORef [(String, IORef LispVal)]

-- Monad containing IO actions that may throw an error
-- We specify exception type to be LispError and the inner monad is IO
type IOThrowsError = ExceptT LispError IO

-- This should give text info defined for LispErrors
-- Instead of throwing exception by system, handle it ourselves
trapError action = catchError action (return . show)
extractValue :: ThrowsError a -> a
-- Not defining Left on purpose, since only using this after catchError
extractValue (Right val) = val


makeFun :: Maybe String -> Env -> [String] -> [LispVal] -> IOThrowsError LispVal
makeFun varargs env params body =
    return $ Fun (map show params) varargs body env

makeNormalFun :: Env -> [String] -> [LispVal] -> IOThrowsError LispVal
makeNormalFun = makeFun Nothing

makeVarArgFun :: String -> Env -> [String] -> [LispVal] -> IOThrowsError LispVal
makeVarArgFun = makeFun . Just . show
