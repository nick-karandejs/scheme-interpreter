module Evaluator (
    eval
) where

import Common
import Control.Monad.Except


eval :: LispVal -> ThrowsError LispVal
-- using `@` we capture the passed value (LispVal) rather than String value
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom fun : args)) = mapM eval args >>= apply fun
eval badForm = throwError $ BadSpecialForm "Unrecognized special form " badForm

numericMonoid :: (Integer -> Integer -> Integer)
                 -> [LispVal] -> ThrowsError LispVal
numericMonoid _ [] = throwError $ NumArgs 2 []
numericMonoid _ val@[_] = throwError $ NumArgs 2 val
numericMonoid op params = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum val = throwError $ TypeMismatch "number" val

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr val = throwError $ TypeMismatch "string" val

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool val = throwError $ TypeMismatch "boolean" val

-- TODO: add weak typing? i.e. to accept string as number as well

-- TODO: add more primitives
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericMonoid (+)),
              ("-", numericMonoid (-)),
              ("*", numericMonoid (*)),
              ("/", numericMonoid div),
              ("mod", numericMonoid mod),
              ("quotient", numericMonoid quot),
              ("remainder", numericMonoid rem),
              ("number?", typeTest isNum),
              ("string?", typeTest isString),
              ("=", numBoolMonoid (==)),
              ("string=?", strBoolMonoid (==)),
              ("&&", boolBoolMonoid (&&))]


numBoolMonoid :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolMonoid = boolMonoid unpackNum

strBoolMonoid :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolMonoid = boolMonoid unpackStr

boolBoolMonoid :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolMonoid = boolMonoid unpackBool

boolMonoid :: (LispVal -> ThrowsError a) -> (a -> a -> Bool)
              -> [LispVal] -> ThrowsError LispVal
boolMonoid unpacker op args =
    if length args /= 2
    then throwError $ NumArgs 2 args
    else do
        arg0 <- unpacker $ args !! 0
        arg1 <- unpacker $ args !! 1
        return . Bool $ op arg0 arg1
{- numBoolMonoid op [arg1, arg2] = return $ Bool $ op (unpackNum arg1) (unpackNum arg2) -}


typeTest :: (LispVal -> Bool) -> [LispVal] -> ThrowsError LispVal
typeTest op (arg:_) = return $ Bool $ op arg
typeTest op [] = throwError $ NumArgs 1 []

isNum :: LispVal -> Bool
isNum (Number _) = True
isNum _ = False

isString :: LispVal -> Bool
isString (String _) = True
isString _ = False

apply :: String -> [LispVal] -> ThrowsError LispVal
-- `maybe` allows to specify what to do in case of success and failure
-- here in case `lookup` fails we return False, otherwise the lookup returns
-- a function that we want to apply to args, hence `$ args`
apply fun args =
    maybe
        (throwError $ NotFunction "Unrecognized primitive function args" fun)
        ($ args)
        (lookup fun primitives)

