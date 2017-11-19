module Evaluator (
    eval
  , primitiveBindings
) where

import Common
import Control.Monad.Except
import Data.List
import Environment

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericOp (+)),
              ("-", numericOp (-)),
              ("*", numericOp (*)),
              ("/", numericOp div),
              ("mod", numericOp mod),
              ("quotient", numericOp quot),
              ("remainder", numericOp rem),
              ("=", numBoolMonoid (==)),
              ("<", numBoolMonoid (<)),
              ("<=", numBoolMonoid (<=)),
              (">", numBoolMonoid (>)),
              (">=", numBoolMonoid (>=)),
              ("string=?", strBoolMonoid (==)),
              ("string<?", strBoolMonoid (<)),
              ("string>?", strBoolMonoid (>)),
              ("string<=?", strBoolMonoid (<=)),
              ("string>=?", strBoolMonoid (>=)),
              ("&&", boolBoolMonoid (&&)),
              ("||", boolBoolMonoid (||)),
              ("number?", typeTest isNum),
              ("string?", typeTest isString),
              ("cons", cons),
              ("car", car),
              ("cdr", cdr),
              ("eqv?", eqv)
              ]


primitiveBindings :: IO Env
primitiveBindings =
    nullEnv >>= (flip bindVars) (map makePrimitive primitives)
    where makePrimitive (name, fun) = (name, PrimitiveFun fun)

eval :: Env -> LispVal -> IOThrowsError LispVal
-- using `@` we capture the passed value (LispVal) rather than String value
eval env val@(String _) = return val

eval env val@(Number _) = return val

eval env val@(Bool _) = return val

eval env (Atom id) = getVar env id

eval env (List [Atom "quote", val]) = return val

eval env (List [Atom "if", pred, conseq, alt]) = do
    predResult <- eval env pred
    -- Anything apart from #f is considered #t
    case predResult of
        Bool False -> eval env alt
        Bool True -> eval env conseq
        otherwise -> throwError $ TypeMismatch "bool" predResult

eval env (List [Atom "set!", Atom var, form]) =
    eval env form >>= setVar env var

eval env (List [Atom "define", Atom var, form]) =
    eval env form >>= defineVar env var

eval env (List ((Atom "cond") : clauses)) = do
    -- evalClauses should end up being either a LispError or a list of lvalues
    evalClauses <- mapM (evalClauseTest env) clauses
    case evalClauses of
        (x:xs) -> do
            let pairs = zip evalClauses clauses
                getBool (Bool v, _) = v
                resultPair = find getBool pairs
            case resultPair of
                Just (_, expr) -> evalClauseExpr env expr
                Nothing -> throwError $ RuntimeError "No test was true in" (List clauses)
        otherwise -> throwError $ TypeMismatch "bool" (Bool False)

eval env (List (function : args)) = do
    fun <- eval env function
    argValues <- mapM (eval env) args
    liftThrows $ apply fun argValues

        
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form " badForm

-- Working on clauses with exactly one expression
evalClauseTest :: Env -> LispVal -> IOThrowsError LispVal
evalClauseTest env (List [test, _]) = eval env test 
evalClauseExpr :: Env -> LispVal -> IOThrowsError LispVal
evalClauseExpr env (List [_, expr]) = eval env expr 

numericOp :: (Integer -> Integer -> Integer)
                 -> [LispVal] -> ThrowsError LispVal
numericOp _ [] = throwError $ NumArgs 2 []
numericOp _ val@[_] = throwError $ NumArgs 2 val
numericOp op params = mapM unpackNum params >>= return . Number . foldl1 op

numBoolMonoid :: (Integer -> Integer -> Bool) -> [LispVal]
                  -> ThrowsError LispVal
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

eqv :: [LispVal] -> ThrowsError LispVal
eqv [Number arg1, Number arg2] = return $ Bool $ arg1 == arg2
eqv [String arg1, String arg2] = return $ Bool $ arg1 == arg2
eqv [Bool arg1, Bool arg2] = return $ Bool $ arg1 == arg2
eqv [Atom arg1, Atom arg2] = return $ Bool $ arg1 == arg2
eqv [DottedList xs x, DottedList ys y] = eqv [List (xs++[x]), List (ys++[y])]
eqv [List xs, List ys] =
    if length xs /= length ys
        then return $ Bool False
        else return $ Bool $ all binEqv (zip xs ys)
                          where binEqv (x, y) =
                                  case eqv [x, y] of
                                      Left err -> False
                                      Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList
--
-- List primitives
-- | Returns the first element of a list.
car :: [LispVal] -> ThrowsError LispVal
car [List (x:_)] = return x
car [DottedList (x:_) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

-- | Returns the tail of the list.
cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_:xs)] = return $ List xs
cdr [DottedList (_:xs) tail] = return $ DottedList xs tail
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

-- Constructs a list from two elements
cons :: [LispVal] -> ThrowsError LispVal
-- List [] is meant to represent a `Nil` in lisp
cons [x, List []] = return $ List [x]
cons [x, List xs] = return $ List (x:xs)
cons [x, DottedList xs tail] = return $ DottedList (x:xs) tail
cons [x1, x2] = return $ DottedList [x1] x2
cons badArg = throwError $ NumArgs 2 badArg

typeTest :: (LispVal -> Bool) -> [LispVal] -> ThrowsError LispVal
typeTest op (arg:_) = return $ Bool $ op arg
typeTest op [] = throwError $ NumArgs 1 []

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum val = throwError $ TypeMismatch "number" val

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr val = throwError $ TypeMismatch "string" val

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool val = throwError $ TypeMismatch "boolean" val

isNum :: LispVal -> Bool
isNum (Number _) = True
isNum _ = False

isString :: LispVal -> Bool
isString (String _) = True
isString _ = False

apply :: LispVal -> [LispVal] -> ThrowsError LispVal
apply (PrimitiveFun fun) args = fun args
