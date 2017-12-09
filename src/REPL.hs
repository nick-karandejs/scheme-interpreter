module REPL (
    runRepl
  , runOne
  , evalAndPrint 
)
where

import System.IO
import Control.Monad
import Parser (readExpr)
import Evaluator (eval, primitiveBindings)
import Common
import Environment


stdlibPath = "src/stdlib.scm"


flushStr :: String -> IO ()
flushStr s = putStr s >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr =
    runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

-- Use underscore for monadic functions that repeat but don't return a value
until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
    input <- prompt
    if pred input
        then return ()
        else action input >> until_ pred prompt action

-- Filename and arguments to evaluate with
runOne :: [String] -> IO ()
runOne args = do
-- nullEnv >>= flip evalAndPrint expr
    env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
    runIOThrows $ liftM show $ eval env (List [Atom "load", String $ args!!0])
    >>= hPutStrLn stderr

runRepl :: IO ()
runRepl = do
    env <- primitiveBindings
    result <- runIOThrows $ liftM show $ eval env (List [Atom "load", String stdlibPath])
    hPutStrLn stderr result
    return env >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint
