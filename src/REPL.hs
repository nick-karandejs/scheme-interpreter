module REPL (
    runRepl
  , evalAndPrint 
)
where

import System.IO
import Control.Monad
import Parser (readExpr)
import Evaluator (eval)
import Common


flushStr :: String -> IO ()
flushStr s = putStr s >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> IO String
evalString expr = return $ extractValue $ trapError $ liftM show $ readExpr expr >>= eval

evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn

-- Use underscore for monadic functions that repeat but don't return a value
until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
    input <- prompt
    if pred input
        then return ()
        else action input >> until_ pred prompt action

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "Lisp>>> ") evalAndPrint
