module Main where

import System.Environment
import Parser (readExpr)
import Evaluator (eval)
import Common (extractValue, trapError)
import REPL
import Control.Monad


main :: IO ()
main = do
    args <- getArgs
    case length args of
        0 -> runRepl
        1 -> runOne $ head args

