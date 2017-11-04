module Main where

import System.Environment
import Parser (readExpr)
import Evaluator (eval)
import Common (extractValue, trapError)
import Control.Monad


main :: IO ()
main = do
    arg <- liftM head $ getArgs
    result <- return $ liftM show $ readExpr arg >>= eval
    putStrLn $ extractValue $ trapError result

