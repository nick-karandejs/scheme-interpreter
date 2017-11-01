module Main where

import System.Environment
import Parser (readExpr)
import Evaluator (eval)


{- schemeParse :: IO () -}
{- schemeParse = do -}
{-     (expr:_) <- getArgs -}
{-     putStrLn $ readExpr expr -}

main :: IO ()
main = getArgs >>= print . eval . readExpr . head

