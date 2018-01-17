module Main where

import Data.List
import Control.Monad.Except
import System.IO
import Test.Tasty
import Test.Tasty.HUnit

import Text.ParserCombinators.Parsec hiding (spaces)

import ParserInternal as P
import Common
import Evaluator


readString s = P.readOrThrow P.parseString s
-- env = liftIO $ primitiveBindings

parserTests :: TestTree
parserTests =
    testGroup "Parser unit tests" [
        testCase "String with escaped characters" $
        (Right $ String "\"x") @=? (readString "\"\\\"x\"")
      , testCase "String parse" $
        assertBool "" $ (Right $ String "\nx") /= (readString "\"\\nx\"")
      , testCase "Errored string parse" $
        assertBool "" $ (Right $ String "x") /= (readString "x")
      , testCase "Parse list" $
        (Right $ List $ map Number [1, 2, 3]) @=? (readExpr "(1 2 3)")
      , testCase "Parse dotted list" $
            (Right $ DottedList (map Number [1, 2]) (Number 3))
            @=? (readExpr "(1 2 . 3)")
      , testCase "Parse atom" $
        (Right $ Atom "define") @=? (readExpr "define")
      , testCase "Parse boolean atom" $
        (Right $ Bool True) @=? (readExpr "#t")
    ]

runEval :: LispVal -> IO (Either LispError LispVal)
runEval expr = do
    env <- primitiveBindings
    runExceptT $ eval env expr

compareIOResult :: (Eq a, Monad m) => a -> m a -> m Bool
compareIOResult expected actual = do
    actual' <- actual
    return $ expected == actual'

evaluatorTests :: TestTree
evaluatorTests =
    testGroup "Evaluator unit tests" [
        testCase "If statement" $ compareIOResult exp1 res1 @? ""
      , testCase "Addition" $ compareIOResult exp2 res2 @? ""
      , testCase "String comparison" $ compareIOResult exp3 res3 @? ""
      , testCase "Eqv check" $ compareIOResult exp4 res4 @? ""
    ]
    where exp1 = Right $ Number 1
          res1 = runEval (List [Atom "if", Bool True, Number 1, Number 2])
          exp2 = Right $ Number 7
          res2 = runEval (List [Atom "+", Number 3, Number 4])
          exp3 = Right $ Bool True
          res3 = runEval (List [Atom "string=?", String "abc", String "abc"])
          exp4 = Right $ Bool True
          res4 = runEval (List [Atom "eqv?", String "abc", String "abc"])

tests :: TestTree
tests = testGroup "" [parserTests, evaluatorTests]

main :: IO ()
main = defaultMain tests
