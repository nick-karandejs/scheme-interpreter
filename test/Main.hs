module Main where

import Data.List
import Control.Monad.Except

import Test.Tasty
import Test.Tasty.HUnit

import Text.ParserCombinators.Parsec hiding (spaces)
import ParserInternal as P
import Common


readString s = P.readOrThrow P.parseString s

unitTests :: TestTree
unitTests =
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

tests :: TestTree
tests = testGroup "" [unitTests]

main :: IO ()
main = defaultMain tests
