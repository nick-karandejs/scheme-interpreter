module Evaluator (
    eval
) where

import Parser

eval :: LispVal -> LispVal
-- using `@` we capture the passed value (LispVal) rather than String value
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom fun : args)) = apply fun $ map eval args

numericMonoid :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericMonoid op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
-- TODO: add weak typing? i.e. to accept string as number as well

-- TODO: add more primitives
primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericMonoid (+)),
              ("-", numericMonoid (-))]

apply :: String -> [LispVal] -> LispVal
-- `maybe` allows to specify what to do in case of success and failure
-- here in case `lookup` fails we return False, otherwise the lookup returns
-- a function that we want to apply to args, hence `$ args`
apply fun args = maybe (Bool False) ($ args) $ lookup fun primitives

