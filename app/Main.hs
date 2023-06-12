{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where
import Data.String (IsString(..))

main :: IO ()
main = myFunction


myFunction :: IO ()
myFunction = putStrLn "Hello, Haskell!"

data BooleanExpression
    = And BooleanExpression BooleanExpression
    | Or BooleanExpression BooleanExpression
    | Not BooleanExpression
    | LogicalVariable String
    deriving (Eq, Show)

l :: String -> BooleanExpression
l = LogicalVariable

instance IsString BooleanExpression where
    fromString :: String -> BooleanExpression
    fromString = LogicalVariable

myExpression :: BooleanExpression
myExpression = "x" * "y" + "y" * "z" + (-"w")

instance Num BooleanExpression where
    (*) :: BooleanExpression -> BooleanExpression -> BooleanExpression
    x * y = And x y

    (+) :: BooleanExpression -> BooleanExpression -> BooleanExpression
    x + y = Or x y

    negate :: BooleanExpression -> BooleanExpression
    negate = Not

    abs :: BooleanExpression -> BooleanExpression
    abs = id

    signum :: BooleanExpression -> BooleanExpression
    signum = id

    fromInteger :: Integer -> BooleanExpression
    fromInteger = undefined


goalFunction :: BooleanExpression -> BooleanExpression
goalFunction = undefined
