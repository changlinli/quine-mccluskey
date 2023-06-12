{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where
import Data.String (IsString(..))
import Numeric.Natural (Natural)

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

class Mappable f where
    myMap :: (a -> b) -> f a -> f b

-- This means to implement MultipleMappabe you must implement both myMap and myMap2
class Mappable g => Mappable2 g where
    myMap2 :: (a -> b -> c) -> g a -> g b -> g c

instance Mappable Maybe where
    myMap f (Just x) = Just (f x)
    myMap _ Nothing = Nothing

instance Mappable2 Maybe where
    myMap2 f (Just x) (Just y) = Just (f x y)
    myMap2 _ Nothing _ = Nothing
    myMap2 _ _ Nothing = Nothing

class Wrappable f where
    wrap :: a -> f a

instance Wrappable Maybe where
    wrap = Just

class (Wrappable f, Mappable2 f) => WrapAndMappable2 f where

class AltWrapAndMappable2 f where
    wrap' :: a -> f a
    myMap' :: (a -> b) -> f a -> f b
    moveOutFromTuple :: (f a, f b) -> f (a, b)

myMap2' :: AltWrapAndMappable2 f => (a -> b -> c) -> f a -> f b -> f c
myMap2' = undefined

data Input = Input
    { minTerms :: [ Natural ]
    , dontCare :: [ Natural ]
    , totalNumOfBooleanVars :: Natural
    }

    -- I want 0 as my only minterm
    -- I don't care about 1 and 2
    -- Is this 4 boolean values with the fourth always set to 1? Is this 8 boolean values with 3, 4, 5, 6, 7 all set to 1?

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

data TableResult
    = DontCare
    | BoolValue Bool

generateTable :: Input -> [([ Bool ], TableResult )]
generateTable myInput = undefined


goalFunction :: Input -> BooleanExpression
goalFunction = undefined
