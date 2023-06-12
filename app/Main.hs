{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where
import Data.String (IsString(..))
import Numeric.Natural (Natural)
import Data.Function ((&))
import Debug.Trace (traceShow)

main :: IO ()
main = do
    print (calculateTableResult customInput [False,True,False,False])
    print (goalFunction customInput 4)

customInput :: Input
customInput = Input
    { minTerms = 
            [ [False, True, False, False]
            , [True, False, False, False]
            , [True, False, True, False]
            , [True, False, True, True]
            , [True, True, False, False]
            , [True, True, True, True]
            ]
    , dontCare =
            [ [True, False, False, True ]
            , [True, True, True, False ]
            ]
    }

data BooleanExpression
    = And BooleanExpression BooleanExpression
    | Or BooleanExpression BooleanExpression
    | Not BooleanExpression
    | LogicalVariable String
    deriving (Eq)

instance Show BooleanExpression where
    show (And x0 x1) = show x0 ++ " * " ++ show x1
    show (Or x0 x1) = show x0 ++ " + " ++ show x1
    show (Not x0) = "-(" ++ show x0 ++ ")"
    show (LogicalVariable str) = "\"" ++ str ++ "\""

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

myFunction' :: Int -> Int
myFunction' x = x + 1

usingMyMap = myMap myFunction' (Just 0) -- Just 1

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

instance AltWrapAndMappable2 Maybe where
    wrap' = Just
    myMap' = fmap
    moveOutFromTuple (Just x, Just y) = Just (x, y)
    moveOutFromTuple (Nothing, _) = Nothing
    moveOutFromTuple (_, Nothing) = Nothing

myMap2' :: AltWrapAndMappable2 f => (a -> b -> c) -> f a -> f b -> f c
myMap2' = undefined

myMap3' :: AltWrapAndMappable2 f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
myMap3' f x y z =
    let
        something = myMap2' f x y
        -- applyAFunction not True <=> not True
        applyAFunction g a = g a
    in
        myMap2' applyAFunction something z

class (AltWrapAndMappable2 f) => MyFlattable f where
    flatten :: f (f a) -> f a
    -- unwrap :: f a -> a

class MyMonad f where
    wrap'' :: a -> f a
    map'' :: (a -> b) -> f a -> f b
    flatten'' :: f (f a) -> f a

flatMap :: MyMonad m => (a -> m b) -> m a -> m b
flatMap f a = flatten'' (map'' f a)

myMap2'' :: MyMonad m => forall a b c. (a -> b -> c) -> m a -> m b -> m c
myMap2'' f x y =
    let
        -- intermediateF :: m (b -> c)
        intermediateF = map'' f x
        -- mapFAgain :: m (m c)
        mapFAgain = map'' (`map''` y) intermediateF
    in
        flatten'' mapFAgain


ifElse :: Bool -> a -> a -> a
ifElse condition thenAction elseAction = if condition then thenAction else elseAction

ifElseMaybe0 :: Maybe Bool -> Maybe a -> Maybe a -> Maybe a
ifElseMaybe0 Nothing _ _ = Nothing
ifElseMaybe0 (Just _) Nothing _ = Nothing
ifElseMaybe0 (Just _) _ Nothing = Nothing
ifElseMaybe0 (Just condition) (Just thenAction) (Just elseAction) = Just (ifElse condition thenAction elseAction)

ifElseMaybe0' :: Maybe Bool -> Maybe a -> Maybe a -> Maybe a
ifElseMaybe0' = myMap3' ifElse

ifElseMaybe1 :: Maybe Bool -> Maybe a -> Maybe a -> Maybe a
ifElseMaybe1 Nothing _ _ = Nothing
ifElseMaybe1 (Just True) Nothing _ = Nothing
ifElseMaybe1 (Just False) _ (Just elseValue) = Just elseValue
ifElseMaybe1 (Just False) _ Nothing = Nothing
ifElseMaybe1 (Just x) (Just thenValue) (Just elseValue) = Just (ifElse x thenValue elseValue)


-- Defining intermediate values
myArithmeticFunction :: Int -> Int
myArithmeticFunction x =
    let
        added2 = x + 2
    in
        added2 * 3 -- Exact same as (x + 2) * 3

myArithmeticFunction' :: Int -> Int
myArithmeticFunction' x =
    added2 * 3 -- Exact same as (x + 2) * 3
        where
            added2 = x + 2

myAddition :: Int -> Int -> Int
myAddition x y = x + y

usingMyMap2' :: Maybe Int
usingMyMap2' = myMap2' myAddition (Just 1) (Just 2) -- Just 3

data Input = Input
    { minTerms :: [[Bool]]
    , dontCare :: [[Bool]]
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
    deriving (Eq, Show)

generateAllBooleanPossibilities :: Natural -> [[ Bool ]]
generateAllBooleanPossibilities 0 = [[]]
generateAllBooleanPossibilities n = generateAllBooleanPossibilities (n - 1) >>= (\possibility -> [ True : possibility, False : possibility ])

calculateTableResult :: Input -> [ Bool ] -> TableResult
calculateTableResult input bools =
    let
        currentMinTerms = minTerms input
        isMinTerm = (or . fmap (\x -> (traceSelf x) == (bools & traceSelf)) $ currentMinTerms) & traceSelf
        currentDontCares = dontCare input
        isDontCare = or . fmap (== bools) $ currentDontCares
    in
        if isDontCare then DontCare
        else if isMinTerm then BoolValue True
        else BoolValue False

generateTable :: Input -> Natural -> [([ Bool ], TableResult )]
generateTable myInput totalNumOfBooleanVars =
    fmap (\row -> (row, calculateTableResult myInput row)) (generateAllBooleanPossibilities totalNumOfBooleanVars)

shouldIncludeTableResult :: TableResult -> Bool
shouldIncludeTableResult DontCare = False
shouldIncludeTableResult (BoolValue x) = x

-- traceSelf a = traceShow a a
traceSelf a = a

idxToVariableName :: Natural -> String
idxToVariableName idx = ['a'..'z']
    & fmap (\c -> [c])
    & drop (fromInteger. toInteger $ idx)
    & head

singleMinTermToBooleanExpression :: [ Bool ] -> BooleanExpression
singleMinTermToBooleanExpression bools = foldr convertBoolToBooleanExpression (1, firstElemExpression) (tail bools) & snd
    where
        firstElem = head bools
        firstElemExpression = if firstElem then "a" else Not "a"

        convertBoolToBooleanExpression True (idx, currentExpression) = (idx + 1, And (LogicalVariable $ idxToVariableName idx) currentExpression)
        convertBoolToBooleanExpression False (idx, currentExpression) = (idx + 1, And (Not (LogicalVariable $ idxToVariableName idx)) currentExpression)

goalFunction :: Input -> Natural -> BooleanExpression
goalFunction myInput totalNumOfBooleanVars = generateTable myInput totalNumOfBooleanVars
    & filter (\(_, tableResult) -> shouldIncludeTableResult tableResult)
    & traceSelf
    & fmap (singleMinTermToBooleanExpression . fst)
    & foldr1 Or
