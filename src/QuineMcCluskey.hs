{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module QuineMcCluskey (main) where

import Data.String (IsString(..))
import Numeric.Natural (Natural)
import Data.Function ((&))

main :: IO ()
main = do
    print (goalFunction customInput)

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

data BooleanFormula
    = And BooleanFormula BooleanFormula
    | Or BooleanFormula BooleanFormula
    | Not BooleanFormula
    | LogicalVariable String
    | ConstantTrue
    | ConstantFalse
    deriving (Eq)

instance Show BooleanFormula where
    show (And x0 x1) = show x0 ++ show x1
    show (Or x0 x1) = show x0 ++ " + " ++ show x1
    show (Not (LogicalVariable str)) = show (LogicalVariable str) ++ "'"
    show (Not x0) = "(" ++ show x0 ++ ")'"
    show (LogicalVariable str) = str
    show ConstantTrue = "TRUE"
    show ConstantFalse = "FALSE"

instance IsString BooleanFormula where
    fromString :: String -> BooleanFormula
    fromString = LogicalVariable

myFormula :: BooleanFormula
myFormula = "x" * "y" + "y" * "z" + (-"w")

data Input = Input
    { minTerms :: [[Bool]]
    , dontCare :: [[Bool]]
    }

numOfBooleanVars :: Input -> Natural
numOfBooleanVars input = (minTerms input) & head & length & fromIntegral

    -- I want 0 as my only minterm
    -- I don't care about 1 and 2
    -- Is this 4 boolean values with the fourth always set to 1? Is this 8 boolean values with 3, 4, 5, 6, 7 all set to 1?

instance Num BooleanFormula where
    (*) :: BooleanFormula -> BooleanFormula -> BooleanFormula
    x * y = And x y

    (+) :: BooleanFormula -> BooleanFormula -> BooleanFormula
    x + y = Or x y

    negate :: BooleanFormula -> BooleanFormula
    negate = Not

    abs :: BooleanFormula -> BooleanFormula
    abs = id

    signum :: BooleanFormula -> BooleanFormula
    signum = id

    fromInteger :: Integer -> BooleanFormula
    fromInteger x
        | x <= 0 = ConstantFalse
        | x == 1 = ConstantTrue
        | x > 2 = fromInteger (x - 2)
        -- Various values of infinity
        | otherwise = ConstantFalse

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
        isMinTerm = or . fmap (== bools) $ currentMinTerms
        currentDontCares = dontCare input
        isDontCare = or . fmap (== bools) $ currentDontCares
    in
        if isDontCare then DontCare
        else if isMinTerm then BoolValue True
        else BoolValue False

generateTable :: Input -> [([ Bool ], TableResult )]
generateTable myInput =
    fmap (\row -> (row, calculateTableResult myInput row)) (generateAllBooleanPossibilities (numOfBooleanVars myInput))

shouldIncludeTableResult :: TableResult -> Bool
shouldIncludeTableResult DontCare = False
shouldIncludeTableResult (BoolValue x) = x

-- A fun little application of Haskell's laziness and infinite lists
variableNames :: [ String ]
variableNames = ['a'..'z']
    & cycle
    & zip [(0 :: Integer)..]
    & fmap (\(idx, c) -> if idx < 26 then [ c ] else c : show (idx `div` 26))

toInt :: (Integral a) => a -> Int
toInt x = fromIntegral (toInteger x)

idxToVariableName :: Natural -> String
idxToVariableName idx = variableNames !! toInt idx

singleMinTermToBooleanExpression :: [ Bool ] -> BooleanFormula
singleMinTermToBooleanExpression bools = foldr convertBoolToBooleanExpression (1, firstElemExpression) (tail bools) & snd
    where
        firstElem = head bools
        firstElemExpression = if firstElem then "a" else Not "a"

        convertBoolToBooleanExpression True (idx, currentExpression) = (idx + 1, And currentExpression (LogicalVariable $ idxToVariableName idx))
        convertBoolToBooleanExpression False (idx, currentExpression) = (idx + 1, And currentExpression (Not (LogicalVariable $ idxToVariableName idx)))

-- This doesn't do minimization yet, just finds a correct expression
goalFunction :: Input -> BooleanFormula
goalFunction myInput = generateTable myInput
    & filter (\(_, tableResult) -> shouldIncludeTableResult tableResult)
    & fmap (singleMinTermToBooleanExpression . fst)
    & foldr1 Or
