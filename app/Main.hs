{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where
import Data.String (IsString(..))
import Numeric.Natural (Natural)
import Data.Function ((&))

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
