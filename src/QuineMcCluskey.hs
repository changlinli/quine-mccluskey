{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module QuineMcCluskey
    ( main
    , BooleanFormula(..)
    , BooleanFunction(..)
    , interpretBooleanFormula
    , retrieveVariablesFromBooleanFormula
    , combineImplicants
    , ImplicantValue(..)
    , stringToImplicant
    , deriveNewImplicants
    , calculatePrimeImplicants
    , calculatePrimeImplicantsFormula
    , calculateSumOfProductsFormula
    , calculateSumOfProductsFunction
    , Input(..)
    , functionFromFormulaAssumingAllVariablesUsed
    )
where

import Data.String (IsString(..))
import Numeric.Natural (Natural)
import Data.Function ((&))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (mapMaybe, fromJust)
import GHC.Generics (Generic)
import qualified Debug.Trace as Debug
import Data.Containers.ListUtils (nubOrd)


main :: IO ()
main = do
    print (calculateSumOfProductsFormula customInput)
    print (calculatePrimeImplicants customInput)

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
    deriving (Eq, Generic)

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

-- This is meant to represent a full function, even if certain variables are left unused, e.g.
-- f(a, b, c, d) = ab + d
data BooleanFunction = BooleanFunction
    { variables :: [ String ]
    , body :: BooleanFormula
    }
    deriving (Show)

data Input = Input
    { minTerms :: [[Bool]]
    , dontCare :: [[Bool]]
    }
    deriving (Eq, Show)

numOfBooleanVars :: Input -> Natural
numOfBooleanVars input = minTerms input & head & length & fromIntegral

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

data TermTableOutput
    = DontCareAboutTerm
    | TermBoolOutput Bool
    deriving (Eq, Show, Ord)

data ImplicantValue
    = DontCare
    | BoolValue Bool
    deriving (Eq, Show, Ord)

type Implicant = [ ImplicantValue ]

generateAllBooleanPossibilities :: Natural -> [[ Bool ]]
generateAllBooleanPossibilities 0 = [[]]
generateAllBooleanPossibilities n = generateAllBooleanPossibilities (n - 1) >>= (\possibility -> [ True : possibility, False : possibility ])

calculateTableResult :: Input -> [ Bool ] -> TermTableOutput
calculateTableResult input bools =
    let
        currentMinTerms = minTerms input
        isMinTerm = or . fmap (== bools) $ currentMinTerms
        currentDontCares = dontCare input
        isDontCare = or . fmap (== bools) $ currentDontCares
    in
        if isDontCare then DontCareAboutTerm
        else if isMinTerm then TermBoolOutput True
        else TermBoolOutput False

generateTermTable :: Input -> [([ Bool ], TermTableOutput )]
generateTermTable myInput =
    fmap (\row -> (row, calculateTableResult myInput row)) (generateAllBooleanPossibilities (numOfBooleanVars myInput))

isTrueValue :: TermTableOutput -> Bool
isTrueValue DontCareAboutTerm = False
isTrueValue (TermBoolOutput x) = x

termBoolOutputIsNotFalse :: TermTableOutput -> Bool
termBoolOutputIsNotFalse DontCareAboutTerm = True
termBoolOutputIsNotFalse (TermBoolOutput x) = x

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

implicantToBooleanFunction :: Implicant -> Maybe BooleanFunction
implicantToBooleanFunction implicant = 
    do
        booleanFormula <- booleanFormulaMaybe
        pure $
            BooleanFunction
                { variables = take (fromIntegral finalIdx) variableNames
                , body = booleanFormula
                }
    where
        firstElem = head implicant
        firstElemExpression = case firstElem of
            DontCare -> Nothing
            BoolValue boolVal -> if boolVal then Just "a" else Just . Not $ "a"

        convertBoolToBooleanExpression (idx, currentExpressionMaybe) implicantValue =
            case (implicantValue, currentExpressionMaybe) of
                (BoolValue True, Just currentExpression) ->
                    (idx + 1, Just $ And currentExpression (LogicalVariable $ idxToVariableName idx))

                (BoolValue False, Just currentExpression) ->
                    (idx + 1, Just $ And currentExpression (Not (LogicalVariable $ idxToVariableName idx)))

                (DontCare, Just currentExpression) ->
                    (idx + 1, Just currentExpression )

                (BoolValue True, Nothing) ->
                    (idx + 1, Just (LogicalVariable $ idxToVariableName idx))

                (BoolValue False, Nothing) ->
                    (idx + 1, Just (Not (LogicalVariable $ idxToVariableName idx)))

                (DontCare, Nothing) ->
                    (idx + 1, Nothing)
        
        (finalIdx, booleanFormulaMaybe) = foldl convertBoolToBooleanExpression (1, firstElemExpression) (tail implicant)

-- The minimal boolean formula which is true if and only if passed this set of booleans
boolTermToBooleanFormula :: [ Bool ] -> BooleanFormula
boolTermToBooleanFormula bools = foldl convertBoolToBooleanExpression (1, firstElemExpression) (tail bools) & snd
    where
        firstElem = head bools
        firstElemExpression = if firstElem then "a" else Not "a"

        convertBoolToBooleanExpression (idx, currentExpression) True = (idx + 1, And currentExpression (LogicalVariable $ idxToVariableName idx))
        convertBoolToBooleanExpression (idx, currentExpression) False = (idx + 1, And currentExpression (Not (LogicalVariable $ idxToVariableName idx)))

implicantToBooleanFormula :: Implicant -> BooleanFunction
implicantToBooleanFormula implicant = implicant
    & implicantToBooleanFunction
    & traceWithValue "after extracting bool values"
    & fromJust

-- This doesn't do minimization yet, just finds a correct expression
calculateSumOfProductsFormula :: Input -> BooleanFormula
calculateSumOfProductsFormula myInput = generateTermTable myInput
    & filter (\(_, tableResult) -> isTrueValue tableResult)
    & fmap (boolTermToBooleanFormula . fst)
    & foldr1 Or

-- We can directly extract variable names from the formula because we know by the way that the sum of products is calculated that every variable name must be used
calculateSumOfProductsFunction :: Input -> BooleanFunction
calculateSumOfProductsFunction myInput = 
    let
        formula = calculateSumOfProductsFormula myInput
        variablesForFunction = retrieveVariablesFromBooleanFormula formula
    in
        BooleanFunction
            { variables = variablesForFunction
            , body = formula
            }

data ImplicantComparison
    = IdenticalSoFar Implicant
    | IdenticalExceptForOneBoolVal Implicant
    | MismatchedDontCares
    | MoreThanOneBoolValDifferent

implicantsFoldOnce :: (ImplicantValue, ImplicantValue) -> ImplicantComparison -> ImplicantComparison
implicantsFoldOnce (i0, i1) comparisonResult =
    case comparisonResult of
        IdenticalSoFar previous -> case (i0, i1) of
            (DontCare, DontCare) -> IdenticalSoFar (i0 : previous)
            (DontCare, BoolValue _) -> MismatchedDontCares
            (BoolValue _, DontCare) -> MismatchedDontCares
            (BoolValue b0, BoolValue b1) -> if b0 == b1 then IdenticalSoFar (i0 : previous) else IdenticalExceptForOneBoolVal (DontCare : previous)
        IdenticalExceptForOneBoolVal previous -> case (i0, i1) of
            (DontCare, DontCare) -> IdenticalExceptForOneBoolVal (i0 : previous)
            (DontCare, BoolValue _) -> MismatchedDontCares
            (BoolValue _, DontCare) -> MismatchedDontCares
            (BoolValue b0, BoolValue b1) -> if b0 == b1 then IdenticalExceptForOneBoolVal (i0 : previous) else MoreThanOneBoolValDifferent
        MismatchedDontCares -> MismatchedDontCares
        MoreThanOneBoolValDifferent -> MoreThanOneBoolValDifferent

-- Must assume that two implicants are of same length
-- Only combine implicants if they differ by exactly one BoolValue
-- Replace with DontCare in that case
-- Otherwise return Nothing
combineImplicants :: Implicant -> Implicant -> Maybe Implicant
combineImplicants implicant0 implicant1 =
    case foldr implicantsFoldOnce (IdenticalSoFar []) (zip implicant0 implicant1) of
        IdenticalSoFar _ -> Nothing
        IdenticalExceptForOneBoolVal result -> Just result
        MismatchedDontCares -> Nothing
        MoreThanOneBoolValDifferent -> Nothing

-- A nice way of writing them out with ones and zeroes and dashes
stringToImplicant :: String -> Implicant
stringToImplicant = mapMaybe 
    (\c -> 
        if c == '1' 
            then Just $ BoolValue True 
            else if c == '0' then Just $ BoolValue False 
            else if c == '-' then Just DontCare 
            else Nothing
    )

data ImplicantUsage = ImplicantUsage
    { implicantsUsed :: Set.Set Implicant
    , allStartingImplicants :: [ Implicant ]
    , newImplicants :: Set.Set Implicant
    }
    deriving (Show)

combineImplicantWithCurrentImplicants :: ImplicantUsage -> Implicant -> ImplicantUsage
combineImplicantWithCurrentImplicants implicantUsage newImplicant =
    let
        currentImplicantsUsed = implicantsUsed implicantUsage
        currentAllStartingImplicants = allStartingImplicants implicantUsage
        currentNewImplicants = newImplicants implicantUsage
        moreNewImplicants =
            mapMaybe (combineImplicants newImplicant) currentAllStartingImplicants
    in
        if not (null moreNewImplicants)
            then ImplicantUsage
                { implicantsUsed = Set.insert newImplicant currentImplicantsUsed
                , allStartingImplicants = currentAllStartingImplicants
                , newImplicants = foldr Set.insert currentNewImplicants moreNewImplicants
                }
            else implicantUsage

initializeImplicantUsage :: [ Implicant ] -> ImplicantUsage
initializeImplicantUsage implicants = ImplicantUsage
    { implicantsUsed = Set.empty
    , allStartingImplicants = implicants
    , newImplicants = Set.empty
    }

deriveNewImplicants :: [ Implicant ] -> [ Implicant ]
deriveNewImplicants implicants =
    let
        finalImplicantUsage = foldl combineImplicantWithCurrentImplicants (initializeImplicantUsage implicants) implicants
        finalImplicantsUsed = implicantsUsed finalImplicantUsage
        implicantsNotUsed = filter (not . (`Set.member` finalImplicantsUsed)) implicants
        newImplicantsAsList = Set.toList . newImplicants $ finalImplicantUsage
    in
        implicantsNotUsed ++ newImplicantsAsList

derivePrimeImplicantsFromImplicants :: [ Implicant ] -> [ Implicant ]
derivePrimeImplicantsFromImplicants implicants =
    let
        result = deriveNewImplicants implicants
    in
        if result == implicants
            then result
            else derivePrimeImplicantsFromImplicants result

calculatePrimeImplicants :: Input -> [ Implicant ]
calculatePrimeImplicants myInput = generateTermTable myInput
    & filter (\(_, value) -> termBoolOutputIsNotFalse value)
    & fmap (fmap BoolValue . fst)
    & derivePrimeImplicantsFromImplicants

-- FIXME: This is partial
calculatePrimeImplicantsFormula :: Input -> BooleanFunction
calculatePrimeImplicantsFormula myInput = 
    let
        perImplicantBooleanFunctions = calculatePrimeImplicants myInput
            & fmap implicantToBooleanFormula
        variablesForFunction = variables . head $ perImplicantBooleanFunctions
        perImplicantBooleanFormulas = fmap body perImplicantBooleanFunctions
        combinedFormula = foldr1 Or perImplicantBooleanFormulas
    in
        BooleanFunction
            { variables = variablesForFunction
            , body = combinedFormula
            }

retrieveVariablesFromBooleanFormula :: BooleanFormula -> [ String ]
retrieveVariablesFromBooleanFormula (And x0 x1) = retrieveVariablesFromBooleanFormula x0 ++ retrieveVariablesFromBooleanFormula x1
retrieveVariablesFromBooleanFormula (Or x0 x1) = retrieveVariablesFromBooleanFormula x0 ++ retrieveVariablesFromBooleanFormula x1
retrieveVariablesFromBooleanFormula (Not x) = retrieveVariablesFromBooleanFormula x
retrieveVariablesFromBooleanFormula (LogicalVariable var) = [ var ]
retrieveVariablesFromBooleanFormula ConstantTrue = []
retrieveVariablesFromBooleanFormula ConstantFalse = []

traceWithValue :: Show a => String -> a -> a
traceWithValue str a = Debug.trace (str ++ ": " ++ show a) a
-- traceWithValue str a = const a str

-- If the number of bools given is incorrect, we give back Nothing
interpretBooleanFormula :: BooleanFunction -> [ Bool ] -> Maybe Bool
interpretBooleanFormula function bools =
    interpretBooleanFormulaMapVariableNames (body function) variablesToBools
    where
        variablesToBools = zip (nubOrd . variables $ function) bools
            & Map.fromList

interpretBooleanFormulaMapVariableNames :: BooleanFormula -> Map.Map String Bool -> Maybe Bool
interpretBooleanFormulaMapVariableNames formula variablesToBools =
    case formula of
        And x0 x1 ->
            do
                y0 <- interpretBooleanFormulaMapVariableNames x0 variablesToBools
                y1 <- interpretBooleanFormulaMapVariableNames x1 variablesToBools
                pure $ traceWithValue "(y0 && y1)" (y0 && y1)
        Or x0 x1 ->
            do
                y0 <- interpretBooleanFormulaMapVariableNames x0 variablesToBools
                y1 <- interpretBooleanFormulaMapVariableNames x1 variablesToBools
                pure $ traceWithValue "(y0 || y1)" (y0 || y1)
        Not x -> traceWithValue "not" `fmap` not <$> interpretBooleanFormulaMapVariableNames x variablesToBools
        LogicalVariable var -> traceWithValue ("var" ++ show var) `fmap` Map.lookup var variablesToBools
        ConstantTrue -> Just True
        ConstantFalse -> Just False

-- This function assumes that all variables that the function takes in are present in the formula
-- So e.g. for the formula ab + de we'll have a function that takes in four arguments: a, b, d, and e.
-- Note however that "c" will be missing!
functionFromFormulaAssumingAllVariablesUsed :: BooleanFormula -> BooleanFunction
functionFromFormulaAssumingAllVariablesUsed formula =
    BooleanFunction
        { variables = retrieveVariablesFromBooleanFormula formula
        , body = formula
        }