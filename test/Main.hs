{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.Hedgehog as HH
import Test.Tasty.HUnit

import Data.List

import qualified QuineMcCluskey as QM
import Data.Function ((&))
import Control.Monad (forM_)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ properties
  , unitTests
  ]

properties :: TestTree
properties = testGroup "Properties"
  [ hedgehogProperties
  ]

-- FIXME: This is pretty wasteful because we have duplicate bools
inputGenerator :: Hedgehog.MonadGen m => m (QM.Input, Int)
inputGenerator = do
  numOfBools <- Gen.int (Range.linear 1 20)
  numOfRowsOfBools <- Gen.int (Range.linear 1 numOfBools)
  bools <- Gen.list (Range.singleton numOfRowsOfBools) (Gen.list (Range.singleton numOfBools) Gen.bool)
  let uniqueBools = nub bools
  numOfMinTerms <- Gen.int (Range.linear 1 (length uniqueBools))
  let minTerms = take numOfMinTerms uniqueBools
  let dontCare = drop numOfMinTerms uniqueBools
  let input = QM.Input { QM.minTerms = minTerms, QM.dontCare = dontCare }
  pure (input, numOfBools)

hedgehogProperties :: TestTree
hedgehogProperties = testGroup "(checked by Hedgehog)"
  [ HH.testProperty "boolean formula after prime implicants equivalent to sum of products on min terms" $
      booleanFormulaAfterPrimeImplicantsAgreesWithSumOfProductOnMinTerms
  ]

booleanFormulaAfterPrimeImplicantsAgreesWithSumOfProductOnMinTerms :: Hedgehog.Property
booleanFormulaAfterPrimeImplicantsAgreesWithSumOfProductOnMinTerms =
  Hedgehog.property $ do
    inputAndNumOfBools <- Hedgehog.forAll inputGenerator
    let (input, _) = inputAndNumOfBools
    let primeImplicantsFormula = QM.calculatePrimeImplicantsFormula input & QM.interpretBooleanFormula
    let productOfSumsFormula = QM.calculateSumOfProductsFunction input & QM.interpretBooleanFormula
    minTerm <- Hedgehog.forAll (Gen.element $ QM.minTerms input)
    primeImplicantsFormula minTerm Hedgehog./== Nothing
    primeImplicantsFormula minTerm Hedgehog.=== productOfSumsFormula minTerm

myFormula0 :: QM.BooleanFormula
myFormula0 = "x" * "y" + "y" * "z" + (-"w")

myFormula1 :: QM.BooleanFormula
myFormula1 = "x" * "y" + "y" * "z"

calculatePrimeImplicantsFormulaCase0Input :: QM.Input
calculatePrimeImplicantsFormulaCase0Input = 
  QM.Input
    { QM.minTerms =
            [ [False, True, False, False]
            , [True, False, False, False]
            , [True, False, True, False]
            , [True, False, True, True]
            , [True, True, False, False]
            , [True, True, True, True]
            ]
    , QM.dontCare =
            [ [True, False, False, True ]
            , [True, True, True, False ]
            ]
    }

knownInputCase1 :: QM.Input
knownInputCase1 = 
  QM.Input
    { QM.minTerms = [ [ True , False , False , False ] ]
    , QM.dontCare = [ [ True , True , True , True ] ]
    }


unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "Example Boolean formula is interpreted correctly" $
      QM.interpretBooleanFormula (QM.functionFromFormulaAssumingAllVariablesUsed myFormula0) [True, True, True, True] @?= Just True
  , testCase "Another example Boolean formula is interpreted correctly" $
      QM.interpretBooleanFormula (QM.functionFromFormulaAssumingAllVariablesUsed myFormula1) [False, False, False] @?= Just False
  , testCase "retrieveVariablesFromBooleanFormula retrieves variables from a known case correctly" $
      QM.retrieveVariablesFromBooleanFormula ("a" * (-"b") * (-"c") * (-"d")) @?= [ "a", "b", "c", "d" ]
  , testCase "combineImplicants works on known inputs case 0" $
      QM.combineImplicants (QM.stringToImplicant "0100") (QM.stringToImplicant "1100") @?= Just (QM.stringToImplicant "-100")
  , testCase "combineImplicants works on known inputs case 1" $
      QM.combineImplicants (QM.stringToImplicant "1000") (QM.stringToImplicant "1001") @?= Just (QM.stringToImplicant "100-")
  , testCase "combineImplicants works on known inputs case 2" $
      QM.combineImplicants (QM.stringToImplicant "1000") (QM.stringToImplicant "1011") @?= Nothing
  , testCase "combineImplicants works on known inputs case 3" $
      QM.combineImplicants (QM.stringToImplicant "100-") (QM.stringToImplicant "-000") @?= Nothing
  , testCase "deriveNewImplicants works on known inputs case 0" $
      QM.deriveNewImplicants 
        [ QM.stringToImplicant "0100" 
        , QM.stringToImplicant "1100" 
        ]
        @?= [ QM.stringToImplicant "-100" ]
  , testCase "calculatePrimeImplicantsFormula works on known input case 0" $
    QM.calculatePrimeImplicants calculatePrimeImplicantsFormulaCase0Input 
      @?= 
        [ QM.stringToImplicant "-100"
        , QM.stringToImplicant "1--0"
        , QM.stringToImplicant "1-1-"
        , QM.stringToImplicant "10--"
        ]
  , testCase "calculatePrimeImplicantsFormula works on known input case 1" $
    QM.calculateSumOfProductsFormula knownInputCase1
      @?= "a" * (-"b") * (-"c") * (-"d")
  , testCase "calculatePrimeImplicantsFormula works on known input case 2" $
    QM.interpretBooleanFormula (QM.calculateSumOfProductsFunction knownInputCase1) [ True, False, False, False ]
      @?= Just True
  ]
