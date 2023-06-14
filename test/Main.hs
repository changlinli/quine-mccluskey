{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.List

import qualified QuineMcCluskey as QM
import Data.Maybe (fromMaybe, isJust)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ properties
  , unitTests
  ]

properties :: TestTree
properties = testGroup "Properties"
  [ scProps
  , qcProps
  ]

scProps :: TestTree
scProps = testGroup "(checked by SmallCheck)"
  -- [ SC.testProperty "all BooleanFormula values are valid boolean functions" $
  --     \booleanFormula -> 
  --       let
  --         result =
  --           QM.interpretBooleanFormula
  --             booleanFormula
  --             (replicate (length . QM.retrieveVariablesFromBooleanFormula $ booleanFormula) True)
  --       in
  --         isJust result
  [ SC.testProperty "Fermat's little theorem" $
      \x -> ((x :: Integer)^(7::Integer) - x) `mod` 7 == 0
  -- the following property does not hold
  -- , SC.testProperty "Fermat's last theorem" $
  --     \x y z n ->
  --       (n :: Integer) >= 3 SC.==> x^n + y^n /= (z^n :: Integer)
  ]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "sort == sort . reverse" $
      \list -> sort (list :: [Int]) == sort (reverse list)
  , QC.testProperty "Fermat's little theorem" $
      \x -> ((x :: Integer)^(7::Integer) - x) `mod` 7 == 0
  -- the following property does not hold
  -- , QC.testProperty "Fermat's last theorem" $
  --     \x y z n ->
  --       (n :: Integer) >= 3 QC.==> x^n + y^n /= (z^n :: Integer)
  ]

myFormula0 :: QM.BooleanFormula
myFormula0 = "x" * "y" + "y" * "z" + (-"w")

myFormula1 :: QM.BooleanFormula
myFormula1 = "x" * "y" + "y" * "z"

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "Example Boolean formula is interpreted correctly" $
      QM.interpretBooleanFormula myFormula0 [True, True, True, True] @?= Just True

  , testCase "Another example Boolean formula is interpreted correctly" $
      QM.interpretBooleanFormula myFormula1 [False, False, False] @?= Just False
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
  ]
