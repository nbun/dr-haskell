module Tests (
  checkExpect, quickCheck, TestResult (..),
  Arb.Arbitrary, Arb.arbitrary,
  Gen.oneof
  ) where

import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Arbitrary as Arb
import qualified Test.QuickCheck.Gen as Gen

data TestResult = Failure Int String String | Success Int

checkExpect :: (Eq a, Show a) => Int -> String -> a -> a -> IO TestResult
checkExpect l texp a b = if a == b
  then return $ Success l
  else return $ Failure l texp ("Expected Result\n  "
                                ++ show a
                                ++ "\ndoes not match actual result\n  "
                                ++ show b)

quickCheck :: QC.Testable prop => Int -> String -> prop -> IO TestResult
quickCheck l pexp p = convertQC l pexp
                        <$> QC.quickCheckWithResult
                              QC.stdArgs {QC.chatty=False} p

convertQC :: Int -> String -> QC.Result -> TestResult
convertQC l pexp QC.Success {} = Success l
convertQC l pexp a             = Failure l pexp $ QC.output a
