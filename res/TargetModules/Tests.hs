module Tests (checkExpect, quickCheck, TestResult (..)) where

import qualified Test.QuickCheck as QC

data TestResult = Failure String | Success

checkExpect :: (Eq a, Show a) => a -> a -> IO TestResult
checkExpect a b = if a == b
                  then return $ Success
                  else return $ Failure ("Expected Result " ++ show a ++ " does not match actual result " ++ show b)

quickCheck :: QC.Testable prop => prop -> IO TestResult
quickCheck p = convertQC <$> (QC.quickCheckWithResult QC.stdArgs {QC.chatty=False} p)

convertQC :: QC.Result -> TestResult
convertQC (QC.Success _ _ _) = Success
convertQC a = Failure $ QC.output a