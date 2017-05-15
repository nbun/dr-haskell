module AstChecks.Lambda (
    lambdaCheck
) where

import           Check
import           Language.Haskell.Exts

lambdaCheck :: ExpCheck l
lambdaCheck (Lambda info _ _) = Just ("Lambda-Function", info)
lambdaCheck _                 = Nothing
