module StaticAnalysis.StaticChecks.Lambda (
    lambdaCheck
) where

import           AstChecks.Check
import           Language.Haskell.Exts

lambdaCheck :: ExpCheck l (Response l)
lambdaCheck (Lambda info _ _) = [Just ("Lambda-Function", info)]
lambdaCheck _                 = [Nothing]

