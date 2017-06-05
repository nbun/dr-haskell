module StaticAnalysis.StaticChecks.Lambda (
    lambdaCheck
) where

import           AstChecks.Check
import           Language.Haskell.Exts
import           StaticAnalysis.Messages.StaticErrors

lambdaCheck :: ExpCheck l (Error l)
lambdaCheck (Lambda info _ _) = [LambdaFunction info]
lambdaCheck _                 = []

