-- | Check if a lambda function is used
module StaticAnalysis.StaticChecks.Lambda (lambdaCheck) where

import AstChecks.Check                      (ExpCheck)
import Language.Haskell.Exts                (Exp (..))
import StaticAnalysis.Messages.StaticErrors (Error (..))

-- | Checks if a lambda function is used
lambdaCheck :: ExpCheck l (Error l)
lambdaCheck (Lambda info _ _) = [LambdaFunction info]
lambdaCheck _                 = []
