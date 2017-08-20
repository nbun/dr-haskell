-- | Check if a lambda function is used
module StaticAnalysis.StaticChecks.TypeInstance (
    typeinstanceCheck
) where

import AstChecks.Check
import Language.Haskell.Exts
import StaticAnalysis.Messages.StaticErrors

-- | Checks if a lambda function is used
typeinstanceCheck :: DeclCheck l (Error l)
typeinstanceCheck (InstanceDecl info _ _ _) = [ClassInstance info]
typeinstanceCheck (ClassDecl info _ _ _ _)  = [Class info]
typeinstanceCheck _                         = []
