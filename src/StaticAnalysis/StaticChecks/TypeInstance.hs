-- | Check if a lambda function is used
module StaticAnalysis.StaticChecks.TypeInstance (
    typeinstanceCheck
) where

import AstChecks.Check
import Language.Haskell.Exts
import StaticAnalysis.Messages.StaticErrors

-- | Checks if a lambda function is used
typeinstanceCheck :: DeclCheck l (Error l)
typeinstanceCheck (InstDecl  info _ _ _)    = [InstDeclUsed info]
typeinstanceCheck (ClassDecl info _ _ _ _)  = [ClassDeclUsed info]
typeinstanceCheck _                         = []