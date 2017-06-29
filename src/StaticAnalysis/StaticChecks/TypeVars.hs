-- | Check if type variables are used
module StaticAnalysis.StaticChecks.TypeVars (checkForTypVar) where

import           AstChecks.Check
import           Language.Haskell.Exts
import           StaticAnalysis.Messages.StaticErrors

-- | Checks if type variables are used
checkForTypVar :: TypeCheck l (Error l)
checkForTypVar (TyFun _ x _) =
    case x of
        (TyVar _ n) -> [TypeVar n]
        _           -> []
checkForTypVar _ = []

