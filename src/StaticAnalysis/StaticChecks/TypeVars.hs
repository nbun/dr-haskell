-- | Check if type variables are used
module StaticAnalysis.StaticChecks.TypeVars (checkForTypVar) where

import AstChecks.Check                      (TypeCheck, mapOverTyTypesRec)
import Language.Haskell.Exts                (Type (..))
import StaticAnalysis.Messages.StaticErrors (Error (..))

-- | Checks if type variables are used
checkForTypVar :: TypeCheck l (Error l)
checkForTypVar t =
    case t of
        (TyVar _ n) -> [TypeVar n]
        _           -> mapOverTyTypesRec False checkForTypVar [t]
