-- | Check if type variables are used
module StaticAnalysis.StaticChecks.TypeVars (checkForTypVar) where

import AstChecks.Check                      (TypeCheck)
import Language.Haskell.Exts                (Type (..))
import StaticAnalysis.Messages.StaticErrors (Error (..))

-- | Checks if type variables are used
checkForTypVar :: TypeCheck l (Error l)
checkForTypVar (TyFun _ x _) =
    case x of
        (TyVar _ n) -> [TypeVar n]
        _           -> []
checkForTypVar _ = []
