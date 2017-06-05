module StaticAnalysis.StaticChecks.TypeVars (
    checkForTypVar
) where

import           AstChecks.Check
import           Language.Haskell.Exts
import           StaticAnalysis.Messages.StaticErrors

checkForTypVar :: TypeCheck l (Error l)
checkForTypVar (TyFun _ x xs) =
    case x of
        (TyVar _ name) -> [TypeVar name]
        _              -> []

