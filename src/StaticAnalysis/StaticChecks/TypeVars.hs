module StaticAnalysis.StaticChecks.TypeVars (
    checkForTypVar
) where

import           AstChecks.Check
import           Language.Haskell.Exts

checkForTypVar :: TypeCheck l (Response l)
checkForTypVar (TyFun _ x xs) =
    case x of
        (TyVar _ (Ident info name)) -> [Just ("Usage of Typevar " ++ name, info)]
        _                           -> [Nothing]

