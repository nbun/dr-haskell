module TypeVars (
    checkForTypVar
) where

import           Check
import           Control.Monad
import           Language.Haskell.Exts

checkForTypVar :: TypeCheck l
checkForTypVar (TyFun _ x xs) =
    case x of
        (TyVar _ (Ident info name)) -> Just ("Usage of Typevar " ++ name, info)
        _                           -> Nothing
