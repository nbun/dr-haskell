module AstChecks.HigherOrder (
    checkForHigherOrderFunction
) where

import           Check
import           Language.Haskell.Exts

checkForHigherOrderFunction :: TypeCheck l
checkForHigherOrderFunction (TyFun _ x _) = spotTyParen x
checkForHigherOrderFunction _             = Nothing

spotTyParen (TyParen info xs) =
    case xs of
        TyFun{} -> Just ("Higher-Order-Function", info)
        _       -> Nothing
spotTyParen _ = Nothing
