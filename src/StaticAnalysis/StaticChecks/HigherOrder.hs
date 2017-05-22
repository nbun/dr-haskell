module StaticAnalysis.StaticChecks.HigherOrder (
    checkForHigherOrderFunction
) where

import           AstChecks.Check
import           Language.Haskell.Exts

checkForHigherOrderFunction :: TypeCheck l (Response l)
checkForHigherOrderFunction (TyFun _ x _) = spotTyParen x
checkForHigherOrderFunction _             = Nothing

spotTyParen (TyParen info xs) =
    case xs of
        TyFun{} -> Just ("Higher-Order-Function", info)
        _       -> Nothing
spotTyParen _ = Nothing
