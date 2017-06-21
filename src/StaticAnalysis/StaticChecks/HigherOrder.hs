module StaticAnalysis.StaticChecks.HigherOrder (module StaticAnalysis.StaticChecks.HigherOrder) where

import           AstChecks.Check
import           Language.Haskell.Exts
import           StaticAnalysis.Messages.StaticErrors

checkForHigherOrderFunction :: TypeCheck l (Error l)
checkForHigherOrderFunction (TyFun _ x y) = spotTyParen x ++ spotTyParen y
checkForHigherOrderFunction _             = []

spotTyParen (TyParen info xs) =
    case xs of
        TyFun{} -> [HigherOrder info]
        _       -> []
spotTyParen _ = []
