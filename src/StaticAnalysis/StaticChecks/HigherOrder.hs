-- | Holds the whole HigherOrderFunction-Occurence Check. Only checks on signatures
module StaticAnalysis.StaticChecks.HigherOrder (checkForHigherOrderFunction) where

import AstChecks.Check                      (TypeCheck)
import Language.Haskell.Exts                (Type (..))
import StaticAnalysis.Messages.StaticErrors (Error (..))

-- | The TypeCheck Implementation for HigherOrder checking
checkForHigherOrderFunction :: TypeCheck l (Error l)
checkForHigherOrderFunction (TyFun _ x y) = spotTyParen x ++ spotTyParen y
checkForHigherOrderFunction _             = []

-- | Checks if the Typefunction Signature containes a Paren type.
-- This indicates if the signature holds a HigherOrderFunction.
spotTyParen :: Type l -> [Error l]
spotTyParen (TyParen info xs) =
    case xs of
        TyFun{} -> [HigherOrder info]
        _       -> []
spotTyParen _ = []
