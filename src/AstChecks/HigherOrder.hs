module AstChecks.HigherOrder where

import           Abstract
import           Control.Monad
import           Language.Haskell.Exts

runCheck :: FilePath -> IO [Response SrcSpanInfo]
runCheck path =
    checkAST <$> getAST path

checkAST :: Module l -> [Response l]
checkAST q =
    concat $ checkForHigherOrderFunctions q

checkForHigherOrderFunctions :: Module l -> [[Response l]]
checkForHigherOrderFunctions (Module _ _ _ _ x) =
    checkForHigherOrderFunctions' x

checkForHigherOrderFunctions' :: [Decl l] -> [[Response l]]
checkForHigherOrderFunctions' [] =
    []
checkForHigherOrderFunctions' (x:xs) =
    case x of
        TypeSig _ _ ys -> spotTyFun ys : checkForHigherOrderFunctions' xs
        _              -> checkForHigherOrderFunctions' xs

spotTyFun (TyFun _ x xs) =
    spotTyParen x ++ spotTyFun xs
spotTyFun _ =
    []

spotTyParen (TyParen info xs) =
    case xs of
        TyFun _ _ ys -> Resp "Higher-Order-Function" info : spotTyParen ys
        _            -> []
spotTyParen _ =
    []
