module AstChecks.TypeVars where

import           Abstract
import           Control.Monad
import           Language.Haskell.Exts

runCheck :: FilePath -> IO [Response SrcSpanInfo]
runCheck path = checkAST <$> getAST path

checkAST :: Module l -> [Response l]
checkAST q = concat (checkForTypVars q)

checkForTypVars :: Module l -> [[Response l]]
checkForTypVars (Module _ _ _ _ x) = checkForTypVars' x

checkForTypVars' :: [Decl l] -> [[Response l]]
checkForTypVars' [] = []
checkForTypVars' (x:xs) =
    case x of
        TypeDecl _ _ k -> checkForTypVar k : checkForTypVars' xs
        _              -> checkForTypVars' xs

checkForTypVar :: Type l -> [Response l]
checkForTypVar (TyFun _ x xs) =
    case x of
        (TyVar _ (Ident info name)) -> Resp ("Usage of Typevar " ++ name) info : checkForTypVar xs
        _                           -> checkForTypVar xs
checkForTypVar _ = []
