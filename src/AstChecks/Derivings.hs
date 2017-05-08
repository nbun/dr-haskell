module AstChecks.DerivingInstance where

import           Abstract
import           Control.Monad
import           Language.Haskell.Exts

runCheck :: FilePath -> IO [Response SrcSpanInfo]
runCheck path =
    checkAST <$> getAST path

checkAST :: Module l -> [Response l]
checkAST q =
    concat $ checkForDerivings q

checkForDerivings :: Module l -> [[Response l]]
checkForDerivings (Module _ _ _ _ x) =
    checkForDerivings' x

checkForDerivings' :: [Decl l] -> [[Response l]]
checkForDerivings' [] =
    []
checkForDerivings' (x:xs) =
    case x of
        DataDecl _ _ _ _ _ k -> checkForDeriving k : checkForDerivings' xs
        _                    -> checkForDerivings' xs

checkForDeriving :: Maybe (Deriving l) -> [Response l]
checkForDeriving (Just (Deriving _ rules)) = scanRules rules
checkForDeriving _                         = []

scanRules :: [InstRule l] -> [Response l]
scanRules []                     = []
scanRules (IRule _ _ _ (IHCon _ (UnQual _ (Ident info name))):rs) =
    Resp ("Deriving " ++ name) info : scanRules rs

appendRuleToModule :: String -> Module SrcSpanInfo -> Module SrcSpanInfo
appendRuleToModule name (Module a b c d e) =
    Module a b c d (appendRuleToDataDecl name e)

appendRuleToDataDecl :: String -> [Decl SrcSpanInfo] -> [Decl SrcSpanInfo]
appendRuleToDataDecl _ [] =
    []
appendRuleToDataDecl name (DataDecl a b c d e f:xs) =
    DataDecl a b c d e (modifyDeriving name f) : appendRuleToDataDecl name xs
appendRuleToDataDecl name (x:xs) =
    x : appendRuleToDataDecl name xs

modifyDeriving :: String -> Maybe (Deriving SrcSpanInfo) -> Maybe (Deriving SrcSpanInfo)
modifyDeriving name Nothing =
    Just (Deriving noSrcSpan (appendRule name []))
modifyDeriving name (Just (Deriving a b)) =
    Just (Deriving a (appendRule name b))

appendRule :: String -> [InstRule SrcSpanInfo] -> [InstRule SrcSpanInfo]
appendRule s xs =
    if containsRule s xs
        then xs
        else xs ++ [IRule noSrcSpan Nothing Nothing (IHCon noSrcSpan (UnQual noSrcSpan (Ident noSrcSpan s)))]

containsRule :: String -> [InstRule SrcSpanInfo] -> Bool
containsRule _ [] =
    False
containsRule s (IRule _ _ _ (IHCon _ (UnQual _ (Ident _ name))):rs) =
    (name == s) || containsRule s rs
