module StaticAnalysis.StaticChecks.Pragma
    (
        pragmaCheck
    )
where

import           AstChecks.Check
import           Language.Haskell.Exts
import           StaticAnalysis.Messages.StaticErrors

-- | Pragma l String
pragmaCheck :: ModuleCheck SrcSpanInfo (Error SrcSpanInfo)
pragmaCheck (Module _ _ prag _ _) =
    concatMap loopPragma prag
    where
        loopPragma p =
            case p of
                (LanguagePragma _ names) -> [Pragma l n | (l,n) <- extractPosAndName names]
                (OptionsPragma l _ s) -> [Pragma l s]
                (AnnModulePragma _ a) -> [Pragma l n | (l,n) <- extractFromAnnotation a]
        extractPosAndName :: [Name SrcSpanInfo] -> [(SrcSpanInfo, String)]
        extractPosAndName [] = []
        extractPosAndName (Ident pos name:xs) = (pos,name) : extractPosAndName xs
        extractPosAndName (Symbol pos name:xs) = (pos,name) : extractPosAndName xs
        extractFromAnnotation :: Annotation SrcSpanInfo -> [(SrcSpanInfo, String)]
        extractFromAnnotation (Ann _ n _)     = extractPosAndName [n]
        extractFromAnnotation (TypeAnn _ n _) = extractPosAndName [n]
        extractFromAnnotation _               = []
