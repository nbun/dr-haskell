module StaticAnalysis.StaticChecks.NoTypeDef where

import           AstChecks.Check
import           Data.List
import           Language.Haskell.Exts
import           StaticAnalysis.StaticChecks.StaticChecks

noTypeDef :: ModuleCheck l (Response l)
noTypeDef m = let
    defs = defNames m
    defsNames  = map nameString defs
    defsWithNames = zip defsNames defs
    decls = map nameString (concatMap declName $ typeSigs m)
    diffDecls = defsNames \\ decls
    in buildResponse diffDecls defsWithNames

buildResponse :: [String] -> [(String,Name l)] -> [Response l]
buildResponse [] _ = []
buildResponse (x:xs) ys =
    case lookup x ys of
        Just v  -> Just ("Missing TypeDef for " ++ x, extractL v)
        Nothing -> Nothing
    : buildResponse xs ys

extractL :: Name l -> l
extractL (Ident k _)  = k
extractL (Symbol k _) = k
