-- | Check if a definition misses an accompanying type signature
module StaticAnalysis.StaticChecks.NoTypeDef (noTypeDef) where

import AstChecks.Check                      (ModuleCheck)
import Data.List                            ((\\))
import Language.Haskell.Exts                (Name)
import StaticAnalysis.Messages.StaticErrors (Error (..))
import StaticAnalysis.StaticChecks.Select   (declName, defFuncs, nameString,
                                             typeSigs)

-- | Checks if a definition misses an accompanying type signature
noTypeDef :: ModuleCheck l (Error l)
noTypeDef m = let
    defs = map fst $ defFuncs m
    defsNames  = map nameString defs
    defsWithNames = zip defsNames defs
    decls = map nameString (concatMap (map fst . declName) $ typeSigs m)
    diffDecls = defsNames \\ decls
    in buildResponse diffDecls defsWithNames

buildResponse :: [String] -> [(String,Name l)] -> [Error l]
buildResponse [] _ = []
buildResponse (x:xs) ys =
    case lookup x ys of
        Just v  -> [NoTypeDef v]
        Nothing -> []
    ++ buildResponse xs ys
