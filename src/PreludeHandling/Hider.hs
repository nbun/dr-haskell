module Hider where

import Data.List
import Language.Haskell.Exts
import ModifyAst

extractDefinedFun :: ModifiedModule -> [String]
extractDefinedFun (ModifiedModule _ (Module _ _ _ _ ds) _) = nub $ concatMap eD ds
  where
    eD :: Decl l -> [String]
    --eD (TypeDecl _ (DHead _ (Ident _ n)) _) = Just n
    --eD (TypeDecl _ (DHInfix _ _ (Ident _ n)) _) = Just n
    eD (TypeSig _ ns _) = map (\(Ident _ n)->n) ns
    eD (PatBind _ (PVar _ (Ident _ n)) _ _) = [n]
    eD (FunBind _ ms) = map eM ms
      where
        eM :: Match l -> String
        eM (Match _ (Ident _ n) _ _ _) = n
        eM (InfixMatch _ _ (Ident _ n) _ _ _) = n
    eD _ = []

extractDefinedData :: ModifiedModule -> [String]
extractDefinedData (ModifiedModule _ (Module _ _ _ _ ds) _) = nub $ concatMap eD ds
  where
    eD :: Decl l -> [String]
    eD (TypeDecl _ (DHead _ (Ident _ n)) _) = [n]
    eD (DataDecl _ _ _ (DHead _ (Ident _ n)) _ _) = [n]
    eD (DataDecl a b c (DHApp _ h _) d e) = eD (DataDecl a b c h d e)
    eD (DataDecl _ _ _ (DHInfix _ _ (Ident _ n)) _ _) = [n]
    eD _ = []

preludeFunctions = ["length", "head", "tail", "last", "map", "foldl", "foldr", "filter", "fst", "snd", "zip", "unzip"]
preludeDatatypes = ["Maybe", "String", "Either"]

makeImportDecl :: [String] -> [String] -> ImportDecl ()
makeImportDecl fs ts =
  let
    filteredfs = fs `intersect` preludeFunctions
    filteredts = ts `intersect` preludeDatatypes
    ivars = map (IVar () . Ident ()) filteredfs
    ithings = map (IThingAll () . Ident ()) filteredts
    name = ModuleName () "Prelude"
    qual  = False
    src   = False
    safe  = False
    pkg   = Nothing
    as    = Nothing
    specs = Just $ ImportSpecList () True (ivars++ithings)
  in
    ImportDecl () name qual src safe pkg as specs

impDeclFromMod :: ModifiedModule -> ImportDecl ()
impDeclFromMod m =
  let
    fs = extractDefinedFun m
    ds = extractDefinedData m
  in
    makeImportDecl fs ds

transformModule :: FilePath -> IO ModifiedModule
transformModule fp = do
  m <- parseModified fp
  let idecl = impDeclFromMod m
  let modified = addImport idecl m
  --putStrLn $ exactPrint (modifiedModule m) []
  putStrLn $ exactPrint (modifiedModule modified) (modifiedComments modified)
  return modified