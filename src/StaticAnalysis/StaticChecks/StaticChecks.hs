module StaticChecks where

import Language.Haskell.Exts
import Text.EditDistance

{-
main = do
  (ParseOk ast) <- parseFile "/home/niels/uni/ss17/masterprojekt/helium/Test.hs"
  print $ noFunDef ast
-}

--------------------------------------------------------------------------------
-- Signatures without a definition

nameString :: Name l -> String
nameString (Ident  _ s) = s
nameString (Symbol _ s) = s

declName :: Decl l -> [Name l]
declName d = case d of
               (TypeSig _ ns _) -> ns
               (FunBind _ ((Match _ n _ _ _) :_)) -> [n]
               (PatBind _ (PVar _ n) _ _) -> [n] -- functions without arguments
               _ -> []

declFilter :: (Decl l -> Bool) -> (Module l) -> [Decl l]
declFilter p (Module _ _ _ _ decls) = filter p decls
declFilter _ _                      = []

typeSigs :: (Module l) -> [Decl l]
typeSigs = declFilter isTypeSig
  where
    isTypeSig TypeSig{} = True
    isTypeSig _         = False

patBinds :: (Module l) -> [Decl l]
patBinds = declFilter isPatBind
  where
    isPatBind PatBind{} = True
    isPatBind _         = False


funBinds :: (Module l) -> [Decl l]
funBinds = declFilter isFunBind
  where
    isFunBind :: Decl l -> Bool
    isFunBind FunBind{} = True
    isFunBind _         = False

noFunDef :: (Module l) -> [Name l]
noFunDef m@Module{} = [sig | sig <- sigNames, nameString sig `notElem` defStrs]
  where sigNames   = concatMap declName $ typeSigs m
        defNames   = concatMap declName $ funBinds m ++ patBinds m
        defStrs    = map nameString defNames
noFunDef _ = []

--------------------------------------------------------------------------------
-- Finding similar names

calcLev :: Name l -> Name l -> Int
calcLev n m = levenshteinDistance defaultEditCosts s t
  where s = nameString n
        t = nameString m

