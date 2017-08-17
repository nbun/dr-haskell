module TypeInference.TypeSig
  (parseTypeSig, parseTypeSignatur) where

import           Data.Functor
import           Language.Haskell.Exts

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

parseTypeSignatur :: Module l -> [(Name l, Type l)]
parseTypeSignatur (Module l mh mp impdec decls) = parseDecls decls
parseTypeSignatur _                             = []

parseDecls :: [Decl l] -> [(Name l,Type l)]
parseDecls = foldr ((++) . parseOneDecl) []

parseOneDecl :: Decl l -> [(Name l,Type l)]
parseOneDecl (TypeSig l names t) = concatMap (parseTypeSig t) names
parseOneDecl _                   = []

parseTypeSig :: Type l -> Name l ->[(Name l,Type l)]
parseTypeSig typ name = [(name,typ)]

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- parseTypeSignatur mit Strings als Namen -------------------------------------

parseTypeSignaturSN :: Module l -> [(String, Type l)]
parseTypeSignaturSN (Module l mh mp impdec decls) = parseDeclsSN decls
parseTypeSignaturSN _                             = []

parseDeclsSN :: [Decl l] -> [(String, Type l)]
parseDeclsSN []     = []
parseDeclsSN (x:xs) = parseOneDeclSN x ++ parseDeclsSN xs

parseOneDeclSN :: Decl l -> [(String, Type l)]
parseOneDeclSN (TypeSig l names t) = concatMap (parseTypeSigSN t) names
parseOneDeclSN _                   = []

parseTypeSigSN :: Type l -> Name l ->[(String, Type l)]
parseTypeSigSN typ (Ident l name)  = [(name,typ)]
parseTypeSigSN typ (Symbol l name) = [(name,typ)]

--------------------------------------------------------------------------------
-- parseTypeSig mit Strings als Namen und Typen --------------------------------
--------------------------------------------------------------------------------

parseTypeSignaturSNT :: Module l -> [(String, String)]
parseTypeSignaturSNT (Module l mh mp impdec decls) = parseDeclsSNT decls
parseTypeSignaturSNT _                             = []

parseDeclsSNT :: [Decl l] -> [(String,String)]
parseDeclsSNT []     = []
parseDeclsSNT (x:xs) = parseOneDeclSNT x ++ parseDeclsSNT xs

parseOneDeclSNT :: Decl l -> [(String, String)]
parseOneDeclSNT (TypeSig l names t) = concatMap (parseTypeSigSNT t) names
parseOneDeclSNT _                   = []

parseTypeSigSNT :: Type l -> Name l ->[(String, String)]
parseTypeSigSNT typ (Ident l name) = [(name, parseTyp typ)]

parseTyp :: Type l -> String
parseTyp (TyVar _ (Ident _ name))  = show name
parseTyp (TyVar _ (Symbol _ name)) = show name
parseTyp (TyTuple _ _ types)       = "(" ++ parseTypList types ++ ")"
parseTyp (TyParen _ typ)           = "(" ++ parseTyp typ ++ ")"
parseTyp (TyCon _ (UnQual _ (Ident _  name))) = show name
parseTyp (TyCon _ (UnQual _ (Symbol _  name))) = show name
parseTyp (TyList _ t)              = "[" ++ parseTyp t ++ "]"
parseTyp (TyFun l t1 t2)           = parseTyp t1 ++ "->" ++ parseTyp t2
parseTyp _                         = ""

parseTypList :: [Type l] -> String
parseTypList []     = []
parseTypList (t:ts) = show (parseTyp t) ++ "," ++ parseTypList ts
