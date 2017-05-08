module ArbitGen (
    extractDataDecls,
    tyConToGenExp,
    typeToInstance,
    modToInstance
) where

--Module for automatic generation of arbitrary instances

import Language.Haskell.Exts
import Data.List
import Data.Char
import Data.Functor
import Data.Maybe

extractDataDecls :: Module a -> [Decl a]
extractDataDecls (Module _ _ _ _ ds) = filter isDataDecl ds
  where
    isDataDecl (DataDecl _ (DataType _) _ _ _ _) = True
    isDataDecl _ = False

multiApplication :: Exp () -> [Exp ()] -> Exp ()
multiApplication e [] = e
multiApplication e (x:xs) = multiApplication (App () e x) xs

tyConToGenExp :: QualConDecl () -> Exp ()
tyConToGenExp (QualConDecl _ _ _ (ConDecl _ n ts)) =
  (Do () ([(Generator () (PVar () (Ident () ('x':show i))) (Var () (UnQual () (Ident () "arbitrary")))) | i <- [1..(length ts)]]++
  [Qualifier () (InfixApp () (Var () (UnQual () (Ident () "return"))) (QVarOp () (UnQual () (Symbol () "$"))) (multiApplication (Con () (UnQual () n)) [(Var () (UnQual () (Ident () ('x':show i))))| i <- [1..(length ts)]]))]))

typeToInstance :: Decl () -> Decl ()
typeToInstance (DataDecl _ _ _ (DHead _ (Ident _ name)) qds _) =
  InstDecl () Nothing (IRule () Nothing Nothing (IHApp () (IHCon () (UnQual () (Ident () "Arbitrary"))) (TyCon () (UnQual () (Ident () name))))) (Just [InsDecl () (PatBind () (PVar () (Ident () "arbitrary")) (UnGuardedRhs () (App () (Var () (UnQual () (Ident () "oneof"))) (List () (map tyConToGenExp qds)))) Nothing)])

parseFile' :: FilePath -> IO (Module SrcSpanInfo, [Comment])
parseFile' f = fromParseResult <$> parseFileWithComments defaultParseMode f

modToInstance :: FilePath -> IO ()
modToInstance fn = do
  m <- void <$> fst <$> parseFile' fn
  let dataDecls = extractDataDecls m
      insDecls  = typeToInstance <$> dataDecls
  mapM_ (putStrLn . prettyPrint) insDecls
