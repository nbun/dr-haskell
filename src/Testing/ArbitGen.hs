module Testing.ArbitGen (
    extractDataDecls,
    tyConToGenExp,
    typeToInstance,
    modToInstance,
    generateArbitraryInModule,
) where

--Module for automatic generation of arbitrary instances

import Control.Arrow
import Data.Char
import Data.Functor
import Data.List
import Data.Maybe
import Language.Haskell.Exts
import Util.ModifyAst

extractDataDecls :: Module a -> [Decl a]
extractDataDecls (Module _ _ _ _ ds) = filter isDataDecl ds
  where
    isDataDecl (DataDecl _ (DataType _) _ _ _ _) = True
    isDataDecl _                                 = False

multiApplication :: Exp () -> [Exp ()] -> Exp ()
multiApplication = foldl (App ())

tyConToGenExp :: QualConDecl () -> Exp ()
tyConToGenExp (QualConDecl _ _ _ (ConDecl _ n ts)) =
  Do () ([Generator () (PVar () (Ident () ('x':show i))) (Var () (Qual () (ModuleName () "Tests") (Ident () "arbitrary"))) | i <- [1..(length ts)]]++
  [Qualifier () (InfixApp () (Var () (Qual () (ModuleName () "Prelude") (Ident () "return"))) (QVarOp () (Qual () (ModuleName () "Prelude") (Symbol () "$"))) (multiApplication (Con () (UnQual () n)) [Var () (UnQual () (Ident () ('x':show i))) | i <- [1..(length ts)]]))])
tyConToGenExp (QualConDecl l1 tv c (RecDecl l2 n ts)) = tyConToGenExp (QualConDecl l1 tv c (ConDecl l2 n $ fieldsToTypes ts))
  where
    fieldsToTypes :: [FieldDecl ()] -> [Type ()]
    fieldsToTypes = concatMap fieldToTypes
    fieldToTypes :: FieldDecl () -> [Type ()]
    fieldToTypes (FieldDecl l ns t) = replicate (length ns) t

typeToInstance :: Decl () -> Decl ()
typeToInstance (DataDecl _ _ _ h qds _) =
    InstDecl () Nothing irule (Just [InsDecl () (PatBind () (PVar () (Ident () "arbitrary")) (UnGuardedRhs () (App () (Var () (Qual () (ModuleName () "Tests") (Ident () "oneof"))) (List () (map tyConToGenExp qds)))) Nothing)])
  where
    (name, tyvars) = deconstructHead h
    deconstructHead :: DeclHead l -> (String, [String])
    deconstructHead (DHead _ (Ident _ name)) = (name, [])
    deconstructHead (DHParen _ h)            = deconstructHead h
    deconstructHead (DHApp _ h (UnkindedVar _ (Ident _ tv)))   = (tv:) `second` deconstructHead h
    deconstructHead (DHInfix _ (UnkindedVar _ (Ident _ tv)) (Ident _ name)) = (name, [tv])
    irule = IRule () Nothing context (IHApp () (IHCon () (Qual () (ModuleName () "Tests") (Ident () "Arbitrary"))) instHead)
    context = buildContext tyvars
    buildContext [] = Nothing
    buildContext xs = Just $ CxTuple () $ map buildContext' xs
    buildContext' x = ClassA () (Qual () (ModuleName () "Tests") (Ident () "Arbitrary")) [TyVar () (Ident () x)]
    instHead = TyParen () $ buildInstHead tyvars
    buildInstHead []     = TyCon () (UnQual () (Ident () name))
    buildInstHead (x:xs) = TyApp () (buildInstHead xs) (TyVar () (Ident () x))

parseFile' :: FilePath -> IO (Module SrcSpanInfo, [Comment])
parseFile' f = fromParseResult <$> parseFileWithComments defaultParseMode f

modToInstance :: FilePath -> IO ()
modToInstance fn = do
  m <- void . fst <$> parseFile' fn
  let dataDecls = extractDataDecls m
      insDecls  = typeToInstance <$> dataDecls
  mapM_ (putStrLn . prettyPrint) insDecls

generateArbitraryInModule :: ModifiedModule -> ModifiedModule
generateArbitraryInModule m =
  let
    dataDecls = extractDataDecls $ void $ modifiedModule m
    insDecls  = typeToInstance <$> dataDecls
  in
    foldl (flip appendDecl) m insDecls

processModule :: FilePath -> IO ()
processModule fn = do
  ParseOk m <- parseModified fn
  let m' = generateArbitraryInModule m
  putStrLn $ exactPrint (modifiedModule m') (modifiedComments m')
