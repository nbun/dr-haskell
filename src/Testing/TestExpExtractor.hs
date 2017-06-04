module Testing.TestExpExtractor(
    extractComments,
    extractTests,
    replaceAllTests,
    buildTestMethod,
    transformFile
) where

import           Data.Char
import           Data.Functor
import           Data.List
import           Data.Maybe
import           Language.Haskell.Exts

import           Util.ModifyAst

--module for extracting tests specified in comments

infixr <.>

(<.>) :: Functor m => (b -> c) -> (a -> m b) -> a -> m c
(<.>) f g x = f <$> g x

parseFile' :: FilePath -> IO (Module SrcSpanInfo, [Comment])
parseFile' = fromParseResult <.> parseFileWithComments defaultParseMode

extractComments :: (a, [Comment]) -> [Comment]
extractComments = snd

commentLines :: Comment -> [String]
commentLines (Comment _ _ t) = lines t

commentsLines :: [Comment] -> [String]
commentsLines = concatMap commentLines

filterCommentLines :: [String] -> [String]
filterCommentLines = map (dropWhile (\x -> or $ ($ x) <$> [isSpace, (== '>')])) . filter (isPrefixOf "> ") . map (dropWhile isSpace)

extractTests :: (a, [Comment]) -> [Exp ()]
extractTests = map void . map fromParseResult . map parseExp . filterCommentLines . commentsLines . extractComments

makeTestsNode :: [Exp ()] -> Exp ()
makeTestsNode es = (List () es)

getPatBind :: String -> Module a -> Maybe (Decl a)
getPatBind n (Module _ _ _ _ ds) = find correctPat ds
  where
    correctPat (PatBind _ (PVar _ (Ident _ p)) _ _) = p == n
    correctPat _                                    = False

replaceAllTests :: Exp a -> Decl a -> Decl a
replaceAllTests replacement (PatBind l p (UnGuardedRhs l1 r) b) = (PatBind l p (UnGuardedRhs l1 r') b)
  where
    r' = repExp r
    repStms = map repStm
    repStm (Generator l p e) = Generator l p $ repExp e
    repStm (LetStmt l b)     = LetStmt l $ repBind b
    repStm a                 = a
    repExp (Var _ (UnQual _ (Ident _ "__allTests__"))) = replacement
    repExp (Do a ss)                                   = Do a $ repStms ss
    repExp a                                           = a
    repBind (BDecls l ds) = BDecls l $ map (replaceAllTests replacement) ds
    repBind a             = a

replaceAllTests _ a = a

buildTestMethod :: [Exp ()] -> IO (Decl ())
buildTestMethod es = do
  templateAST <- void <$> fst <$> parseFile' "../res/Testing/templates.hs"
  let Just runAllTestDecl = getPatBind "runAllTests" templateAST
  return $ replaceAllTests (makeTestsNode es) runAllTestDecl

--transformFile :: FilePath -> IO ()
transformFile fn = do
  m <- parseModified fn
  let tests = extractTests ((),modifiedComments m)
  testDeclAST <- buildTestMethod tests
  let
    impAdded = addImport (ImportDecl {importAnn = (), importModule = ModuleName () "Tests", importQualified = False, importSrc = False, importSafe = False, importPkg = Nothing, importAs = Nothing, importSpecs = Nothing}) m
    m' = appendDecl testDeclAST impAdded
  putStr $ exactPrint (modifiedModule m') (modifiedComments m')
  --writeFile (fn++".transformed.hs") $ prettyPrint modifiedMod
