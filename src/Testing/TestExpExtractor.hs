module Testing.TestExpExtractor(
    extractComments,
    extractTests,
    replaceAllTests,
    buildTestMethod,
    transformModule
) where

import           Control.Arrow
import           Data.Char
import           Data.Either
import           Data.Functor
import           Data.List
import           Data.Maybe
import           Language.Haskell.Exts
import           Paths_drhaskell
import           StaticAnalysis.Messages.StaticErrors
import           Util.ModifyAst


--module for extracting tests specified in comments

infixr <.>

(<.>) :: Functor m => (b -> c) -> (a -> m b) -> a -> m c
(<.>) f g x = f <$> g x

parseFile' :: FilePath -> IO (Module SrcSpanInfo, [Comment])
parseFile' = fromParseResult <.> parseFileWithComments defaultParseMode

extractComments :: (a, [Comment]) -> [Comment]
extractComments = snd

commentLines :: Comment -> [(Int,String)]
commentLines (Comment _ l t) = let ln = srcSpanStartLine l : map (+1) ln in
                                   zip ln $ lines t

commentsLines :: [Comment] -> [(Int,String)]
commentsLines = concatMap commentLines

filterCommentLines :: [(Int,String)] -> [(Int,String)]
filterCommentLines = map (second (dropWhile (\x -> or $ ($ x) <$> [isSpace, (== '>')]))) . filter (isPrefixOf "> " . snd) . map (second (dropWhile isSpace))

-- this adds the test expression's string representation to the test expression
-- for pretty printing of failed tests
annotateTest :: Int -> String -> Exp () -> Exp ()
annotateTest l t e = case e of
  App () e1 e2 -> App () (annotateTest l t e1) e2
  InfixApp () e1 q e2 -> InfixApp () (annotateTest l t e1) q e2
  e1@(Var () (UnQual () (Ident () _))) ->
    App ()
        (App ()
             e1
             (Lit () (Int () (toInteger l) (show l))))
        (Lit () (String () t t))

-- check if an expression might be a valid test
-- very simple, does not find many errors, but should find some.
-- this should be much better once we have working type inference
checkTest :: Exp a -> Bool
checkTest a = case a of
                   InfixApp _ e q _ -> isDollar q && checkTest' e
                   App      _ e   _ -> checkTest' e
                   _                -> False
  where
    checkTest' (App _ e _)        = findTestFunc e
    checkTest' (InfixApp _ e q _) = isDollar q && findTestFunc e
    checkTest' _                  = False
    isDollar (QVarOp _ (UnQual _ (Symbol _ "$"))) = True
    isDollar _                                    = False
    findTestFunc (Var _ (UnQual _ (Ident _ "checkExpect"))) = True
    findTestFunc (Var _ (UnQual _ (Ident _ "quickCheck")))  = True
    findTestFunc (App _ e _)                                = findTestFunc e
    findTestFunc (InfixApp _ e q _)                         = isDollar q && findTestFunc e
    findTestFunc _                                          = False


-- try to parse and validate a test
parseTest :: (Int,String) -> Either (Error Int) (Exp ())
parseTest (l, s) = case parseExp s of
                        ParseFailed _ _ -> Left $ InvalidTest l s
                        ParseOk       e -> if checkTest e
                                         then Right $ annotateTest l s $ void e
                                         else Left $ InvalidTest l s

extractTests :: (a, [Comment]) -> [Either (Error Int) (Exp ())]
extractTests = map parseTest . filterCommentLines . commentsLines . extractComments

makeTestsNode :: [Exp ()] -> Exp ()
makeTestsNode = List ()

getPatBind :: String -> Module a -> Maybe (Decl a)
getPatBind n (Module _ _ _ _ ds) = find correctPat ds
  where
    correctPat (PatBind _ (PVar _ (Ident _ p)) _ _) = p == n
    correctPat _                                    = False

-- replaces the placeholder __allTests__ in the template with the actual list of tests
replaceAllTests :: Exp a -> Decl a -> Decl a
replaceAllTests replacement (PatBind l p (UnGuardedRhs l1 r) b) = PatBind l p (UnGuardedRhs l1 r') b
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

-- reads the template that contains the test method
buildTestMethod :: [Exp ()] -> IO (Decl ())
buildTestMethod es = do
  templateLoc <- getDataFileName "Testing/templates.hs"
  templateAST <- void . fst <$> parseFile' templateLoc
  let Just runAllTestDecl = getPatBind "runAllTests" templateAST
  return $ replaceAllTests (makeTestsNode es) runAllTestDecl

transformErrors :: Error Int -> Error SrcSpanInfo
transformErrors (InvalidTest l t) = let s = SrcLoc "" l 0 in InvalidTest (infoSpan (mkSrcSpan s s) []) t

-- does all of the above:
-- extracts all test expressions from comments, collects them in a list,
-- pushes this list into the template-loaded test method, adds this method
-- to the module and finally adds the needed modules
transformModule :: ModifiedModule -> IO (ModifiedModule, [Error SrcSpanInfo])
transformModule m = do
  let testsAndErrors = extractTests ((),modifiedComments m)
      tests          = rights testsAndErrors
      errors         = lefts testsAndErrors
  testDeclAST <- buildTestMethod tests
  let
    impAdded = addImport ImportDecl {importAnn = (), importModule = ModuleName () "Tests", importQualified = False, importSrc = False, importSafe = False, importPkg = Nothing, importAs = Nothing, importSpecs = Nothing} m
    m' = appendDecl testDeclAST impAdded
  return (m', transformErrors <$> errors)
  --writeFile (fn++".transformed.hs") $ prettyPrint modifiedMod
