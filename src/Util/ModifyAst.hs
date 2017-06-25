module Util.ModifyAst (
  Modification,
  ModifiedModule(..),
  parseModified,
  recordModification,
  addImport,
  prependDecl,
  appendDecl,
  printModified,
) where

import           Language.Haskell.Exts

{-
  Module for modifying modules while preserving positional information.
  These things work at the moment:
    adding a new import
    adding a declaration to the end of the file
    adding a declaration to the beginning of all declarations
    Realigning comments

  These things are ToDo:
    Modifying elements inline, especially expressions
    Adding module head stuff
    Dealing with column information?
-}

type Modification = (Int, Int)

--TODO: monad instance possible?
data ModifiedModule = ModifiedModule {
  modifications    :: [Modification],
  modifiedModule   :: Module SrcSpanInfo,
  modifiedComments :: [Comment]}
  deriving (Show)


parseModified :: FilePath -> IO (ParseResult ModifiedModule)
parseModified fn = do
  pr <- parseFileWithComments (defaultParseMode { parseFilename = fn }) fn
  case pr of
    ParseOk (m,c)   -> return $ ParseOk $ ModifiedModule [] m c
    ParseFailed l e -> return $ ParseFailed l e

insertModification :: Modification -> [Modification] -> [Modification]
insertModification a@(start, len) = (a:) . map (\(s,l) -> (s + if s>=start then len else 0, l))

-- this function handles comments transparently when recording a modification
recordModification :: Modification -> ModifiedModule -> ModifiedModule
recordModification a@(start,len) (ModifiedModule ms m cs) =
  ModifiedModule
    (insertModification a ms)
    m
    (map (pushCommentAfter start len) cs)

-- If we have AST elements without position info (usually l is ()), we cannot
-- insert these into an AST with position info
-- generateSrcSpanInfo shall generate valid position info for AST elements.
-- all supplied instances do this by doing a round trip of pretty printing and
-- then parsing again.
class SrcSpanGenerator a where
  generateSrcSpanInfo :: a b -> a SrcSpanInfo

instance SrcSpanGenerator ImportDecl where
  generateSrcSpanInfo = fromParseResult . parseImportDecl . prettyPrint

instance SrcSpanGenerator Decl where
  generateSrcSpanInfo = fromParseResult . parseDecl . prettyPrint

-- generalized map for SrcSpanInfo inside a functor
modifySpanInfo :: Functor f => (SrcSpan -> SrcSpan) -> f SrcSpanInfo -> f SrcSpanInfo
modifySpanInfo modifySpan = fmap modifyInfo
  where
    modifyInfo (SrcSpanInfo s pts) = SrcSpanInfo (modifySpan s) (map modifySpan pts)

-- adds the length of an insertion to all row information that are below the
-- start line of the insertion
pushAfter :: Functor f => Int -> Int -> f SrcSpanInfo -> f SrcSpanInfo
pushAfter start len = modifySpanInfo $ \(SrcSpan f sl sc el ec) ->
  SrcSpan f
          (sl + if sl >= start then len else 0)
          sc
          (el + if el >= start then len else 0)
          ec

-- same as pushAfter, but for comments
pushCommentAfter :: Int -> Int -> Comment -> Comment
pushCommentAfter start len (Comment ml (SrcSpan f sl sc el ec) t) =
  Comment ml
    (SrcSpan f
           (sl + if sl >= start then len else 0)
           sc
           (el + if el >= start then len else 0)
           ec) t

-- counts the number of lines that an AST element spans
numLines :: Annotated f => f SrcSpanInfo -> Int
numLines x = let (SrcSpanInfo (SrcSpan _ sl _ el _) _) = ann x
             in el-sl+1

-- finds the last position where an element is placed
-- assumes typical ordering of elements as can be found in a module
lastPos :: Maybe (ModuleHead SrcSpanInfo) -> [ModulePragma SrcSpanInfo] -> [ImportDecl SrcSpanInfo] -> [Decl SrcSpanInfo] -> Int
lastPos h ps is ds = case (h, ps, is, ds) of
    (Nothing, [], [], []) -> 0
    (Just x,  [], [], []) -> lastOfElement x
    (_,      _:_, [], []) -> lastOfElement $ last ps
    (_,       _, _:_, []) -> lastOfElement $ last is
    (_,        _, _, _:_) -> lastOfElement $ last ds
  where
    lastOfElement :: Annotated f => f SrcSpanInfo -> Int
    lastOfElement x = let (SrcSpanInfo (SrcSpan _ _ _ el _) _) = ann x
                      in el

-- haskell-src-exts uses the first two (or more) SrcSpan points to align the
-- module header
-- this is non obvious and not documented at all. THis behavior was reverse
-- engineered from the haskell-src-exts code and may be faulty and/or
-- incomplete.
fixFirstSpans :: Annotated a => Int -> SrcSpanInfo -> SrcSpanInfo -> a SrcSpanInfo -> SrcSpanInfo
fixFirstSpans n (SrcSpanInfo s xs) (SrcSpanInfo _ ys) z = SrcSpanInfo (minStart s) ((map minStart $ take n ys) ++ drop n xs)
  where
    (SrcSpanInfo (SrcSpan _ zs _ _ _) _) = ann z
    minStart :: SrcSpan -> SrcSpan
    minStart (SrcSpan f sr sc er ec)
      | sr < zs   = SrcSpan f sr sc er ec
      | otherwise = SrcSpan f zs sc zs ec


-- adding an element always takes a few steps:
-- 1. the supplied element is annotated with valid position information
-- 2. the position of the new element is determined (using lastPos)
-- 3. all existing elements *after* this position are pushed down by the number
--    of lines that the insertion spans
-- 4. the new element is inserted at the proper position
-- 5. the ModifiedModule structure records the lines that the insertion
--    happened at

addImport :: ImportDecl l -> ModifiedModule -> ModifiedModule
addImport d m =
  let
    ast = modifiedModule m
    annDecl = generateSrcSpanInfo d
    len = numLines annDecl
    (Module l h ps is ds) = ast
    pos = lastPos h ps is []
    annDecl' = pushAfter 0 pos annDecl
    (Module l' h' ps' is' ds') = pushAfter (pos+1) len ast
    -- as imports may be added as the first lines of the module, we may have
    -- modified the first few position points (which behave strangely)
    -- we reset those to their previous values
    l'' = fixFirstSpans (max (length ps' + 1) 2 + length is') l' l annDecl'
  in recordModification (pos,len) m{modifiedModule=(Module l'' h' ps' (is'++[annDecl']) ds')}

appendDecl :: Decl l -> ModifiedModule -> ModifiedModule
appendDecl d m =
  let
    ast = modifiedModule m
    annDecl = generateSrcSpanInfo d
    len = numLines annDecl
    (Module l h ps is ds) = ast
    pos = lastPos h ps is ds
    annDecl' = pushAfter 0 pos annDecl
    Just l' = pushAfter pos len $ Just l
  in recordModification (pos,len) m{modifiedModule=(Module l' h ps is (ds++[annDecl']))}

prependDecl :: Decl l -> ModifiedModule -> ModifiedModule
prependDecl d m =
  let
    ast = modifiedModule m
    annDecl = generateSrcSpanInfo d
    len = numLines annDecl
    (Module l h ps is ds) = ast
    pos = lastPos h ps is []
    annDecl' = pushAfter 0 pos annDecl
    (Module l' h' ps' is' ds') = pushAfter (pos+1) len ast
  in recordModification (pos,len) m{modifiedModule=(Module l' h' ps' is' (annDecl' : ds'))}

printModified :: ModifiedModule -> String
printModified m = exactPrint (modifiedModule m) (modifiedComments m)
