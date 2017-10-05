module Util.ModifyAst (
  Modification,
  ModifiedModule(..),
  parseModified,
  recordModification,
  addImport,
  prependDecl,
  appendDecl,
  addDeriving,
  addDerivingToAllData,
  printModified,
  translateLine,
) where

import Data.Char
import Data.Maybe
import Language.Haskell.Exts

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
  fileRaw <- readFile fn
  let file = reverse $ dropWhile isSpace $ reverse fileRaw
      pr = parseFileContentsWithComments (defaultParseMode { parseFilename = fn }) file
  case pr of
    ParseOk (m,c)   -> return $ ParseOk $ ModifiedModule [] m c
    ParseFailed l e -> return $ ParseFailed l e

insertModification :: Modification -> [Modification] -> [Modification]
insertModification a@(start, len) = (a:) . map (\(s,l) -> (s + if s>=start
                                                              then len
                                                              else 0, l))

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
modifySpanInfo :: Functor f =>
                  (SrcSpan -> SrcSpan)
               -> f SrcSpanInfo
               -> f SrcSpanInfo
modifySpanInfo modifySpan = fmap modifyInfo
  where
    modifyInfo (SrcSpanInfo s pts) = SrcSpanInfo (modifySpan s)
                                                 (map modifySpan pts)

restorePositionInfo :: Functor f =>
                       SrcSpanInfo
                    -> f SrcSpanInfo
                    -> f SrcSpanInfo
restorePositionInfo (SrcSpanInfo (SrcSpan f l _ _ _) _) =
  modifySpanInfo $ \(SrcSpan _ sl sc el ec) ->
                     SrcSpan f (sl+l-1) sc (el+l-1) ec

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
           (sl + if sl > start then len else 0)
           sc
           (el + if el > start then len else 0)
           ec) t

-- counts the number of lines that an AST element spans
numLines :: Annotated f => f SrcSpanInfo -> Int
numLines x = let (SrcSpanInfo (SrcSpan _ sl _ el _) _) = ann x
             in el-sl+1

-- finds the last position where an element is placed
-- assumes typical ordering of elements as can be found in a module
lastPos :: SrcSpanInfo
        -> Maybe (ModuleHead SrcSpanInfo)
        -> [ModulePragma SrcSpanInfo]
        -> [ImportDecl SrcSpanInfo]
        -> [Decl SrcSpanInfo]
        -> (Int,Int)
lastPos l h ps is ds = case (h, ps, is, ds) of
    (Nothing, [], [], []) -> (extractFirst l, extractFirst l + 1)
    (Just x,  [], [], []) -> mkTuple $ lastOfElement x
    (_,      _:_, [], []) -> mkTuple $ lastOfElement $ last ps
    (_,       _, _:_, []) -> mkTuple $ lastOfElement $ last is
    (_,        _, _, _:_) -> mkTuple $ lastOfElement $ last ds
  where
    lastOfElement :: Annotated f => f SrcSpanInfo -> Int
    lastOfElement x = let (SrcSpanInfo (SrcSpan _ _ _ el _) _) = ann x
                      in el
    extractFirst :: SrcSpanInfo -> Int
    extractFirst (SrcSpanInfo (SrcSpan _ sl _ _ _) _) = sl - 1
    mkTuple :: a -> (a,a)
    mkTuple x = (x,x)

-- haskell-src-exts uses the first two (or more) SrcSpan points to align the
-- module header
-- this is non obvious and not documented at all. THis behavior was reverse
-- engineered from the haskell-src-exts code and may be faulty and/or
-- incomplete.
fixFirstSpans :: Annotated a =>
                 Int
              -> SrcSpanInfo
              -> SrcSpanInfo
              -> a SrcSpanInfo
              -> SrcSpanInfo
fixFirstSpans n (SrcSpanInfo s xs) (SrcSpanInfo _ ys) z =
    SrcSpanInfo (minStart s) ((map minStart $ take n ys) ++ drop n xs)
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
    (elemPos, pushPos) = lastPos l h ps is []
    annDecl' = pushAfter 0 elemPos annDecl
    (Module l' h' ps' is' ds') = pushAfter (elemPos+1) len ast
    -- as imports may be added as the first lines of the module, we may have
    -- modified the first few position points (which behave strangely)
    -- we reset those to their previous values
    l'' = fixFirstSpans (max (length ps' + 1) 2 + length is')
                        l'
                        l
                        (pushAfter 0 (elemPos-pushPos) annDecl')
  in recordModification
       (pushPos,len)
       m{modifiedModule = Module l'' h' ps' (is'++[annDecl']) ds'}

appendDecl :: Decl l -> ModifiedModule -> ModifiedModule
appendDecl d m =
  let
    ast = modifiedModule m
    annDecl = generateSrcSpanInfo d
    len = numLines annDecl
    (Module l h ps is ds) = ast
    (elemPos, pushPos) = lastPos l h ps is ds
    annDecl' = pushAfter 0 elemPos annDecl
    Just l' = pushAfter elemPos len $ Just l
  in recordModification
       (pushPos,len)
       m{modifiedModule = Module l' h ps is (ds++[annDecl'])}

prependDecl :: Decl l -> ModifiedModule -> ModifiedModule
prependDecl d m =
  let
    ast = modifiedModule m
    annDecl = generateSrcSpanInfo d
    len = numLines annDecl
    (Module l h ps is ds) = ast
    (elemPos, pushPos) = lastPos l h ps is []
    annDecl' = pushAfter 0 elemPos annDecl
    (Module l' h' ps' is' ds') = pushAfter (elemPos+1) len ast
  in recordModification
       (pushPos,len)
       m{modifiedModule = Module l' h' ps' is' (annDecl' : ds')}

-- This is a huuuge hack as it doesn't modify the AST, but instead prints,
-- modifies the textual representation and then parses again. It works well
-- enough though.
-- Just make sure to only pass valid deriving strings
-- (eg: "deriving (Show, Eq, Ord)").
addDeriving' :: String
             -> Decl SrcSpanInfo
             -> (Decl SrcSpanInfo, Maybe (Int, Int))
addDeriving' d (DataDecl l t@(DataType _) ctx dh cs _) = let
    -- print into string, omit any previous deriving clause
    printed = exactPrint (DataDecl l t ctx dh cs Nothing) []
    -- append the new deriving clause
    appended = printed ++ (' ' : d)
    -- parse it back to AST elements
    ParseOk reparsed = parseDecl appended
    -- we need some offset for adjusting comments
    SrcSpanInfo (SrcSpan _ _ _ bel bec) _ =
      foldl (flip const) l (DataDecl l t ctx dh cs Nothing)
    SrcSpanInfo (SrcSpan _ _ _ _   aec) _ =
      foldl (flip const) l reparsed
  in
    (reparsed, Just (bel, aec - bec + 1))
addDeriving' _ x = (x, Nothing)

addDeriving :: String -> Decl SrcSpanInfo -> Decl SrcSpanInfo
addDeriving d = fst . addDeriving' d

addDerivingToAllData :: String -> ModifiedModule -> ModifiedModule
addDerivingToAllData d (ModifiedModule mods (Module l h ps is ds) cs) = let
      -- try to add the deriving clause to all fitting declarations
      modDs = map (addDeriving' d) ds
      -- extract all comment-relocations
      commentModders = foldl (.) id $ map fixComment $
                                          catMaybes (map snd modDs)
    in
      ModifiedModule mods
                     (Module l h ps is (map fst modDs))
                     (map commentModders cs)
  where
    -- add offsets to comments in the applicable lines
    fixComment (l,n) (Comment ml (SrcSpan f sl sc el ec) t) =
      Comment ml (SrcSpan f sl
        (if sl == l
         then sc + n
         else sc)
        el
        (if el == l
         then ec + n
         else ec) )
        t

printModified :: ModifiedModule -> String
printModified m = exactPrint (modifiedModule m) (modifiedComments m)

translateLine :: ModifiedModule -> Int -> Int
translateLine m l = translateLine' (modifications m) l - 1
  where
    translateLine' []         q = q
    translateLine' ((p,l):ms) q = translateLine' ms q'
      where
        q' = if q>p
                then q-l
                else q
