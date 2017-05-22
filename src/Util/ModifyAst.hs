module Util.ModifyAst (
  Modification,
  ModifiedModule(..),
  parseModified,
  recordModification,
  addImport,
  prependDecl,
  appendDecl,
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
  modifiedModule   :: (Module SrcSpanInfo),
  modifiedComments :: [Comment]}
  deriving (Show)


parseModified :: FilePath -> IO ModifiedModule
parseModified fn = do
  ParseOk (m,c) <- parseFileWithComments defaultParseMode fn
  return $ ModifiedModule [] m c

insertModification :: Modification -> [Modification] -> [Modification]
insertModification a@(start, len) = (a:) . map (\(s,l) -> (s+if(s>=start) then len else 0,l))

recordModification :: Modification -> ModifiedModule -> ModifiedModule
recordModification a@(start,len) (ModifiedModule ms m cs) =
  (ModifiedModule
    (insertModification a ms)
    m
    (map (pushCommentAfter start len) cs))

class SrcSpanGenerator a where
  generateSrcSpanInfo :: a b -> a SrcSpanInfo

instance SrcSpanGenerator ImportDecl where
  generateSrcSpanInfo = fromParseResult . parseImportDecl . prettyPrint

instance SrcSpanGenerator Decl where
  generateSrcSpanInfo = fromParseResult . parseDecl . prettyPrint

modifySpanInfo :: Functor f => (SrcSpan -> SrcSpan) -> f SrcSpanInfo -> f SrcSpanInfo
modifySpanInfo modifySpan = fmap $ modifyInfo
  where
    modifyInfo (SrcSpanInfo s pts) = SrcSpanInfo (modifySpan s) (map modifySpan pts)

pushAfter :: Functor f => Int -> Int -> f SrcSpanInfo -> f SrcSpanInfo
pushAfter start len = modifySpanInfo $ \(SrcSpan f sl sc el ec) ->
  SrcSpan f
          (sl + if sl >= start then len else 0)
          sc
          (el + if el >= start then len else 0)
          ec

pushCommentAfter :: Int -> Int -> Comment -> Comment
pushCommentAfter start len (Comment ml (SrcSpan f sl sc el ec) t) = (Comment ml
  (SrcSpan f
           (sl + if sl >= start then len else 0)
           sc
           (el + if el >= start then len else 0)
           ec) t)

numLines :: Annotated f => f SrcSpanInfo -> Int
numLines x = let (SrcSpanInfo (SrcSpan _ sl _ el _) _) = ann x
             in el-sl+1

lastPos :: Maybe (ModuleHead SrcSpanInfo) -> [ModulePragma SrcSpanInfo] -> [ImportDecl SrcSpanInfo] -> [Decl SrcSpanInfo] -> Int
lastPos h ps is ds = case (h, ps, is, ds) of
    (Nothing, [], [], []) -> 0
    (Just x,  [], [], []) -> lastOfElement x
    (_,    (_:_), [], []) -> lastOfElement $ last ps
    (_,     _, (_:_), []) -> lastOfElement $ last is
    (_,      _, _, (_:_)) -> lastOfElement $ last ds
  where
    lastOfElement :: Annotated f => f SrcSpanInfo -> Int
    lastOfElement x = let (SrcSpanInfo (SrcSpan _ _ _ el _) _) = ann x
                      in el

fixFirstSpans :: Annotated a => Int -> SrcSpanInfo -> SrcSpanInfo -> a SrcSpanInfo -> SrcSpanInfo
fixFirstSpans n (SrcSpanInfo s xs) (SrcSpanInfo _ ys) z = SrcSpanInfo (minStart s) ((map minStart $ take n ys) ++ drop n xs)
  where
    (SrcSpanInfo (SrcSpan _ zs _ _ _) _) = ann z
    minStart :: SrcSpan -> SrcSpan
    minStart (SrcSpan f sr sc er ec)
      | sr < zs   = (SrcSpan f sr sc er ec)
      | otherwise = (SrcSpan f zs sc zs ec)

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
    l'' = fixFirstSpans (max (length ps' + 1) 2 + length is') l' l annDecl'
  in recordModification (pos,len) m{modifiedModule=(Module l'' h' ps' (is'++[annDecl']) ds')}

prependDecl :: Decl l -> ModifiedModule -> ModifiedModule
prependDecl d m =
  let
    ast = modifiedModule m
    annDecl = generateSrcSpanInfo d
    len = numLines annDecl
    (Module l h ps is ds) = ast
    pos = lastPos h ps is ds
    annDecl' = pushAfter 0 pos annDecl
  in recordModification (pos,len) m{modifiedModule=(Module l h ps is (ds++[annDecl']))}

appendDecl :: Decl l -> ModifiedModule -> ModifiedModule
appendDecl d m =
  let
    ast = modifiedModule m
    annDecl = generateSrcSpanInfo d
    len = numLines annDecl
    (Module l h ps is ds) = ast
    pos = lastPos h ps is []
    annDecl' = pushAfter 0 pos annDecl
    (Module l' h' ps' is' ds') = pushAfter (pos+1) len ast
  in recordModification (pos,len) m{modifiedModule=(Module l' h' ps' is' (annDecl' : ds'))}
