module ModifyAst where

import Language.Haskell.Exts

{-
  Module for modifying modules while preserving positional information.
  These things work at the moment:
    adding a new import
    adding a declaration to the end of the file
    adding a declaration to the beginning of all declarations

  These things are ToDo:
    Modifying elements inline, especially expressions
    Adding module head stuff
    Dealing with column information?
    Realigning comments
-}

type Modification = (Int, Int)

--TODO: monad instance possible?
data ModifiedModule = ModifiedModule {
  modifications :: [Modification],
  modifiedModule :: (Module SrcSpanInfo)}
  deriving (Show)


parseModified :: FilePath -> IO ModifiedModule
parseModified fn = do
  ParseOk m <- parseFile fn
  return $ ModifiedModule [] m

insertModification :: Modification -> [Modification] -> [Modification]
insertModification a@(start, len) = (a:) . map (\(s,l) -> (s+if(s>=start) then len else 0,l))

adjustSpanToZero :: (Foldable f, Functor f) => f SrcSpanInfo -> f SrcSpanInfo
adjustSpanToZero x =
  let minLine = foldl (\m (SrcSpanInfo (SrcSpan _ sl _ el _) _) -> minimum [sl, el, m]) maxBound x
  in pushAfter minLine (-minLine) x

fixModulePath :: Annotated f => (Module SrcSpanInfo) -> f SrcSpanInfo -> f SrcSpanInfo
fixModulePath ast x =
  let
    (SrcSpanInfo (SrcSpan name _ _ _ _) _) = ann x
    modifyFilename (SrcSpanInfo s ss) = (SrcSpanInfo (modifyFilename' s) (map modifyFilename' ss))
    modifyFilename' (SrcSpan _ a b c d) = SrcSpan name a b c d
  in
    fmap modifyFilename x

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

minLine :: Annotated l => l SrcSpanInfo -> Int
minLine x = let (SrcSpanInfo (SrcSpan _ sl _ _ _) _) = ann x in sl

resetMinLine :: Annotated l => Int -> l SrcSpanInfo -> l SrcSpanInfo
resetMinLine l = amap (\(SrcSpanInfo (SrcSpan n _ a b c) xs) -> (SrcSpanInfo (SrcSpan n l a b c) xs))

insertSrcInfo :: Int -> SrcSpan -> SrcSpanInfo -> SrcSpanInfo
insertSrcInfo pos pt info =
  let
    (prev, after) = splitAt pos (srcInfoPoints info)
    pt' = pt{srcSpanEndColumn=1}
  in
    info {srcInfoPoints = prev ++ pt':after}

addImport :: ImportDecl l -> ModifiedModule -> ModifiedModule
addImport d (ModifiedModule mods ast) =
  let
    --ml = minLine ast
    annDecl = generateSrcSpanInfo d
    len = numLines annDecl
    (Module l h ps is ds) = ast
    pos = lastPos h ps is []
    annDecl' = pushAfter 0 pos annDecl
    (Module l' h' ps' is' ds') = pushAfter (pos+1) len ast
    --l'' = insertSrcInfo (max (length ps' + 1) 2 + length is') (srcInfoSpan $ ann annDecl') l'
  in ModifiedModule (insertModification (pos,len) mods) (Module l h' ps' (is'++[annDecl']) ds')

prependDecl :: Decl l -> ModifiedModule -> ModifiedModule
prependDecl d (ModifiedModule mods ast) =
  let
    annDecl = generateSrcSpanInfo d
    len = numLines annDecl
    (Module l h ps is ds) = ast
    pos = lastPos h ps is ds
    annDecl' = pushAfter 0 pos annDecl
  in ModifiedModule (insertModification (pos,len) mods) (Module l h ps is (ds++[annDecl']))

appendDecl :: Decl l -> ModifiedModule -> ModifiedModule
appendDecl d (ModifiedModule mods ast) =
  let
    annDecl = generateSrcSpanInfo d
    len = numLines annDecl
    (Module l h ps is ds) = ast
    pos = lastPos h ps is []
    annDecl' = pushAfter 0 pos annDecl
    (Module l' h' ps' is' ds') = pushAfter (pos+1) len ast
  in ModifiedModule (insertModification (pos,len) mods) (Module l' h' ps' is' (annDecl' : ds'))