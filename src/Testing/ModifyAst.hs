module ModifyAst where

import Language.Haskell.Exts

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


class SrcSpanGenerator a where
  generateSrcSpanInfo :: a b -> a SrcSpanInfo

instance SrcSpanGenerator ImportDecl where
  generateSrcSpanInfo = fromParseResult . parseImportDecl . prettyPrint

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


addImport :: ImportDecl l -> ModifiedModule -> ModifiedModule
addImport d (ModifiedModule mods ast) =
  let
    annDecl = generateSrcSpanInfo d
    len = numLines annDecl
    (Module l h ps is ds) = ast
    pos = lastPos h ps is []
    annDecl' = pushAfter 0 (pos) annDecl
    (Module l' h' ps' is' ds') = pushAfter (pos+1) len ast
  in ModifiedModule (insertModification (pos,len) mods) (Module l' h' ps' (is'++[annDecl']) ds')