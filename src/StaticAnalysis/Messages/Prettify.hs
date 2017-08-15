-- | Pretty printing of errors
module StaticAnalysis.Messages.Prettify (
    module StaticAnalysis.Messages.Prettify
) where

import           Data.List
import           Language.Haskell.Exts
import           StaticAnalysis.Messages.StaticErrors
import           StaticAnalysis.StaticChecks.Select

type Filename = String
type Position = (Int, Int, Int, Int)

-- | A level determine the checks applied to a module
data Level = Level1 | Level2 | Level3 | LevelFull
  deriving Show

printFilenameAndPos :: Filename -> Position -> String
printFilenameAndPos filename pos =
    let (line, column, _, _) = pos
    in filename ++ ":" ++ show line ++ ":" ++ show column ++ ":\n"

printFilenameAndPosWithSwitch :: Bool -> Filename -> Position -> String
printFilenameAndPosWithSwitch False _ _ = ""
printFilenameAndPosWithSwitch True  f p = printFilenameAndPos f p

prettyError :: Error SrcSpanInfo -> String
prettyError = prettyErrorWithInfoSwitch False

prettyErrorForLintWithLevel :: Maybe Level -> Error SrcSpanInfo -> String
prettyErrorForLintWithLevel = prettyErrorWithInfoSwitchAndLevel True

prettyErrorForLint :: Error SrcSpanInfo -> String
prettyErrorForLint = prettyErrorWithInfoSwitch True

prettyErrorWithInfoSwitch :: Bool -> Error SrcSpanInfo -> String
prettyErrorWithInfoSwitch s = prettyErrorWithInfoSwitchAndLevel s Nothing

prettyErrorWithInfoSwitchAndLevel :: Bool -> Maybe Level
                                          -> Error SrcSpanInfo
                                          -> String
prettyErrorWithInfoSwitchAndLevel s level e@(NoFunDef name sims) =
    let (filename, pos) = extractFilenameAndPositionFromName name
    in printFilenameAndPosWithSwitch s filename pos
       ++ appendLevelTag level e
       ++ "Type signature for " ++ prettyPrintQ name ++ " at "
       ++ prettyNameLoc name ++ " without a definition.\n" ++ prettySims sims
prettyErrorWithInfoSwitchAndLevel s level e@(Undefined name sims) =
    let (filename, pos) = extractFilenameAndPositionFromName name
    in printFilenameAndPosWithSwitch s filename pos
       ++ appendLevelTag level e
       ++ "Undefined identifier " ++ prettyPrintQ name ++ " at "
       ++ prettyNameLoc name ++ ".\n" ++ prettySims sims
prettyErrorWithInfoSwitchAndLevel s level e@(Duplicated name entity maymod) =
    let (filename, pos) = extractFilenameAndPositionFromName name
    in printFilenameAndPosWithSwitch s filename pos
       ++ appendLevelTag level e
       ++ show entity ++ prettyPrintQ name ++ " at " ++ prettyNameLoc name
       ++ " is already defined" ++ mod ++ "."
    where mod = case maymod of
                  Just mname -> " in module " ++ prettyPrintQ mname
                  Nothing    -> ""
prettyErrorWithInfoSwitchAndLevel s level e@(TypeVarApplication name) =
    let (filename, pos) = extractFilenameAndPositionFromName name
    in printFilenameAndPosWithSwitch s filename pos
       ++ appendLevelTag level e
       ++ "Type variable " ++ prettyPrintQ name ++ " at " ++ prettyNameLoc name
       ++ " cannot be applied to another type."
prettyErrorWithInfoSwitchAndLevel s level e@(HigherOrder position) =
    let (filename, pos) = extractFilenameAndPosition position
    in printFilenameAndPosWithSwitch s filename pos
       ++ appendLevelTag level e
       ++ "HigherOrder function located at " ++ prettyLoc position ++ "."
prettyErrorWithInfoSwitchAndLevel s level e@(LambdaFunction position) =
    let (filename, pos) = extractFilenameAndPosition position
    in printFilenameAndPosWithSwitch s filename pos
       ++ appendLevelTag level e
       ++ "Lambda function located at " ++ prettyLoc position ++ "."
prettyErrorWithInfoSwitchAndLevel s level e@(NoTypeDef name) =
    let (filename, pos) = extractFilenameAndPositionFromName name
    in printFilenameAndPosWithSwitch s filename pos
       ++ appendLevelTag level e
       ++ "No TypeSignature for function named " ++ prettyPrintQ name ++ " at "
       ++ prettyNameLoc name ++ "."
prettyErrorWithInfoSwitchAndLevel s level e@(Shadowing qname) =
    let (filename, pos) = extractFilenameAndPositionFromQName qname
    in printFilenameAndPosWithSwitch s filename pos
       ++ appendLevelTag level e
       ++ "Found shadowing of variable " ++ getNameOfQName qname ++ " at "
       ++ prettyLoc (extractPositionFromQname qname) ++ "."
prettyErrorWithInfoSwitchAndLevel s level e@(TypeVar name) =
    let (filename, pos) = extractFilenameAndPositionFromName name
    in printFilenameAndPosWithSwitch s filename pos
       ++ appendLevelTag level e
       ++ "Found typevariable " ++ prettyPrintQ name ++ " at "
       ++ prettyNameLoc name ++ "."
prettyErrorWithInfoSwitchAndLevel s level e@(Imported name) =
    let (filename, pos) = extractFilenameAndPositionFromModuleName name
    in printFilenameAndPosWithSwitch s filename pos
       ++ appendLevelTag level e
       ++ "Found import " ++ prettyPrintQ name ++ " at "
       ++ prettyModNameLoc name ++ "."
prettyErrorWithInfoSwitchAndLevel s level e@(ModuleHeadUsed name) =
    let (filename, pos) = extractFilenameAndPositionFromModuleName name
    in printFilenameAndPosWithSwitch s filename pos
       ++ appendLevelTag level e
       ++ "Found module head " ++ prettyPrintQ name ++ " at "
       ++ prettyModNameLoc name ++ "."
prettyErrorWithInfoSwitchAndLevel s level e@(OwnDataDecl l) =
    let (filename, pos) = extractFilenameAndPosition l
    in printFilenameAndPosWithSwitch s filename pos
       ++ appendLevelTag level e
       ++ "Found data declaration or type synonym at " ++ prettyLoc l ++ "."
prettyErrorWithInfoSwitchAndLevel s level e@(DoUsed l) =
    let (filename, pos) = extractFilenameAndPosition l
    in printFilenameAndPosWithSwitch s filename pos
       ++ appendLevelTag level e
       ++ "Found 'do' notation at " ++ prettyLoc l ++ "."
prettyErrorWithInfoSwitchAndLevel s level e@(SyntaxError l err) =
    let (filename, pos) = extractFilenameAndPosition l
        msg = if pragmaErrorMsg err
              then ""
              else " (" ++ err ++ ")"
    in printFilenameAndPosWithSwitch s filename pos
       ++ appendLevelTag level e
       ++ "Syntax Error" ++ msg ++ " at " ++ prettyLoc l ++ "."
prettyErrorWithInfoSwitchAndLevel s level e@(InvalidTest l t) =
    let (filename, pos) = extractFilenameAndPosition l
    in printFilenameAndPosWithSwitch s filename pos
       ++ appendLevelTag level e
       ++ "Invalid Test \"" ++ t ++ "\" at line " ++ prettyLineNum l ++ "."
prettyErrorWithInfoSwitchAndLevel s level e@(Pragma l name) =
    let (filename, pos) = extractFilenameAndPosition l
    in printFilenameAndPosWithSwitch s filename pos
       ++ appendLevelTag level e
       ++ "Use of Pragma \"" ++ name ++ "\" at line " ++ prettyLineNum l ++ "."

pragmaErrorMsg :: String -> Bool
pragmaErrorMsg msg = foldr (\w b -> (w == "pragma") || b) False (words msg)

appendLevelTag :: Maybe Level -> Error SrcSpanInfo -> String
appendLevelTag Nothing _ = ""
appendLevelTag (Just l) e = case appendLevelErrorTag' e of
                                "" -> ""
                                t  -> t ++ levelString l ++ ". "
    where appendLevelErrorTag' :: Error SrcSpanInfo -> String
          appendLevelErrorTag' (HigherOrder _ ) =
              "Higher-Order functions are not allowed on " --level...
          appendLevelErrorTag' (LambdaFunction _) =
              "Lambda functions are not allowed on " --level...
          appendLevelErrorTag'(Shadowing _) =
              "Shadowing of variables is not allowed on " --level...
          appendLevelErrorTag' (NoTypeDef _) =
              "Type signatures are mandatory on " --level...
          appendLevelErrorTag' (Imported _) =
              "Importing of modules is forbidden on " --level...
          appendLevelErrorTag' (ModuleHeadUsed _) =
              "Declaring a module is not allowed on " --level...
          appendLevelErrorTag' (OwnDataDecl _) =
              "Data type declarations are not allowed on " --level...
          appendLevelErrorTag' (DoUsed _) =
              "Usage of the do construct is forbidden on " --level...
          appendLevelErrorTag' (Pragma _ _) =
              "Usage of Pragma is forbidden on " --level...
          appendLevelErrorTag' _ = ""
          levelString :: Level -> String
          levelString Level1    = "Level 1"
          levelString Level2    = "Level 2"
          levelString Level3    = "Level 3"
          levelString LevelFull = "Level full"

extractPositionFromQname :: QName l -> l
extractPositionFromQname (Qual l _ _) = l
extractPositionFromQname (UnQual l _) = l

getNameOfQName :: QName l -> String
getNameOfQName (Qual _ _ name) = nameString name
getNameOfQName (UnQual _ name) = nameString name

prettyNameLoc :: Name SrcSpanInfo -> String
prettyNameLoc (Ident l _)  = prettyLoc l
prettyNameLoc (Symbol l _) = prettyLoc l

prettyModNameLoc :: ModuleName SrcSpanInfo -> String
prettyModNameLoc (ModuleName l _) = prettyLoc l

prettyLoc :: SrcSpanInfo -> String
prettyLoc (SrcSpanInfo ispan ipoints) = prettySrcSpan ispan

prettySrcSpan :: SrcSpan -> String
prettySrcSpan (SrcSpan _ sl sc el ec) = pretty sl sc ++ " - " ++ pretty el ec
  where pretty x y = "(" ++ show x ++ ":" ++ show y ++ ")"

prettySims :: [Name SrcSpanInfo] -> String
prettySims [] = ""
prettySims ns = "Did you mean " ++ prettySims' ns ++ "?"
  where
    prettySims' [n] = prettyPrintQ n
    prettySims' names@(_:_) = intercalate ", " (map prettyPrintQ (init names))
                              ++ " or " ++ prettyPrintQ (last names)

prettyPrintQ :: Pretty a => a -> String
prettyPrintQ x = "'" ++ prettyPrint x ++ "'"

-- Some helperfunctions
extractFilenameAndPositionFromQName :: QName SrcSpanInfo -> (Filename, Position)
extractFilenameAndPositionFromQName (Qual l _ _) = extractFilenameAndPosition l
extractFilenameAndPositionFromQName (UnQual l _) = extractFilenameAndPosition l

extractFilenameAndPositionFromName :: Name SrcSpanInfo -> (Filename, Position)
extractFilenameAndPositionFromName (Ident info _) =
    extractFilenameAndPosition info
extractFilenameAndPositionFromName (Symbol info _) =
    extractFilenameAndPosition info

extractFilenameAndPositionFromModuleName :: ModuleName SrcSpanInfo
                                            -> (Filename, Position)
extractFilenameAndPositionFromModuleName (ModuleName info _) =
    extractFilenameAndPosition info

extractFilenameAndPosition :: SrcSpanInfo -> (Filename, Position)
extractFilenameAndPosition (SrcSpanInfo infoSpan _) =
    let filename = extractFileName infoSpan
        startPos = extractStartPosition infoSpan
    in (filename, startPos)

extractFileName :: SrcSpan -> Filename
extractFileName (SrcSpan filename _ _ _ _) = filename

extractStartPosition :: SrcSpan -> Position
extractStartPosition (SrcSpan _ line column lineE columnE) =
    (line, column, lineE, columnE)

prettyLineNum :: SrcSpanInfo -> String
prettyLineNum (SrcSpanInfo (SrcSpan _ sl _ _ _) _) = show sl
