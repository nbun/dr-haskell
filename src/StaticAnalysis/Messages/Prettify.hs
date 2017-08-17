-- | Pretty printing of errors
module StaticAnalysis.Messages.Prettify
  (Filename, Position
  , prettyErrorForLintWithLevel, prettyErrorWithInfoSwitchAndLevel
  , extractFilenameAndPositionFromModuleName, extractFilenameAndPositionFromName
  , extractFilenameAndPosition, extractFilenameAndPositionFromQName, pragmaErrorMsg
  , extractStartPosition
  ) where

import Data.List                            (intercalate)
import Language.Haskell.Exts                (ModuleName (..), Name (..), Pretty,
                                             QName (..), SrcSpan (..),
                                             SrcSpanInfo (..), prettyPrint)

import StaticAnalysis.Level                 (Level (..))
import StaticAnalysis.Messages.StaticErrors (Error (..))
import StaticAnalysis.StaticChecks.Select   (getNameOfQName)

import TypeInference.AbstractHaskell        (defaultAHOptions)
import TypeInference.Main                   (showTIError)

-- | An ordinary filename
type Filename = String

-- | Consists of a line, column, line end and column end value
type Position = (Int, Int, Int, Int)

-- | Prettyprints an error with positional information
prettyErrorForLint :: Error SrcSpanInfo -> String
prettyErrorForLint = prettyErrorWithInfoSwitch True

-- | Prettyprints an error with or without positional information
prettyErrorWithInfoSwitch :: Bool -> Error SrcSpanInfo -> String
prettyErrorWithInfoSwitch s = prettyErrorWithInfoSwitchAndLevel s Nothing

-- | Prettyprints an error with level information
prettyErrorForLintWithLevel :: Maybe Level -> Error SrcSpanInfo -> String
prettyErrorForLintWithLevel = prettyErrorWithInfoSwitchAndLevel True

-- | Prettyprints an error with positional and level information
prettyErrorWithInfoSwitchAndLevel :: Bool -> Maybe Level
                                          -> Error SrcSpanInfo
                                          -> String
prettyErrorWithInfoSwitchAndLevel s level e =
  case e of
    NoFunDef name sims ->
      infoLine name
      ++ appendLevelTag level e
      ++ "Type signature for " ++ prettyPrintQ name ++ " at "
      ++ prettyNameLoc name ++ " without a definition.\n" ++ prettySims sims
    Undefined name sims ->
      infoLine name
      ++ appendLevelTag level e
      ++ "Undefined identifier " ++ prettyPrintQ name ++ " at "
      ++ prettyNameLoc name ++ ".\n" ++ prettySims sims
    Duplicated name entity maymod ->
      infoLine name
      ++ appendLevelTag level e
      ++ show entity ++ prettyPrintQ name ++ " at " ++ prettyNameLoc name
      ++ " is already defined" ++ mod ++ "."
      where mod = case maymod of
                    Just mname -> " in module " ++ prettyPrintQ mname
                    Nothing    -> ""
    TypeVarApplication name ->
      infoLine name
      ++ appendLevelTag level e
      ++ "Type variable " ++ prettyPrintQ name ++ " at " ++ prettyNameLoc name
      ++ " cannot be applied to another type."
    HigherOrder position ->
      infoLinePos position
      ++ appendLevelTag level e
      ++ "HigherOrder function located at " ++ prettyLoc position ++ "."
    LambdaFunction position ->
      infoLinePos position
      ++ appendLevelTag level e
      ++ "Lambda function located at " ++ prettyLoc position ++ "."
    NoTypeDef name ->
      infoLine name
      ++ appendLevelTag level e
      ++ "No TypeSignature for function named " ++ prettyPrintQ name ++ " at "
      ++ prettyNameLoc name ++ "."
    Shadowing qname ->
      infoLineQ qname
      ++ appendLevelTag level e
      ++ "Found shadowing of variable " ++ getNameOfQName qname ++ " at "
      ++ prettyLoc (extractPositionFromQname qname) ++ "."
    TypeVar name ->
      infoLine name
      ++ appendLevelTag level e
      ++ "Found typevariable " ++ prettyPrintQ name ++ " at "
      ++ prettyNameLoc name ++ "."
    Imported name ->
      infoLineMod name
      ++ appendLevelTag level e
      ++ "Found import " ++ prettyPrintQ name ++ " at "
      ++ prettyModNameLoc name ++ "."
    ModuleHeadUsed name ->
      infoLineMod name
         ++ appendLevelTag level e
         ++ "Found module head " ++ prettyPrintQ name ++ " at "
         ++ prettyModNameLoc name ++ "."
    OwnDataDecl l  ->
      infoLinePos l
      ++ appendLevelTag level e
      ++ "Found data declaration or type synonym at " ++ prettyLoc l ++ "."
    DoUsed l ->
      infoLinePos l
      ++ appendLevelTag level e
      ++ "Found 'do' notation at " ++ prettyLoc l ++ "."
    SyntaxError l err ->
      infoLinePos l
      ++ appendLevelTag level e
      ++ "Syntax Error" ++ msg ++ " at " ++ prettyLoc l ++ "."
      where msg = if pragmaErrorMsg err
                    then ""
                    else " (" ++ err ++ ")"
    InvalidTest l t ->
      infoLinePos l
      ++ appendLevelTag level e
      ++ "Invalid Test \"" ++ t ++ "\" at line " ++ prettyLineNum l ++ "."
    Pragma l name ->
      infoLinePos l
      ++ appendLevelTag level e
      ++ "Use of Pragma \"" ++ name ++ "\" at line " ++ prettyLineNum l ++ "."
    TypeError l tiError ->
      infoLinePos l
      ++ appendLevelTag level e
      ++ showTIError defaultAHOptions tiError
  where infoLine name =
          let (filename, pos) = extractFilenameAndPositionFromName name
          in printFilenameAndPosWithSwitch s filename pos
        infoLinePos position =
          let (filename, pos) = extractFilenameAndPosition position
          in printFilenameAndPosWithSwitch s filename pos
        infoLineQ qname =
          let (filename, pos) = extractFilenameAndPositionFromQName qname
          in printFilenameAndPosWithSwitch s filename pos
        infoLineMod modname =
          let (filename, pos) = extractFilenameAndPositionFromModuleName modname
          in printFilenameAndPosWithSwitch s filename pos

-- | Returns a warning depending on the level for an error
appendLevelTag :: Maybe Level -> Error SrcSpanInfo -> String
appendLevelTag Nothing _ = ""
appendLevelTag (Just l) e = case appendLevelErrorTag' e of
                              "" -> ""
                              t  -> t ++ levelString l ++ ". "
  where
    appendLevelErrorTag' :: Error SrcSpanInfo -> String
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

-- | Prettyprints SrcSpanInfo
prettyLoc :: SrcSpanInfo -> String
prettyLoc (SrcSpanInfo ispan ipoints) = prettySrcSpan ispan

-- | Prettyprints a name's positional information
prettyNameLoc :: Name SrcSpanInfo -> String
prettyNameLoc (Ident l _)  = prettyLoc l
prettyNameLoc (Symbol l _) = prettyLoc l

-- | Prettyprints a module name's positional information
prettyModNameLoc :: ModuleName SrcSpanInfo -> String
prettyModNameLoc (ModuleName l _) = prettyLoc l

-- | Prettyprints SrcSpan
prettySrcSpan :: SrcSpan -> String
prettySrcSpan (SrcSpan _ sl sc el ec) = pretty sl sc ++ " - " ++ pretty el ec
  where pretty x y = "(" ++ show x ++ ":" ++ show y ++ ")"

-- | Prettyprints a list of similar names as a suggestion
prettySims :: [Name SrcSpanInfo] -> String
prettySims [] = ""
prettySims ns = "Did you mean " ++ prettySims' ns ++ "?"
  where
    prettySims' [n] = prettyPrintQ n
    prettySims' names@(_:_) = intercalate ", " (map prettyPrintQ (init names))
                              ++ " or " ++ prettyPrintQ (last names)

-- | Prettyprints something in quotes
prettyPrintQ :: Pretty a => a -> String
prettyPrintQ x = "'" ++ prettyPrint x ++ "'"

-- | Prettyprints a line number
prettyLineNum :: SrcSpanInfo -> String
prettyLineNum (SrcSpanInfo (SrcSpan _ sl _ _ _) _) = show sl

--------------------------------------------------------------------------------
-- Helper functions

-- | Is an error message suggesting the use of a pragma?
pragmaErrorMsg :: String -> Bool
pragmaErrorMsg msg = foldr (\w b -> (w == "pragma") || b) False (words msg)

-- | Prints a filename + position block in ghc format
printFilenameAndPos :: Filename -> Position -> String
printFilenameAndPos filename pos =
    let (line, column, _, _) = pos
    in filename ++ ":" ++ show line ++ ":" ++ show column ++ ":\n"

-- | Prints or omits filename + position block
printFilenameAndPosWithSwitch :: Bool -> Filename -> Position -> String
printFilenameAndPosWithSwitch False _ _ = ""
printFilenameAndPosWithSwitch True  f p = printFilenameAndPos f p

-- | Extracts the position from a qualified name
extractPositionFromQname :: QName l -> l
extractPositionFromQname (Qual l _ _) = l
extractPositionFromQname (UnQual l _) = l

-- | Extracts filename and position from a qualified name
extractFilenameAndPositionFromQName :: QName SrcSpanInfo -> (Filename, Position)
extractFilenameAndPositionFromQName (Qual l _ _) = extractFilenameAndPosition l
extractFilenameAndPositionFromQName (UnQual l _) = extractFilenameAndPosition l

-- | Extracts filename and position from a name
extractFilenameAndPositionFromName :: Name SrcSpanInfo -> (Filename, Position)
extractFilenameAndPositionFromName (Ident info _) =
    extractFilenameAndPosition info
extractFilenameAndPositionFromName (Symbol info _) =
    extractFilenameAndPosition info

-- | Extracts filename and position from a module name
extractFilenameAndPositionFromModuleName :: ModuleName SrcSpanInfo
                                            -> (Filename, Position)
extractFilenameAndPositionFromModuleName (ModuleName info _) =
    extractFilenameAndPosition info

-- | Extracts filename and position from a SrcSpanInfo
extractFilenameAndPosition :: SrcSpanInfo -> (Filename, Position)
extractFilenameAndPosition (SrcSpanInfo infoSpan _) =
    let filename = extractFileName infoSpan
        startPos = extractStartPosition infoSpan
    in (filename, startPos)

-- | Extracts the filename from a SrcSpan
extractFileName :: SrcSpan -> Filename
extractFileName (SrcSpan filename _ _ _ _) = filename

-- | Extracts the start position from a SrcSpan
extractStartPosition :: SrcSpan -> Position
extractStartPosition (SrcSpan _ line column lineE columnE) =
    (line, column, lineE, columnE)
