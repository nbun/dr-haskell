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

printFilenameAndPos :: Filename -> Position -> String
printFilenameAndPos filename pos =
    let (line, column, _, _) = pos
    in filename ++ ":" ++ show line ++ ":" ++ show column ++ ":\n"

printFilenameAndPosWithSwitch :: Bool -> Filename -> Position -> String
printFilenameAndPosWithSwitch False _ _ = ""
printFilenameAndPosWithSwitch True  f p = printFilenameAndPos f p

prettyError :: Error SrcSpanInfo -> String
prettyError = prettyErrorWithInfoSwith False

prettyErrorForLint :: Error SrcSpanInfo -> String
prettyErrorForLint = prettyErrorWithInfoSwith True

prettyErrorWithInfoSwith :: Bool -> Error SrcSpanInfo -> String
prettyErrorWithInfoSwith s (NoFunDef name sims) =
    let (filename, pos) = extractFilenameAndPositionFromName name
    in printFilenameAndPosWithSwitch s filename pos
       ++ "Type signature for " ++ prettyPrintQ name ++ " at "
       ++ prettyNameLoc name ++ " without a definition.\n" ++ prettySims sims
prettyErrorWithInfoSwith s (Undefined name sims) =
    let (filename, pos) = extractFilenameAndPositionFromName name
    in printFilenameAndPosWithSwitch s filename pos
       ++ "Undefined identifier " ++ prettyPrintQ name ++ " at "
       ++ prettyNameLoc name ++ ".\n" ++ prettySims sims
prettyErrorWithInfoSwith s (Duplicated name entity maymod) =
    let (filename, pos) = extractFilenameAndPositionFromName name
    in printFilenameAndPosWithSwitch s filename pos
       ++ show entity ++ prettyPrintQ name ++ " at " ++ prettyNameLoc name
       ++ " is already defined" ++ mod ++ "."
    where mod = case maymod of
                  Just mname -> " in module " ++ prettyPrintQ mname
                  Nothing    -> ""
prettyErrorWithInfoSwith s (TypeVarApplication name) =
    let (filename, pos) = extractFilenameAndPositionFromName name
    in printFilenameAndPosWithSwitch s filename pos
       ++ "Type variable " ++ prettyPrintQ name ++ " at " ++ prettyNameLoc name
       ++ " cannot be applied to another type."
prettyErrorWithInfoSwith s (HigherOrder position) =
    let (filename, pos) = extractFilenameAndPosition position
    in printFilenameAndPosWithSwitch s filename pos
       ++ "HigherOrder function located at " ++ prettyLoc position ++ "."
prettyErrorWithInfoSwith s (LambdaFunction position) =
    let (filename, pos) = extractFilenameAndPosition position
    in printFilenameAndPosWithSwitch s filename pos
       ++ "Lambda function located at " ++ prettyLoc position ++ "."
prettyErrorWithInfoSwith s (NoTypeDef name) =
    let (filename, pos) = extractFilenameAndPositionFromName name
    in printFilenameAndPosWithSwitch s filename pos
       ++ "No TypeSignature for function named " ++ prettyPrintQ name ++ " at "
       ++ prettyNameLoc name ++ "."
prettyErrorWithInfoSwith s (Shadowing qname) =
    let (filename, pos) = extractFilenameAndPositionFromQName qname
    in printFilenameAndPosWithSwitch s filename pos
       ++ "Found shadowing of variable " ++ getNameOfQName qname ++ " at "
       ++ prettyLoc (extractPositionFromQname qname) ++ "."
prettyErrorWithInfoSwith s (TypeVar name) =
    let (filename, pos) = extractFilenameAndPositionFromName name
    in printFilenameAndPosWithSwitch s filename pos
       ++ "Found typevariable " ++ prettyPrintQ name ++ " at "
       ++ prettyNameLoc name ++ "."
prettyErrorWithInfoSwith s (Imported name) =
    let (filename, pos) = extractFilenameAndPositionFromModuleName name
    in printFilenameAndPosWithSwitch s filename pos
       ++ "Found import " ++ prettyPrintQ name ++ " at "
       ++ prettyModNameLoc name ++ "."
prettyErrorWithInfoSwith s (ModuleHeadUsed name) =
    let (filename, pos) = extractFilenameAndPositionFromModuleName name
    in printFilenameAndPosWithSwitch s filename pos
       ++ "Found module head " ++ prettyPrintQ name ++ " at "
       ++ prettyModNameLoc name ++ "."
prettyErrorWithInfoSwith s (OwnDataDecl l) =
    let (filename, pos) = extractFilenameAndPosition l
    in printFilenameAndPosWithSwitch s filename pos
       ++ "Found data declaration or type synonym at " ++ prettyLoc l ++ "."
prettyErrorWithInfoSwith s (DoUsed l) =
    let (filename, pos) = extractFilenameAndPosition l
    in printFilenameAndPosWithSwitch s filename pos
       ++ "Found 'do' notation at " ++ prettyLoc l ++ "."
prettyErrorWithInfoSwith s (SyntaxError l e) =
    let (filename, pos) = extractFilenameAndPosition l
    in printFilenameAndPosWithSwitch s filename pos
       ++ "Syntax Error (" ++ e ++ ") at " ++ prettyLoc l ++ "."
prettyErrorWithInfoSwith s (InvalidTest l t) =
    let (filename, pos) = extractFilenameAndPosition l
    in printFilenameAndPosWithSwitch s filename pos
       ++ "Invalid Test \"" ++ t ++ "\" at line " ++ prettyLineNum l ++ "."

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
