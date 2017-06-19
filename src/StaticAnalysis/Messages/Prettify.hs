module StaticAnalysis.Messages.Prettify (module StaticAnalysis.Messages.Prettify) where

import           Data.List
import           Language.Haskell.Exts
import qualified Language.Haskell.Interpreter         as Hint
import           StaticAnalysis.Messages.StaticErrors
import           StaticAnalysis.StaticChecks.Select

type Filename = String
type Position = (Int, Int, Int, Int)

prettyIntError :: Hint.InterpreterError -> String
prettyIntError error =
  case error of
    Hint.UnknownError s -> s
    Hint.WontCompile es -> unlines $ map (\(Hint.GhcError s) -> s) es
    Hint.NotAllowed   s -> s
    Hint.GhcException s -> s

{-
ShadowingTest.hs:16:1:
    Duplicate type signatures for ‘funSig’
    at ShadowingTest.hs:1:1-6
       ShadowingTest.hs:6:1-6
       ShadowingTest.hs:11:1-6
       ShadowingTest.hs:16:1-6

ShadowingTest.hs:17:1:
    Multiple declarations of ‘funSig’
    Declared at: ShadowingTest.hs:2:1
                 ShadowingTest.hs:7:1
                 ShadowingTest.hs:12:1
                 ShadowingTest.hs:17:1
-}

printFilenameAndPos :: Filename -> Position -> String
printFilenameAndPos filename pos =
    let (line, column, _, _) = pos
    in filename ++ ":" ++ show line ++ ":" ++ show column ++ ":\r\n"

prettyError :: Error SrcSpanInfo -> String
prettyError (NoFunDef name sims) =
    let (filename, pos) = extractFilenameAndPositionFromName name
    in printFilenameAndPos filename pos
       ++ "Type signature for " ++ prettyPrintQ name ++ " at "
       ++ prettyNameLoc name ++ " without a definition.\r\n" ++ prettySims sims
prettyError (Undefined name sims) =
    let (filename, pos) = extractFilenameAndPositionFromName name
    in printFilenameAndPos filename pos
       ++ "Undefined identifier " ++ prettyPrintQ name ++ " at "
       ++ prettyNameLoc name ++ ".\r\n" ++ prettySims sims
prettyError (Duplicated name maymod) =
    let (filename, pos) = extractFilenameAndPositionFromName name
    in printFilenameAndPos filename pos
       ++ "Definition " ++ prettyPrintQ name ++ " at " ++ prettyNameLoc name
       ++ " is already defined" ++ mod ++ "."
    where mod = case maymod of
                  Just mname -> " in module " ++ prettyPrintQ mname
                  Nothing    -> ""
prettyError (TypeVarApplication name) =
    let (filename, pos) = extractFilenameAndPositionFromName name
    in printFilenameAndPos filename pos
       ++ "Type variable " ++ prettyPrintQ name ++ " at " ++ prettyNameLoc name
       ++ " cannot be applied to another type."
prettyError (HigherOrder position) =
    let (filename, pos) = extractFilenameAndPosition position
    in printFilenameAndPos filename pos
       ++ "HigherOrder function located at " ++ prettyLoc position ++ "."
prettyError (LambdaFunction position) =
    let (filename, pos) = extractFilenameAndPosition position
    in printFilenameAndPos filename pos
       ++ "Lambda function located at " ++ prettyLoc position ++ "."
prettyError (NoTypeDef name) =
    let (filename, pos) = extractFilenameAndPositionFromName name
    in printFilenameAndPos filename pos
       ++ "No TypeSignature for function named " ++ prettyPrintQ name ++ " at "
       ++ prettyNameLoc name ++ "."
prettyError (Shadowing qname) =
    let (filename, pos) = extractFilenameAndPositionFromQName qname
    in printFilenameAndPos filename pos
       ++ "Found shadowing of variable " ++ getNameOfQName qname ++ " at "
       ++ prettyLoc (extractPositionFromQname qname) ++ "."
prettyError (TypeVar name) =
    let (filename, pos) = extractFilenameAndPositionFromName name
    in printFilenameAndPos filename pos
       ++ "Found typevariable " ++ prettyPrintQ name ++ " at "++ prettyNameLoc name ++ "."
prettyError (Imported name) =
    let (filename, pos) = extractFilenameAndPositionFromModuleName name
    in printFilenameAndPos filename pos
       ++ "Found import " ++ prettyPrintQ name ++ " at " ++ prettyModNameLoc name ++ "."
prettyError (ModuleHeadUsed name) =
    let (filename, pos) = extractFilenameAndPositionFromModuleName name
    in printFilenameAndPos filename pos
       ++ "Found module head " ++ prettyPrintQ name ++ " at " ++ prettyModNameLoc name ++ "."
prettyError (OwnDataDecl l) =
    let (filename, pos) = extractFilenameAndPosition l
    in printFilenameAndPos filename pos
       ++ "Found data declaration or type synonym at " ++ prettyLoc l ++ "."
prettyError (DoUsed l) =
    let (filename, pos) = extractFilenameAndPosition l
    in printFilenameAndPos filename pos
       ++ "Found 'do' notation at " ++ prettyLoc l ++ "."

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
extractFilenameAndPositionFromName (Ident info _) = extractFilenameAndPosition info
extractFilenameAndPositionFromName (Symbol info _) = extractFilenameAndPosition info

extractFilenameAndPositionFromModuleName :: ModuleName SrcSpanInfo -> (Filename, Position)
extractFilenameAndPositionFromModuleName (ModuleName info _) = extractFilenameAndPosition info

extractFilenameAndPosition :: SrcSpanInfo -> (Filename, Position)
extractFilenameAndPosition (SrcSpanInfo infoSpan _) =
    let filename = extractFileName infoSpan
        startPos = extractStartPosition infoSpan
    in (filename, startPos)

extractFileName :: SrcSpan -> Filename
extractFileName (SrcSpan filename _ _ _ _) = filename

extractStartPosition :: SrcSpan -> Position
extractStartPosition (SrcSpan _ line column lineE columnE) = (line, column, lineE, columnE)
