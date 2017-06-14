module StaticAnalysis.Messages.ErrorToLint (
    lintErrors,
    plain,
    json
) where
{-
1) Transform StaticError Datatype into Lint Datatype
2) Choose Linteroutput via LinterOutput Enum
3) Outputs linted errors defined by linterfunctions
-}

import           Language.Haskell.Exts
import           StaticAnalysis.Messages.Prettify
import           StaticAnalysis.Messages.StaticErrors
import qualified Text.JSON                            as Json

-- LinterDataType
data MessageClass = Error
                  | Suggestion
                  | Warning
    deriving Show
type Filename = String
type Position = (Int, Int)
type Message = String
data Lint = Lint Filename Position MessageClass Message

-- LinterOutput Enum
data LinterOutput = JSON
                  | PLAIN

plain :: LinterOutput
plain = PLAIN

json :: LinterOutput
json = JSON

lintErrors :: LinterOutput -> [Error SrcSpanInfo] -> String
lintErrors JSON es  = (Json.showJSArray $ map (buildJson . transformError) es) ""
lintErrors PLAIN es = foldr ((\x xs -> x ++ "\r\n" ++ xs) . lintPlain . transformError) "" es

buildForName :: Name SrcSpanInfo -> Error SrcSpanInfo -> Lint
buildForName name e =
    let (filename, position) = extractFilenameAndPositionFromName name
        messageClass = Error
        message = prettyError e
    in Lint filename position messageClass message

buildForInfo :: SrcSpanInfo -> Error SrcSpanInfo -> Lint
buildForInfo info e =
    let (filename, position) = extractFilenameAndPosition info
        messageClass = Error
        message = prettyError e
    in Lint filename position messageClass message

buildForQName :: QName SrcSpanInfo -> Error SrcSpanInfo -> Lint
buildForQName qname e =
    let (filename, position) = extractFilenameAndPositionFromQName qname
        messageClass = Error
        message = prettyError e
    in Lint filename position messageClass message

buildForModuleName :: ModuleName SrcSpanInfo -> Error SrcSpanInfo -> Lint
buildForModuleName mname e =
    let (filename, position) = extractFilenameAndPositionFromModuleName mname
        messageClass = Error
        message = prettyError e
    in Lint filename position messageClass message

transformError :: Error SrcSpanInfo -> Lint
transformError e@(NoFunDef name _)           = buildForName name e
transformError e@(Undefined name _ _)        = buildForName name e
transformError e@(Duplicated name _)         = buildForName name e
transformError e@(TypeVarApplication name)   = buildForName name e
transformError e@(HigherOrder info)          = buildForInfo info e
transformError e@(LambdaFunction info)       = buildForInfo info e
transformError e@(NoTypeDef name)            = buildForName name e
transformError e@(Shadowing qname)           = buildForQName qname e
transformError e@(TypeVar name)              = buildForName name e
transformError e@(Imported moduleName)       = buildForModuleName moduleName e
transformError e@(ModuleHeadUsed moduleName) = buildForModuleName moduleName e
transformError e@(OwnDataDecl info)          = buildForInfo info e

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
extractStartPosition (SrcSpan _ line column _ _) = (line, column)

lintPlain :: Lint -> String
lintPlain (Lint filename position messageClass message) =
    filename
    ++ ":"
    ++ show (fst position)
    ++ ":"
    ++ show (snd position)
    ++ ":"
    ++ " "
    ++ show messageClass
    ++ ":"
    ++ " "
    ++ message

{-
{
    "module": "Main",
    "decl": "run",
    "severity": "Suggestion",
    "hint": "Redundant bracket",
    "file": "/Volumes/ExpandDrive/Dropbox/UniKiel/MASemester2/MPPS/Repo/src/DrHaskellLint.hs",
    "startLine":21,
    "startColumn ":22,
    "endLine ":21,
    "endColumn ":74,
    "from ":" (runChecksL1 file) >>= (putStrLn.lintErrors plain) ",
    "to ":
    "runChecksL1 file >>= (putStrLn . lintErrors plain)",
    "note": [],
    "refactorings": "[Replace {rtype = Expr, pos = SrcSpan {startLine = 21, startCol = 22, endLine = 21, endCol = 40 }, subts = [(\"x\",SrcSpan {startLine = 21, startCol = 23, endLine = 21, endCol = 39})], orig = \"x\"}]"
    },
-}

buildJson :: Lint -> Json.JSValue
buildJson (Lint filename position messageClass message) =
    let obj = [("file", toJSString $ show filename),
               ("startLine", toJSString $ show $ fst position),
               ("startColumn", toJSString $ show $ snd position),
               ("severity", toJSString $ show messageClass),
               ("hint", toJSString message)]
    in Json.JSObject $ Json.toJSObject obj

toJSString :: String -> Json.JSValue
toJSString = Json.JSString . Json.toJSString
