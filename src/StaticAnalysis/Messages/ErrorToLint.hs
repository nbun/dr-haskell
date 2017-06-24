module StaticAnalysis.Messages.ErrorToLint (module StaticAnalysis.Messages.ErrorToLint) where
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
lintErrors = lintErrorHlint []

lintErrorHlint :: [Lint] -> LinterOutput -> [Error SrcSpanInfo] -> String
lintErrorHlint lints JSON es =
    let lintedErrors = map transformError es
    in Json.showJSArray (map buildJson (lintedErrors ++ lints)) ""
lintErrorHlint lints PLAIN es =
    let lintedErrors = map transformError es
        out = map lintPlain (lintedErrors ++ lints)
    in foldr (\x xs -> x ++ "\r\n" ++ xs) "" out

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


buildUnknownError :: Error SrcSpanInfo -> Lint
buildUnknownError e =
    let (filename, position) = ("", (-1, -1, -1, -1))
        messageClass = Warning
        message = prettyError e
    in Lint filename position messageClass message

transformError :: Error SrcSpanInfo -> Lint
transformError e@(NoFunDef name _)           = buildForName name e
transformError e@(Undefined name _)          = buildForName name e
transformError e@(Duplicated name _ _)       = buildForName name e
transformError e@(TypeVarApplication name)   = buildForName name e
transformError e@(HigherOrder info)          = buildForInfo info e
transformError e@(LambdaFunction info)       = buildForInfo info e
transformError e@(NoTypeDef name)            = buildForName name e
transformError e@(Shadowing qname)           = buildForQName qname e
transformError e@(TypeVar name)              = buildForName name e
transformError e@(Imported moduleName)       = buildForModuleName moduleName e
transformError e@(ModuleHeadUsed moduleName) = buildForModuleName moduleName e
transformError e@(OwnDataDecl info)          = buildForInfo info e
transformError e@(DoUsed info)               = buildForInfo info e
transformError e                             = buildUnknownError e

lintPlain :: Lint -> String
lintPlain (Lint filename position messageClass message) =
    let (sl, sc, _, _) = position
    in  filename
        ++ ":"
        ++ show sl
        ++ ":"
        ++ show sc
        ++ ":"
        ++ " "
        ++ show messageClass
        ++ ":"
        ++ " "
        ++ message

buildJson :: Lint -> Json.JSValue
buildJson (Lint filename position messageClass message) =
    let (sl, sc, el, ec) = position
        obj = [("file", toJSString $ show filename),
               ("startLine", toJSString $ show sl),
               ("startColumn", toJSString $ show sc),
               ("endLine", toJSString $ show el),
               ("endColumn", toJSString $ show ec),
               ("severity", toJSString $ show messageClass),
               ("hint", toJSString message)]
    in Json.JSObject $ Json.toJSObject obj

toJSString :: String -> Json.JSValue
toJSString = Json.JSString . Json.toJSString
