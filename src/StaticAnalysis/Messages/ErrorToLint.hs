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
lintErrors PLAIN es = foldr (\x xs -> x ++ "\r\n" ++ xs) "" $ map (lintPlain . transformError) es

transformError :: Error SrcSpanInfo -> Lint
transformError e@(NoFunDef name _) =
    let (filename, position) = extractFilenameAndPositionFromName name
        messageClass = Error
        message = prettyError e
    in Lint filename position messageClass message
transformError e@(Undefined name _ _) =
    let (filename, position) = extractFilenameAndPositionFromName name
        messageClass = Error
        message = prettyError e
    in Lint filename position messageClass message
transformError e@(Duplicated name _) =
    let (filename, position) = extractFilenameAndPositionFromName name
        messageClass = Error
        message = prettyError e
    in Lint filename position messageClass message
transformError e@(TypeVarApplication name) =
    let (filename, position) = extractFilenameAndPositionFromName name
        messageClass = Error
        message = prettyError e
    in Lint filename position messageClass message
transformError e@(HigherOrder info) =
    let (filename, position) = extractFilenameAndPosition info
        messageClass = Error
        message = prettyError e
    in Lint filename position messageClass message
transformError e@(LambdaFunction info) =
    let (filename, position) = extractFilenameAndPosition info
        messageClass = Error
        message = prettyError e
    in Lint filename position messageClass message
transformError e@(NoTypeDef name) =
    let (filename, position) = extractFilenameAndPositionFromName name
        messageClass = Error
        message = prettyError e
    in Lint filename position messageClass message
transformError e@(Shadowing qname) =
    let (filename, position) = extractFilenameAndPositionFromQName qname
        messageClass = Error
        message = prettyError e
    in Lint filename position messageClass message
transformError e@(TypeVar name) =
    let (filename, position) = extractFilenameAndPositionFromName name
        messageClass = Error
        message = prettyError e
    in Lint filename position messageClass message

extractFilenameAndPositionFromQName :: QName SrcSpanInfo -> (Filename, Position)
extractFilenameAndPositionFromQName (Qual l _ _) = extractFilenameAndPosition l
extractFilenameAndPositionFromQName (UnQual l _) = extractFilenameAndPosition l

extractFilenameAndPositionFromName :: Name SrcSpanInfo -> (Filename, Position)
extractFilenameAndPositionFromName (Ident info _) = extractFilenameAndPosition info
extractFilenameAndPositionFromName (Symbol info _) = extractFilenameAndPosition info

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

buildJson :: Lint -> Json.JSValue
buildJson (Lint filename position messageClass message) =
    let obj = [("filename", toJSString $ show filename),
               ("line", toJSString $ show $ fst position),
               ("column", toJSString $ show $ snd position),
               ("messsageclass", toJSString $ show messageClass),
               ("message", toJSString $ message)]
    in Json.JSObject $ Json.toJSObject obj

toJSString :: String -> Json.JSValue
toJSString = Json.JSString . Json.toJSString
