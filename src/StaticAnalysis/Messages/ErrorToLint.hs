-- | Holds everything needed for the Error-Datatype to Lint-Datatype Conversion
-- The Error-Datatype comes from the Haskell-Src-Extensions module
module StaticAnalysis.Messages.ErrorToLint (
    module StaticAnalysis.Messages.ErrorToLint
) where

import           Data.List
import           Data.List.Utils
import           Language.Haskell.Exts
import           StaticAnalysis.Level
import           StaticAnalysis.Messages.Prettify
import           StaticAnalysis.Messages.StaticErrors
import           System.Environment
import           System.Exit
import           System.IO
import qualified Text.JSON                            as Json

-- | MessageClass
-- MessageClass from the Lint-Protocol
data MessageClass = Error
                  | Suggestion
                  | Warning
    deriving Show

-- | Message
-- Holds the actual error message
type Message = String

-- | Lint-Datatype
-- Holds every information needed by the LintOutputGenerators
data Lint = Lint Filename Position MessageClass Message

-- | LinterOutput
-- Identifier for the OutputGenerators
data LinterOutput = JSON
                  | PLAIN
plain :: LinterOutput
plain = PLAIN

json :: LinterOutput
json = JSON

-- | lintErrors generates from an list of Errors the specified LintOutput
lintErrors :: LinterOutput -> Maybe Level -> [Error SrcSpanInfo] -> String
lintErrors = lintErrorHlint []

-- | Basically the same as lintErrors but with preconverted HLint Output.
-- This is mostly used by the LinterImplementation.
lintErrorHlint :: [Lint] -> LinterOutput -> Maybe Level -> [Error SrcSpanInfo] -> String
lintErrorHlint lints JSON l es =
    let lintedErrors = map (transformError l) es
    in Json.showJSArray (map buildJson (lintedErrors ++ lints)) ""
lintErrorHlint lints PLAIN l es =
    let lintedErrors = map (transformError l) es
        out = map lintPlain (lintedErrors ++ lints)
    in foldr (\x xs -> x ++ "\n\n" ++ xs) "" out

-- | Builds the Lint-Datatype for an Error specified by its ErrorName.
-- Also allows the pass-through of a messageclass
buildForName :: Name SrcSpanInfo -> Error SrcSpanInfo
                                 -> MessageClass
                                 -> Maybe Level -> Lint
buildForName name e messageClass l =
    let (filename, position) = extractFilenameAndPositionFromName name
        message = prettyErrorWithInfoSwitchAndLevel False l e
    in Lint filename position messageClass message

-- | Builds the Lint-Datatype for an Error specified by its
--   ErrorPositionInformation.
--   Also allows the pass-through of a messageclass
buildForInfo :: SrcSpanInfo -> Error SrcSpanInfo
                            -> MessageClass
                            -> Maybe Level -> Lint
buildForInfo info e messageClass l =
    let (filename, position) = extractFilenameAndPosition info
        message = prettyErrorWithInfoSwitchAndLevel False l e
    in Lint filename position messageClass message

-- | Builds the Lint-Datatype for an Error specified by its ErrorQName
-- Also allows the pass-through of a messageclass
buildForQName :: QName SrcSpanInfo -> Error SrcSpanInfo
                                   -> MessageClass
                                   -> Maybe Level -> Lint
buildForQName qname e messageClass l =
    let (filename, position) = extractFilenameAndPositionFromQName qname
        message = prettyErrorWithInfoSwitchAndLevel False l e
    in Lint filename position messageClass message

-- | Builds the Lint-Datatype for an Error specified by its ModuleName
-- Also allows the pass-through of a messageclass
buildForModuleName :: ModuleName SrcSpanInfo -> Error SrcSpanInfo
                                             -> MessageClass
                                             -> Maybe Level -> Lint
buildForModuleName mname e messageClass l =
    let (filename, position) = extractFilenameAndPositionFromModuleName mname
        message = prettyErrorWithInfoSwitchAndLevel False l e
    in Lint filename position messageClass message

-- | Builds the Lint-Datatype for an UnknownError
-- Also allows the pass-through of a messageclass
buildUnknownError :: Error SrcSpanInfo -> MessageClass -> Maybe Level -> Lint
buildUnknownError e  messageClass l =
    let (filename, position) = ("", (-1, -1, -1, -1))
        message = prettyErrorWithInfoSwitchAndLevel False l e
    in Lint filename position messageClass message

buildParseError :: SrcLoc -> String -> Lint
buildParseError loc message =
  Lint (srcFilename loc) position Error ("Syntax Error" ++ msg')
  where
    msg' = if pragmaErrorMsg message
              then ""
              else " (" ++ message ++ ")"
    lpos = srcLine loc
    cpos = srcColumn loc
    position = (lpos, cpos, lpos, cpos + 1)

-- | Transforms every known Error via the previpusly specified functions into
-- the Lint-Datatype
transformError :: Maybe Level -> Error SrcSpanInfo -> Lint
transformError l e@(NoFunDef name _)           = buildForName name e Error l
transformError l e@(Undefined name _)          = buildForName name e Error l
transformError l e@(Duplicated name _ _)       = buildForName name e Error l
transformError l e@(TypeVarApplication name)   = buildForName name e Error l
transformError l e@(HigherOrder info)          = buildForInfo info e Error l
transformError l e@(LambdaFunction info)       = buildForInfo info e Error l
transformError l e@(NoTypeDef name)            = buildForName name e Error l
transformError l e@(Shadowing qname)           = buildForQName qname e Error l
transformError l e@(TypeVar name)              = buildForName name e Error l
transformError l e@(Imported moduleName)       = buildForModuleName moduleName
                                                                  e
                                                                  Error l
transformError l e@(ModuleHeadUsed moduleName) = buildForModuleName moduleName
                                                                  e
                                                                  Error l
transformError l e@(OwnDataDecl info)          = buildForInfo info e Error l
transformError l e@(DoUsed info)               = buildForInfo info e Error l
transformError l e@(Pragma info _)             = buildForInfo info e Error l
transformError l e@(TypeError info _)          = buildForInfo info e Error l
transformError l e@(InstanceDecl info)             = buildForInfo info e Error l
transformError l e@(TypeClassDecl info)            = buildForInfo info e Error l
transformError l e                             = buildUnknownError e Warning l

-- TODO: replace tmp path in filename if needed
-- | Plain Lintoutput Generator
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

-- TODO: replace tmp path in filename if needed
-- | Json Lintoutput Generator
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

-- | Helperfunction to Convert a String into its Json Representation
--   inside Haskell
toJSString :: String -> Json.JSValue
toJSString = Json.JSString . Json.toJSString

manipulatePathWithHostvar :: String -> IO String
manipulatePathWithHostvar file = do
    hostpath <- lookupEnv "DRHASKELLHOSTPATH"
    case hostpath of
        Nothing   -> return file
        Just path -> return $ replace path "/tmp/drhaskell-src" file


manipulatePathWithHostvarREV :: String -> IO String
manipulatePathWithHostvarREV file = do
    hostpath <- lookupEnv "DRHASKELLHOSTPATH"
    case hostpath of
        Nothing   -> return file
        Just path -> return $ replace path "/tmp/drhaskell-src" file
