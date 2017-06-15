module StaticAnalysis.Messages.Prettify where

import           Control.Monad.Catch
import           Data.List
import           Language.Haskell.Exts
import qualified Language.Haskell.Interpreter         as Hint
import           StaticAnalysis.Messages.StaticErrors
import           StaticAnalysis.StaticChecks.Select

prettyError :: Error SrcSpanInfo -> String
prettyError (NoFunDef name sims) =
    "Type signature for " ++ prettyPrintQ name ++ " at " ++ prettyNameLoc name
    ++ " without a definition. " ++ prettySims sims
prettyError (Undefined name sims) =
    "Undefined identifier " ++ prettyPrintQ name ++ " at " ++ prettyNameLoc name
    ++ ". " ++ prettySims sims
prettyError (Duplicated name maymod) =
    "Definition " ++ prettyPrintQ name ++ " at " ++ prettyNameLoc name
    ++ " is already defined" ++ mod ++ "."
    where mod = case maymod of
                  Just mname -> " in module " ++ prettyPrintQ mname
                  Nothing    -> ""
prettyError (TypeVarApplication name) =
    "Type variable " ++ prettyPrintQ name ++ " at " ++ prettyNameLoc name
    ++ " cannot be applied to another type."
prettyError (HigherOrder pos) =
    "HigherOrder function located at " ++ prettyLoc pos
prettyError (LambdaFunction pos) =
    "Lambda function located at " ++ prettyLoc pos
prettyError (NoTypeDef name) =
    "No TypeSignature for function named " ++ prettyPrintQ name ++ " at " ++ prettyNameLoc name
prettyError (Shadowing qname) =
    "Found shadowing of variable " ++ getNameOfQName qname ++ " at " ++ prettyLoc (extractPositionFromQname qname)
prettyError (TypeVar name) =
    "Found typevariable " ++ prettyPrintQ name ++ " at "++ prettyNameLoc name
prettyError (Imported name) =
  "Found import " ++ prettyPrintQ name ++ " at " ++ prettyModNameLoc name
prettyError (ModuleHeadUsed name) =
  "Found module head " ++ prettyPrintQ name ++ " at " ++ prettyModNameLoc name
prettyError (OwnDataDecl l) =
  "Found data declaration or type synonym at " ++ prettyLoc l
prettyError (DoUsed l) =
  "Found 'do' notation at " ++ prettyLoc l
prettyError (GHCError e) = displayException e

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
