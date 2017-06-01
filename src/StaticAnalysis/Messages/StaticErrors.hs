module StaticAnalysis.Messages.StaticErrors where

import           Language.Haskell.Exts
import           Data.List

data Error l = NoFunDef (Name l) [Name l]
             --          name,   names in scope
             | Undefined (Name l) [Name l] [String]
             --          name,    names in scope, hints
             | Duplicated (Name l) (Maybe (ModuleName l))
             --           name,    module that contains the duplicate
             | TypeVarApplication (Name l)
  deriving Show

prettyError :: Error SrcSpanInfo -> String
prettyError (NoFunDef name sims) =
    "Type signature for " ++ prettyPrintQ name ++ " at " ++ prettyNameLoc name
    ++ " without a definition. " ++ "Did you mean " ++ prettySims sims ++ "?"
prettyError (Undefined name sims hints) =
    "Undefined identifier " ++ prettyPrintQ name ++ " at " ++ prettyNameLoc name
    ++ ". Did you mean " ++ prettySims sims ++ "?"
prettyError (Duplicated name maymod) =
    "Definition " ++ prettyPrintQ name ++ " at " ++ prettyNameLoc name
    ++ " is already defined" ++ mod ++ "."
    where mod = case maymod of
                  Just mname -> " in module " ++ prettyPrintQ mname
                  Nothing    -> ""
prettyError (TypeVarApplication name) =
  "Type variable " ++ prettyPrintQ name ++ " at " ++ prettyNameLoc name
  ++ " cannot be applied to another type."

prettyNameLoc :: Name SrcSpanInfo -> String
prettyNameLoc (Ident l _)  = prettyLoc l
prettyNameLoc (Symbol l _) = prettyLoc l

prettyLoc :: SrcSpanInfo -> String
prettyLoc (SrcSpanInfo ispan ipoints) = prettySrcSpan ispan

prettySrcSpan :: SrcSpan -> String
prettySrcSpan (SrcSpan _ sl sc el ec) = pretty sl sc ++ " - " ++ pretty el ec
  where pretty x y = "(" ++ show x ++ ":" ++ show y ++ ")"

prettySims :: [Name SrcSpanInfo] -> String
prettySims [] = ""
prettySims [n] = prettyPrintQ n
prettySims names@(_:_) = (concat $ intersperse ", " (map prettyPrintQ (init names)))
                         ++ " or " ++ prettyPrintQ (last names)

prettyPrintQ :: Pretty a => a -> String
prettyPrintQ x = "'" ++ prettyPrint x ++ "'"
