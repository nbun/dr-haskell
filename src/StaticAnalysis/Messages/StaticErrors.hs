module StaticAnalysis.StaticChecks.Messages where

import Language.Haskell.Exts


data Error l = NoFunDef (Name l) [Name l]
             --          name,   names in scope
             | Undefined (Name l) [Name l] [String]
             --          name,    names in scope, hints

