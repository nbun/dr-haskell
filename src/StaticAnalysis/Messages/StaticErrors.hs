module StaticAnalysis.Messages.StaticErrors where

import           Language.Haskell.Exts


data Error l = NoFunDef (Name l) [Name l]
             --          name,   names in scope
             | Undefined (Name l) [Name l] [String]
             --          name,    names in scope, hints
             | Duplicated (Name l) (Maybe (ModuleName l))
  deriving Show
