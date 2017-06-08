module StaticAnalysis.Messages.StaticErrors where

import           Data.List
import           Language.Haskell.Exts
import           Control.Exception
import           Data.Typeable

data Error l = NoFunDef (Name l) [Name l]
             --          name,   names in scope
             | Undefined (Name l) [Name l] [String]
             --          name,    names in scope, hints
             | Duplicated (Name l) (Maybe (ModuleName l))
             --           name,    module that contains the duplicate
             | TypeVarApplication (Name l)
             --            position
             | HigherOrder l
             --               position
             | LambdaFunction l
             --          name
             | NoTypeDef (Name l)
             --          name
             | Shadowing (QName l)
             --        name
             | TypeVar (Name l)
  deriving (Show, Typeable, Ord, Eq) --TODO: mark whether its an error or a warning

instance (Show l, Typeable l) => Exception (Error l)
