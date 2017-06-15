module StaticAnalysis.Messages.StaticErrors where

import           Data.List
import           Language.Haskell.Exts
import           Control.Exception
import           Data.Typeable

data Error l = NoFunDef (Name l) [Name l]
             --          name,   similar names in scope
             | Undefined (Name l) [Name l]
             --          name,    similar names in scope
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
             --        name
             | Imported (ModuleName l)
             --         name of the imported module
             | ModuleHeadUsed (ModuleName l)
             --               name used in the module header
             | OwnDataDecl l
             --            position
             | DoUsed l
             --       position
  deriving (Show, Typeable, Ord, Eq) --TODO: mark whether its an error or a warning

instance (Show l, Typeable l) => Exception (Error l)
