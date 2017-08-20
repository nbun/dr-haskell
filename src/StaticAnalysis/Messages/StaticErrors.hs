-- | Defintions of error and entity
module StaticAnalysis.Messages.StaticErrors
  (module StaticAnalysis.Messages.StaticErrors) where

import Control.Exception     hiding (TypeError (..))
import Data.Typeable
import Language.Haskell.Exts
import TypeInference.Main

-- | Describes the kind of a declaration
data Entity = Signature | Definition | Function | Datatype
  deriving (Eq, Ord, Show)

-- | Errors that occur when running static checks, the GHC or check-expect tests
data Error l = NoFunDef (Name l) [Name l]
             --          name,   similar names in scope
             | Undefined (Name l) [Name l]
             --          name,    similar names in scope
             | Duplicated (Name l) Entity (Maybe (ModuleName l))
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
             | SyntaxError l String
             --     position, error as from haskell-src-exts
             | InvalidTest l String
             --     position, string representation of the test
             | Pragma l String
             --      pos pragmaname
             | TypeError l (TIError l)
             --          position, formatted output
             | ClassDecl l
             | InstDecl l
  deriving (Show, Typeable, Ord, Eq)

instance (Show l, Typeable l) => Exception (Error l)

-- | Errors that make further usage of a module impossible
isCritical :: Error l -> Bool
isCritical (NoFunDef _ _)    = True
-- isCritical (Undefined _ _)   = True
isCritical (SyntaxError _ _) = True
isCritical (Pragma _ _)      = True
isCritical (TypeError _ _)   = True
isCritical _                 = False
