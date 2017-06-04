module StaticAnalysis.AllChecks where

import           AstChecks.Check
import           StaticAnalysis.StaticChecks.Derivings
import           StaticAnalysis.StaticChecks.Duplicated
import           StaticAnalysis.StaticChecks.HigherOrder
import           StaticAnalysis.StaticChecks.Lambda
import           StaticAnalysis.StaticChecks.NoFunDef
import           StaticAnalysis.StaticChecks.NoTypeDef
import           StaticAnalysis.StaticChecks.Select
import           StaticAnalysis.StaticChecks.Shadowing
import           StaticAnalysis.StaticChecks.TypeVarApplication
import           StaticAnalysis.StaticChecks.TypeVars
import           StaticAnalysis.StaticChecks.Undefined
