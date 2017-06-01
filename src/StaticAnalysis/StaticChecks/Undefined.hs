module StaticAnalysis.StaticChecks.Undefined where

import Language.Haskell.Exts
import Data.List
import StaticAnalysis.Messages.StaticErrors
import StaticAnalysis.StaticChecks.Select

undef :: Eq l => Module l -> [Error l]
undef (Module _ _ _ _ []) = []
undef m@(Module l mh mp imps (d:ds)) =
  [Undefined (qNameName qn) (sims qn) []
  | qn <- qns, (nameString . qNameName) qn `notElem` (defStrs qn)]
  ++ undef (Module l mh mp imps ds)
  where qns        = nub $ qNamesOfExps (expsOfDecl d)
        defStrs qn = map nameString $ sims qn
        sims qn    = similar3 d varsOfDecl (qNameName qn)
                     ++ similar3 m defNames (qNameName qn)

