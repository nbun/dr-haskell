-- | Contains the simple level data type to prevent import cycles
module StaticAnalysis.Level (Level (..), useOwnTI) where

-- | A level determine the checks applied to a module
data Level = Level1 | Level2 | Level3 | LevelFull
  deriving Show

-- | Is our type inference active for a given level?
useOwnTI :: Level -> Bool
useOwnTI LevelFull = False
useOwnTI _         = True

