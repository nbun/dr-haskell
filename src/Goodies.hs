{-|
  This library provides some useful auxiliary functions
-}

module Goodies
  ( both, bothM
  ) where

-- -----------------------------------------------------------------------------
-- Definition of auxiliary functions
-- -----------------------------------------------------------------------------

-- Applies a function to both components of a tuple.
both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

-- | Applies a monadic action to both components of a tuple and evaluates both
--   monadic actions in the sequence from left to right.
bothM :: Monad m => (a -> m b) -> (a, a) -> m (b, b)
bothM f (x, y) = (,) <$> f x <*> f y