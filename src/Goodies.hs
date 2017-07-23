{-|
  This library provides some useful auxiliary functions.
-}

module Goodies
  ( (++=), both, bothM, concatMapM, mapAccumM, one, two, parensIf, tupled
  ) where

import Control.Monad.State (get, put, runStateT)
import Control.Monad.Trans (lift)
import Data.Tuple (swap)

-- -----------------------------------------------------------------------------
-- Definition of auxiliary functions
-- -----------------------------------------------------------------------------

-- | Combines two monads into one by appending the inner lists.
(++=) :: Monad m => m [a] -> m [a] -> m [a]
mxs ++= mys = (++) <$> mxs <*> mys

-- | Applies a function to both components of a tuple.
both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

-- | Applies a monadic action to both components of a tuple and evaluates both
--   monadic actions in the sequence from left to right.
bothM :: Monad m => (a -> m b) -> (a, a) -> m (b, b)
bothM f (x, y) = (,) <$> f x <*> f y

-- | Applies a monadic action to all elements of a list and evaluates the
--   monadic actions in the sequence from left to right by concatenating the
--   inner lists.
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = concat <$> mapM f xs

-- | Applies a monadic action to each element of a structure, passing an
--   accumulating parameter from left to right, and returning a final value of
--   this accumulator together with the new structure.
mapAccumM :: (Monad m, Traversable t) => (a -> b -> m (a, c)) -> a -> t b
          -> m (a, t c)
mapAccumM f a t = swap <$> runStateT (mapM go t) a
  where
    go x = do s <- get
              (s', y) <- lift (f s x)
              put s'
              return y

-- | Checks whether the given list has exactly one element.
one :: [a] -> Bool
one []     = False
one (_:xs) = null xs

-- | Checks whether the given list has exactly two elements.
two :: [a] -> Bool
two []     = False
two (_:xs) = one xs

-- | Encloses a string in parentheses if the given condition is true.
parensIf :: Bool -> String -> String
parensIf c s = if c then "(" ++ s ++ ")" else s

-- | Returns a string representation of a tuple with the given list of
--   components.
tupled :: [String] -> String
tupled []     = "()"
tupled (x:xs) = "(" ++ x ++ concatMap (", " ++) xs ++ ")"