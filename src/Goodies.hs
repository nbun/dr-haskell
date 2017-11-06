{-|
  This library provides some useful auxiliary functions.
-}

module Goodies
  ( (++=), both, bothM, concatMapM, mapAccumM, one, two, bquotes, brackets
  , braces, parens, parensIf, tuple, list, indent, vsep, getFullPath
  ) where

import Control.Applicative ((<$>))
import Control.Monad.State (get, put, runStateT)
import Control.Monad.Trans (lift)
import Data.List           (intercalate)
import Data.Tuple          (swap)
import System.Directory    (getHomeDirectory)
import System.FilePath     (joinPath, splitPath)
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

-- | Encloses a string with back quotes.
bquotes :: String -> String
bquotes s = '`' : s ++ "`"

-- | Encloses a string in brackets.
brackets :: String -> String
brackets s = '[' : s ++ "]"

-- | Encloses a string in braces.
braces :: String -> String
braces s = '{' : s ++ "}"

-- | Encloses a string in parentheses.
parens :: String -> String
parens s = '(' : s ++ ")"

-- | Encloses a string in parentheses if the given condition is true.
parensIf :: Bool -> String -> String
parensIf c s = if c then parens s else s

-- | Returns a string representation of a tuple with the given list of
--   components.
tuple :: [String] -> String
tuple = parens . intercalate ", "

-- | Returns a string representation of a list with the given list of
--   components.
list :: [String] -> String
list = brackets . intercalate ", "

-- | Indents the given string with the given number of spaces.
indent :: Int -> String -> String
indent n s = replicate n ' ' ++ s

-- | Joins the given list of strings to one single string by adding a newline
--   character between two adjacent strings.
vsep :: [String] -> String
vsep = intercalate "\n"

-- | Expands paths with a ~
getFullPath :: String -> IO FilePath
getFullPath s = case splitPath s of
    "~/" : t -> joinPath . (: t) <$> getHomeDirectory
    _        -> return s
