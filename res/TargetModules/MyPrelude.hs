{-|
  Simplified version of the 'Prelude' without type classes.
-}

module MyPrelude (
  -- | Data types
  Prelude.Int,
  Prelude.Float,
  Prelude.Bool(..),
  Prelude.Maybe(..),
  Prelude.Either(..),
  Prelude.Char,
  Prelude.String,
  -- | Comparison operators ==, <, >, <=, >= for Int, Bool and Char.
  eqInt,
  eqBool,
  eqChar,
  neqInt,
  neqBool,
  neqChar,
  ltInt,
  ltBool,
  ltChar,
  leqInt,
  leqBool,
  leqChar,
  gtInt,
  gtBool,
  gtChar,
  geqInt,
  geqBool,
  geqChar,
  -- | Arithmetic operators +, -, * and / for Int and Float.
  (+),
  (-),
  (*),
  (/),
  (+.),
  (-.),
  (*.),
  (/.),
  -- | Simple versions of list functions that use Foldable or Num.
  length,
  concat,
  foldl,
  foldr,
  sum,
  all,
  lookup, -- lookup has an extra eq :: a -> a -> Bool argument.
  -- | More infix operators.
  (Prelude.++),
  (Prelude..),
  (Prelude.||),
  (Prelude.&&),
  (Prelude.!!),
  -- | Functions that appear in the lecture or exercises.
  Prelude.last,
  Prelude.head,
  Prelude.tail,
  Prelude.init,
  Prelude.fst,
  Prelude.snd,
  Prelude.zip,
  Prelude.unzip,
  Prelude.take,
  Prelude.flip,
  Prelude.map,
  Prelude.filter,
  Prelude.curry,
  Prelude.uncurry,
  Prelude.const,
  Prelude.repeat,
  Prelude.iterate,
  Prelude.putStr,
  Prelude.getLine,
  Prelude.readFile,
  Prelude.writeFile,
  Prelude.reverse,
  Prelude.replicate,
  Prelude.otherwise
  ) where

import qualified Prelude

--------------------------------------------------------------------------------
-- | Comparison operators
eqInt :: Prelude.Int -> Prelude.Int -> Prelude.Bool
eqInt = (Prelude.==)

eqBool :: Prelude.Bool -> Prelude.Bool -> Prelude.Bool
eqBool = (Prelude.==)

eqChar :: Prelude.Char -> Prelude.Char -> Prelude.Bool
eqChar = (Prelude.==)

neqInt :: Prelude.Int -> Prelude.Int -> Prelude.Bool
neqInt = (Prelude./=)

neqBool :: Prelude.Bool -> Prelude.Bool -> Prelude.Bool
neqBool = (Prelude./=)

neqChar :: Prelude.Char -> Prelude.Char -> Prelude.Bool
neqChar = (Prelude./=)

ltInt :: Prelude.Int -> Prelude.Int -> Prelude.Bool
ltInt = (Prelude.<)

leqInt :: Prelude.Int -> Prelude.Int -> Prelude.Bool
leqInt = (Prelude.<=)

gtInt :: Prelude.Int -> Prelude.Int -> Prelude.Bool
gtInt = (Prelude.>)

geqInt :: Prelude.Int -> Prelude.Int -> Prelude.Bool
geqInt = (Prelude.>=)

ltBool :: Prelude.Bool -> Prelude.Bool -> Prelude.Bool
ltBool = (Prelude.<)

leqBool :: Prelude.Bool -> Prelude.Bool -> Prelude.Bool
leqBool = (Prelude.<=)

gtBool :: Prelude.Bool -> Prelude.Bool -> Prelude.Bool
gtBool = (Prelude.>)

geqBool :: Prelude.Bool -> Prelude.Bool -> Prelude.Bool
geqBool = (Prelude.>=)

ltChar :: Prelude.Char -> Prelude.Char -> Prelude.Bool
ltChar = (Prelude.<)

leqChar :: Prelude.Char -> Prelude.Char -> Prelude.Bool
leqChar = (Prelude.<=)

gtChar :: Prelude.Char -> Prelude.Char -> Prelude.Bool
gtChar = (Prelude.>)

geqChar :: Prelude.Char -> Prelude.Char -> Prelude.Bool
geqChar = (Prelude.>=)

--------------------------------------------------------------------------------
-- | Arithmetic operators
(-) :: Prelude.Int -> Prelude.Int -> Prelude.Int
(-) = (Prelude.-)

(+) :: Prelude.Int -> Prelude.Int -> Prelude.Int
(+) = (Prelude.+)

(*) :: Prelude.Int -> Prelude.Int -> Prelude.Int
(*) = (Prelude.*)

(/) :: Prelude.Int -> Prelude.Int -> Prelude.Int
(/) = Prelude.div

(-.) :: Prelude.Float -> Prelude.Float -> Prelude.Float
(-.) = (Prelude.-)

(+.) :: Prelude.Float -> Prelude.Float -> Prelude.Float
(+.) = (Prelude.+)

(*.) :: Prelude.Float -> Prelude.Float -> Prelude.Float
(*.) = (Prelude.*)

(/.) :: Prelude.Float -> Prelude.Float -> Prelude.Float
(/.) = (Prelude./)

--------------------------------------------------------------------------------
-- | List functions
length :: [a] -> Prelude.Int
length = Prelude.length

concat :: [[a]] -> [a]
concat = Prelude.concat

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl = Prelude.foldl

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr = Prelude.foldr

sum :: [Prelude.Int] -> Prelude.Int
sum = Prelude.sum

all :: (a -> Prelude.Bool) -> [a] -> Prelude.Bool
all = Prelude.all

lookup :: (a -> a -> Prelude.Bool) -> a -> [(a, b)] -> Prelude.Maybe b
lookup _  _ []                              = Prelude.Nothing
lookup eq x ((a, b):xs) | eq x a            = Prelude.Just b
                        | Prelude.otherwise = lookup eq x xs
