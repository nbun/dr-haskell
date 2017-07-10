-- | Simplified version of the Prelude without type classes

module MyPrelude (
  -- | Data types
  P.Int,
  P.Float,
  P.Bool(..),
  P.Maybe(..),
  P.Either(..),
  P.Char,
  P.String,

  -- | Comparison operators ==, <, >, <=, >= for Int, Bool and Char
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

  -- | Arithmetic operators +, -, * and / for Int and Float
  (+),
  (-),
  (*),
  (/),
  (.+),
  (-.),
  (.*),
  (./),

  -- | Simple versions of list functions that use Foldable or Num
  length,
  concat,
  foldl,
  foldr,
  sum,
  all,
  lookup, -- | lookup has an extra eq :: a -> a -> Bool argument

  -- | More infix operators
  (P.++),
  (P..),
  (P.||),
  (P.&&),
  (P.!!),

  -- | Functions that appear in the lecture or exercises
  P.last,
  P.head,
  P.tail,
  P.init,
  P.fst,
  P.snd,
  P.zip,
  P.unzip,
  P.take,
  P.flip,
  P.map,
  P.fltInter,
  P.curry,
  P.uncurry,
  P.const,
  P.repeat,
  P.iterate,
  P.putStr,
  P.getLine,
  P.readFile,
  P.writeFile,
  P.reverse,
  P.replicate,
  P.otherwise
  ) where

import qualified Prelude as P

--------------------------------------------------------------------------------
-- | Data types

data Maybe a = Just a | Nothing

--------------------------------------------------------------------------------
-- | Comparison operators

eqInt :: P.Int -> P.Int -> P.Bool
eqInt = (P.==)

eqBool :: P.Bool -> P.Bool -> P.Bool
eqBool = (P.==)

eqChar :: P.Char -> P.Char -> P.Bool
eqChar = (P.==)

neqInt :: P.Int -> P.Int -> P.Bool
neqInt = (P./=)

neqBool :: P.Bool -> P.Bool -> P.Bool
neqBool = (P./=)

neqChar :: P.Char -> P.Char -> P.Bool
neqChar = (P./=)

ltInt :: P.Int -> P.Int -> P.Bool
ltInt = (P.<)

leqInt :: P.Int -> P.Int -> P.Bool
leqInt = (P.<=)

gtInt :: P.Int -> P.Int -> P.Bool
gtInt = (P.>)

geqInt :: P.Int -> P.Int -> P.Bool
geqInt = (P.>=)

ltBool :: P.Bool -> P.Bool -> P.Bool
ltBool = (P.<)

leqBool :: P.Bool -> P.Bool -> P.Bool
leqBool = (P.<=)

gtBool :: P.Bool -> P.Bool -> P.Bool
gtBool = (P.>)

geqBool :: P.Bool -> P.Bool -> P.Bool
geqBool = (P.>=)

ltChar :: P.Char -> P.Char -> P.Bool
ltChar = (P.<)

leqChar :: P.Char -> P.Char -> P.Bool
leqChar = (P.<=)

gtChar :: P.Char -> P.Char -> P.Bool
gtChar = (P.>)

geqChar :: P.Char -> P.Char -> P.Bool
geqChar = (P.>=)

--------------------------------------------------------------------------------
-- | Arithmetic operators

(-) :: P.Int -> P.Int -> P.Int
(-) = (P.-)

(+) :: P.Int -> P.Int -> P.Int
(+) = (P.+)

(*) :: P.Int -> P.Int -> P.Int
(*) = (P.*)

(/) :: P.Int -> P.Int -> P.Int
(/) = P.div

(-.) :: P.Float -> P.Float -> P.Float
(-.) = (P.-)

(+.) :: P.Float -> P.Float -> P.Float
(+.) = (P.+)

(*.) :: P.Float -> P.Float -> P.Float
(*.) = (P.*)

(/.) :: P.Float -> P.Float -> P.Float
(/.) = (P./)

--------------------------------------------------------------------------------
-- | List functions

length :: [a] -> P.Int
length = P.length

concat :: [[a]] -> [a]
concat = P.concat

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl = P.foldl

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr = P.foldr

sum :: [P.Int] -> P.Int
sum = P.sum

all :: (a -> P.Bool) -> [a] -> P.Bool
all = P.all

lookup :: (a -> a -> P.Bool) -> a -> [(a, b)] -> Maybe b
lookup _ _ [] = Nothing
lookup eq x ((a, b) : xs) | eq x a      = Just b
                          | P.otherwise = lookup eq x xs
