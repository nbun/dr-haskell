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
  ieq,
  beq,
  ceq,
  ineq,
  bneq,
  cneq,
  ilt,
  blt,
  clt,
  ileq,
  bleq,
  cleq,
  igt,
  bgt,
  cgt,
  igeq,
  bgeq,
  cgeq,

  -- | Arithmetic operators +, -, * and / for Int and Float
  (+),
  (-),
  (*),
  (/),
  (.+),
  (.-),
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
  P.filter,
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

ieq :: P.Int -> P.Int -> P.Bool
ieq = (P.==)

beq :: P.Bool -> P.Bool -> P.Bool
beq = (P.==)

ceq :: P.Char -> P.Char -> P.Bool
ceq = (P.==)

ineq :: P.Int -> P.Int -> P.Bool
ineq = (P./=)

bneq :: P.Bool -> P.Bool -> P.Bool
bneq = (P./=)

cneq :: P.Char -> P.Char -> P.Bool
cneq = (P./=)

ilt :: P.Int -> P.Int -> P.Bool
ilt = (P.<)

ileq :: P.Int -> P.Int -> P.Bool
ileq = (P.<=)

igt :: P.Int -> P.Int -> P.Bool
igt = (P.>)

igeq :: P.Int -> P.Int -> P.Bool
igeq = (P.>=)

blt :: P.Bool -> P.Bool -> P.Bool
blt = (P.<)

bleq :: P.Bool -> P.Bool -> P.Bool
bleq = (P.<=)

bgt :: P.Bool -> P.Bool -> P.Bool
bgt = (P.>)

bgeq :: P.Bool -> P.Bool -> P.Bool
bgeq = (P.>=)

clt :: P.Char -> P.Char -> P.Bool
clt = (P.<)

cleq :: P.Char -> P.Char -> P.Bool
cleq = (P.<=)

cgt :: P.Char -> P.Char -> P.Bool
cgt = (P.>)

cgeq :: P.Char -> P.Char -> P.Bool
cgeq = (P.>=)

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

(.-) :: P.Float -> P.Float -> P.Float
(.-) = (P.-)

(.+) :: P.Float -> P.Float -> P.Float
(.+) = (P.+)

(.*) :: P.Float -> P.Float -> P.Float
(.*) = (P.*)

(./) :: P.Float -> P.Float -> P.Float
(./) = (P./)

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
