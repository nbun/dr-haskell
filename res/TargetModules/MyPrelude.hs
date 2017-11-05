{-|
  Simplified version of the 'Prelude' without type classes.
  Only the type classes 'Eq', 'Ord' and 'Show' are used, because every
  user-defined data type has an implicit instance of these three type classes.
-}

module MyPrelude (
  -- | Data types from the 'Prelude'.
  Prelude.Int,
  Prelude.Float,
  Prelude.Char,
  Prelude.String,
  Prelude.Bool (..),
  Prelude.Maybe (..),
  Prelude.Either (..),
  Prelude.Ordering (..),
  -- | Arithmetic operators for 'Int' and 'Float'.
  (+),
  (-),
  (*),
  (/),
  (+.),
  (-.),
  (*.),
  -- | Infix operators from the 'Eq' and 'Ord' type classes.
  (Prelude.==),
  (Prelude./=),
  (Prelude.<),
  (Prelude.<=),
  (Prelude.>),
  (Prelude.>=),
  -- | Other infix operators.
  (Prelude.++),
  (Prelude..),
  (Prelude.$),
  (Prelude.||),
  (Prelude.&&),
  (Prelude.!!),
  -- | Simple versions of functions that use 'Foldable', 'Integral' or 'Num'.
  length,
  concat,
  foldl,
  foldr,
  sum,
  product,
  any,
  all,
  null,
  elem,
  abs,
  div,
  mod,
  even,
  odd,
  and,
  or,
  -- | Other functions that appear in the lecture or exercises.
  Prelude.lookup,
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
  Prelude.reverse,
  Prelude.replicate,
  Prelude.otherwise,
  Prelude.not,
  Prelude.compare,
  Prelude.min,
  Prelude.max,
  Prelude.drop,
  Prelude.zipWith,
  Prelude.lines,
  Prelude.unlines,
  Prelude.words,
  Prelude.unwords,
  Prelude.show,
  ) where

import qualified Prelude

-- -----------------------------------------------------------------------------
-- Arithmetic operators for 'Int' and 'Float'
-- -----------------------------------------------------------------------------

(+) :: Prelude.Int -> Prelude.Int -> Prelude.Int
(+) = (Prelude.+)

(-) :: Prelude.Int -> Prelude.Int -> Prelude.Int
(-) = (Prelude.-)

(*) :: Prelude.Int -> Prelude.Int -> Prelude.Int
(*) = (Prelude.*)

(/) :: Prelude.Int -> Prelude.Int -> Prelude.Float
(/) = (Prelude./)

(+.) :: Prelude.Float -> Prelude.Float -> Prelude.Float
(+.) = (Prelude.+)

(-.) :: Prelude.Float -> Prelude.Float -> Prelude.Float
(-.) = (Prelude.-)

(*.) :: Prelude.Float -> Prelude.Float -> Prelude.Float
(*.) = (Prelude.*)

-- -----------------------------------------------------------------------------
-- Simple versions of functions that use 'Foldable', 'Integral' or 'Num'
-- -----------------------------------------------------------------------------

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

product :: [Prelude.Int] -> Prelude.Int
product = Prelude.product

any :: (a -> Prelude.Bool) -> [a] -> Prelude.Bool
any = Prelude.any

all :: (a -> Prelude.Bool) -> [a] -> Prelude.Bool
all = Prelude.all

null :: [a] -> Prelude.Bool
null = Prelude.null

elem :: Prelude.Eq a => a -> [a] -> Prelude.Bool
elem = Prelude.elem

abs :: Prelude.Int -> Prelude.Int
abs = Prelude.abs

div :: Prelude.Int -> Prelude.Int -> Prelude.Int
div = Prelude.div

mod :: Prelude.Int -> Prelude.Int -> Prelude.Int
mod = Prelude.mod

even :: Prelude.Int -> Prelude.Bool
even = Prelude.even

odd :: Prelude.Int -> Prelude.Bool
odd = Prelude.odd

and :: [Prelude.Bool] -> Prelude.Bool
and = Prelude.and

or :: [Prelude.Bool] -> Prelude.Bool
or = Prelude.or

-- -----------------------------------------------------------------------------
-- Data types and functions only loaded by the type inference (NOT EXPORTED)
-- -----------------------------------------------------------------------------

type String = [Prelude.Char]

data Bool = False | True

data Maybe a = Nothing | Just a

data Either a b = Left a | Right b

data Ordering = LT | EQ | GT

(==) :: Prelude.Eq a => a -> a -> Prelude.Bool
(==) = (Prelude.==)

(/=) :: Prelude.Eq a => a -> a -> Prelude.Bool
(/=) = (Prelude./=)

(<) :: Prelude.Ord a => a -> a -> Prelude.Bool
(<) = (Prelude.<)

(<=) :: Prelude.Ord a => a -> a -> Prelude.Bool
(<=) = (Prelude.<=)

(>) :: Prelude.Ord a => a -> a -> Prelude.Bool
(>) = (Prelude.>)

(>=) :: Prelude.Ord a => a -> a -> Prelude.Bool
(>=) = (Prelude.>=)

(++) :: [a] -> [a] -> [a]
(++) = (Prelude.++)

(.) :: (b -> c) -> (a -> b) -> a -> c
(.) = (Prelude..)

($) :: (a -> b) -> a -> b
($) = (Prelude.$)

(||) :: Prelude.Bool -> Prelude.Bool -> Prelude.Bool
(||) = (Prelude.||)

(&&) :: Prelude.Bool -> Prelude.Bool -> Prelude.Bool
(&&) = (Prelude.&&)

(!!) :: [a] -> Prelude.Int -> a
(!!) = (Prelude.!!)

lookup :: Prelude.Eq a => a -> [(a, b)] -> Prelude.Maybe b
lookup = Prelude.lookup

last :: [a] -> a
last = Prelude.last

head :: [a] -> a
head = Prelude.head

tail :: [a] -> [a]
tail = Prelude.tail

init :: [a] -> [a]
init = Prelude.init

fst :: (a, b) -> a
fst = Prelude.fst

snd :: (a, b) -> b
snd = Prelude.snd

zip :: [a] -> [b] -> [(a, b)]
zip = Prelude.zip

unzip :: [(a, b)] -> ([a], [b])
unzip = Prelude.unzip

take :: Prelude.Int -> [a] -> [a]
take = Prelude.take

flip :: (a -> b -> c) -> b -> a -> c
flip = Prelude.flip

map :: (a -> b) -> [a] -> [b]
map = Prelude.map

filter :: (a -> Prelude.Bool) -> [a] -> [a]
filter = Prelude.filter

curry :: ((a, b) -> c) -> a -> b -> c
curry = Prelude.curry

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry = Prelude.uncurry

const :: a -> b -> a
const = Prelude.const

repeat :: a -> [a]
repeat = Prelude.repeat

iterate :: (a -> a) -> a -> [a]
iterate = Prelude.iterate

reverse :: [a] -> [a]
reverse = Prelude.reverse

replicate :: Prelude.Int -> a -> [a]
replicate = Prelude.replicate

otherwise :: Prelude.Bool
otherwise = Prelude.otherwise

not :: Prelude.Bool -> Prelude.Bool
not = Prelude.not

compare :: Prelude.Ord a => a -> a -> Prelude.Ordering
compare = Prelude.compare

min :: Prelude.Ord a => a -> a -> a
min = Prelude.min

max :: Prelude.Ord a => a -> a -> a
max = Prelude.max

minimum :: Prelude.Ord a => [a] -> a
minimum = Prelude.minimum

maximum :: Prelude.Ord a => [a] -> a
maximum = Prelude.maximum

drop :: Prelude.Int -> [a] -> [a]
drop = Prelude.drop

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith = Prelude.zipWith

lines :: Prelude.String -> [Prelude.String]
lines = Prelude.lines

unlines :: [Prelude.String] -> Prelude.String
unlines = Prelude.unlines

words :: Prelude.String -> [Prelude.String]
words = Prelude.words

unwords :: [Prelude.String] -> Prelude.String
unwords = Prelude.unwords

show :: Prelude.Show a => a -> Prelude.String
show = Prelude.show
