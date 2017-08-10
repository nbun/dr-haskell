{-|
  Simplified version of the 'Prelude' without type classes.
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
  -- | Arithmetic operators +, -, * and / for 'Int' and 'Float'.
  (+),
  (-),
  (*),
  (/),
  (+.),
  (-.),
  (*.),
  (/.),
  -- | Simple versions of list functions that use 'Foldable' or 'Num'.
  length,
  concat,
  foldl,
  foldr,
  sum,
  all,
  -- | 'lookup' has an extra @eq :: a -> a -> Bool@ argument.
  lookup,
  -- | More infix operators.
  (++),
  (.),
  (||),
  (&&),
  (!!),
  -- | Functions that appear in the lecture or exercises.
  last,
  head,
  tail,
  init,
  fst,
  snd,
  zip,
  unzip,
  take,
  flip,
  map,
  filter,
  curry,
  uncurry,
  const,
  repeat,
  iterate,
  reverse,
  replicate,
  otherwise
  ) where

import qualified Prelude

-- -----------------------------------------------------------------------------
-- Arithmetic operators
-- -----------------------------------------------------------------------------

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

-- -----------------------------------------------------------------------------
-- List functions
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

all :: (a -> Prelude.Bool) -> [a] -> Prelude.Bool
all = Prelude.all

lookup :: (a -> a -> Prelude.Bool) -> a -> [(a, b)] -> Prelude.Maybe b
lookup _  _ []                              = Prelude.Nothing
lookup eq x ((a, b):xs) | eq x a            = Prelude.Just b
                        | Prelude.otherwise = lookup eq x xs

-- -----------------------------------------------------------------------------
-- Prelude functions
-- -----------------------------------------------------------------------------

(++) :: [a] -> [a] -> [a]
(++) = (Prelude.++)

(.) :: (b -> c) -> (a -> b) -> a -> c
(.) = (Prelude..)

(||) :: Prelude.Bool -> Prelude.Bool -> Prelude.Bool
(||) = (Prelude.||)

(&&) :: Prelude.Bool -> Prelude.Bool -> Prelude.Bool
(&&) = (Prelude.&&)

(!!) :: [a] -> Prelude.Int -> a
(!!) = (Prelude.!!)

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

-- -----------------------------------------------------------------------------
-- Data types and functions only loaded by the type inference (NOT EXPORTED)
-- -----------------------------------------------------------------------------

data Bool = False | True

data Maybe a = Nothing | Just a

data Either a b = Left a | Right b

data Ordering = LT | EQ | GT

type String = [Prelude.Char]

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