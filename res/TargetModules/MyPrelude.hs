module MyPrelude (
    Int,
    P.Bool(..),
    ieq,
    ineq,
    (-),
    (+),
    (++),
    Maybe(..),
    map,
    foldl,
    foldr,
    (.),
    filter,
    not,
    (||),
    (&&),
    concatMap
)where

{-
  Probleme:

  Test auf Gleichheit? ieq, ineq, beq, bneq?
  Eigene Typen für Int/Bool vs. Wiederexportieren?
-}

import qualified Prelude as P

type Int = P.Int
--type Bool = P.Bool

--(==) :: P.Eq a => a -> a -> P.Bool
--(==) = (P.==)

ieq :: Int -> Int -> P.Bool
ieq = (P.==)

ineq :: Int -> Int -> P.Bool
ineq = (P./=)

(-) :: Int -> Int -> Int
(-) = (P.-)
(+) :: Int -> Int -> Int
(+) = (P.+)

(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

data Maybe a = Nothing | Just a

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl = P.foldl

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr = P.foldr

(.) :: (b -> c) -> (a -> b) -> a -> c
(.) = (P..)

filter :: (a -> P.Bool) -> [a] -> [a]
filter = P.filter

not :: P.Bool -> P.Bool
not P.True = P.False
not P.False = P.True

(&&) :: P.Bool -> P.Bool -> P.Bool
P.True && P.True = P.True
_      && _      = P.False

(||) :: P.Bool -> P.Bool -> P.Bool
P.False || P.False = P.False
_       || _       = P.True

concatMap :: (a -> [b]) -> [a] -> [b]
concatMap = P.concatMap