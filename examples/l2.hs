{-# DRHASKELL LEVEL2 #-}

data List a = Nil | Cons a (List a)

l :: List Int
l = Cons 42 (Cons 8 Nil)

-- Nil > Nil etc.

-- generated instances with polymorphic functions?
maximum :: [Int] -> Int
maximum xs = foldr (\x y -> if x > y then x else y) 0 xs

-- > checkExpect (maximum [4,8,15,16,23,42]) 42
