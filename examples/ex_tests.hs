{-# DRHASKELL LEVEL2 #-}

data List = Cons Int List
          | Nil

length :: List -> Int
length Nil         = 0
length (Cons _ xs) = 1 + length xs

replaceFirst :: Int -> List -> List
replaceFirst _  Nil        = Nil
replaceFirst x (Cons _ xs) = Cons x xs

add :: List -> List -> List
add Nil         ys          = ys
add xs          Nil         = xs
add (Cons x xs) (Cons y ys) = {-Cons (x + y) $-} add xs ys

sum :: List -> Int
sum Nil         = 0
sum (Cons x xs) = x + sum xs


-- > checkExpect 0 (length Nil)
-- > checkExpect 2 (length (Cons 1 (Cons 2 Nil)))

-- > quickCheck (\xs -> (length xs) == (length (replaceFirst 1 xs)))
-- > quickCheck (\xs ys -> sum (add xs ys) == sum xs + sum ys)