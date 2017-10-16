{-# DRHASKELL LEVEL1 #-}

fib :: Int -> Int
fib 1 = 1
fib 2 = 1
fib n = fib (n-1) + fib (n-2)

-- > checkExpect 8 (fib 6)
-- > checkExpect 21 (fib 8)
-- > checkExpect 144 (fib 12)