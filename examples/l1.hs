{-# DRHASKELL LEVEL1 #-}

double :: Int -> Int
double x = x + x

-- hiding + undefined
tail :: [Int] -> [Int]
-- tail (x:xs) = ys
tail (x:xs) = xs

-- type error + linter
length :: [Int] -> Int
-- length xs = if xs == [] then [] else 1 + length (tail xs)
length xs = if xs == [] then 0 else 1 + length (tail xs)
