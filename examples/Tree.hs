data Tree a = Leaf a | Branch (Tree a) (Tree a)

sumTree :: Tree Int -> Int
sumTree = sumTree' 0
  where
    sumTree' acc (Leaf x)     = acc + x
    sumTree' acc (Branch l r) = sumTree' (sumTree' acc l) r

mirrorTree t@(Leaf _)   = t
mirrorTree (Branch l r) = Branch (mirrorTree r) (mirrorTree l)