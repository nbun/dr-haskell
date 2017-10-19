data Tree a = Leaf a | Branch (Tree a) (Tree a)

sumTree :: Tree Int -> Int
sumTree = sumTree.sumTree' 0

sumTree.sumTree' :: Int -> Tree Int -> Int
sumTree.sumTree' acc (Leaf x)
  = acc + x
sumTree.sumTree' acc (Branch l r)
  = sumTree.sumTree' (sumTree.sumTree' acc l) r

mirrorTree :: Tree a -> Tree a
mirrorTree t@(Leaf _)   = t
mirrorTree (Branch l r) = Branch (mirrorTree r) (mirrorTree l)