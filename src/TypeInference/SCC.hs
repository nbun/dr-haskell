{-|
  Library for computation of strongly connected components.
-}

module TypeInference.SCC (scc) where

import Data.Set (Set (..), empty, insert, member)

-- -----------------------------------------------------------------------------
-- Representation of internal data structures
-- -----------------------------------------------------------------------------

-- | A 'Node' is the algorithm's internal data representation. It consists of a
--   key, a list of bound names and a list of free names, as well as the object
--   to be represented.
data Node a b = Node { key  :: Int
                     , bvs  :: [b]
                     , fvs  :: [b]
                     , node :: a   }

instance Eq (Node a b) where
  n1 == n2 = key n1 == key n2

instance Ord (Node a b) where
  n1 <= n2 = key n1 <= key n2

-- -----------------------------------------------------------------------------
-- Functions for computation of strongly connected components
-- -----------------------------------------------------------------------------

-- | Computes the strongly connected components of the given list of entities.
--   The first function is used to map each node to the entities defined in this
--   node. The second function is used to map each node to the entities used in
--   this node.
scc :: Eq b => (a -> [b]) -> (a -> [b]) -> [a] -> [[a]]
scc bvs' fvs' = map (map node) . tsort' . tsort . zipWith wrap [0..]
  where
    wrap i n = Node i (bvs' n) (fvs' n) n
    tsort :: Eq b => [Node a b] -> [Node a b]
    tsort ns = snd (dfs ns empty [])
      where
        dfs []      marks stack = (marks, stack)
        dfs (n:ns') marks stack | member n marks = dfs ns' marks stack
                                | otherwise      = dfs ns' marks' (n:stack')
          where
            (marks', stack') = dfs (defs n) (insert n marks) stack
            defs n1 = filter (any (`elem` (fvs n1)) . bvs) ns
    tsort' :: Eq b => [Node a b] -> [[Node a b]]
    tsort' ns = snd (dfs ns empty [])
      where
        dfs []      marks stack = (marks, stack)
        dfs (n:ns') marks stack
          | member n marks = dfs ns' marks stack
          | otherwise      = dfs ns' marks' ((n:(concat stack')):stack)
          where
            (marks', stack') = dfs (uses n) (insert n marks) []
            uses n1 = filter (any (`elem` (bvs n1)) . fvs) ns