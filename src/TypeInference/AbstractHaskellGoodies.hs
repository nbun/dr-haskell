module TypeInference.AbstractHaskellGoodies
  ( teVar
  ) where

import TypeInference.AbstractHaskell

teVar :: Int -> a -> TypeExpr a
teVar v x = TVar ((v, varToString v), x)