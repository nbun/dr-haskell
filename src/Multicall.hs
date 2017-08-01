module Multicall where

import           System.Environment

import qualified DrHaskellLint      as Lint (main)
import qualified Repl.Main          as Repl (main)

main :: IO ()
main = do
  pn <- getProgName
  case pn of
       "drhaskell" -> Repl.main
       "drhaskell-lint" -> Lint.main
       _ -> putStrLn "Please use one of the provided symlinks to this binary."
