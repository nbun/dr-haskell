module Abstract where

import           Control.Monad
import           Language.Haskell.Exts

data Response l = Resp String l
    deriving Show

getAST path = do
    (ParseOk ast) <- parseFile path
    return ast
