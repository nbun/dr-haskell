{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
module Repl.Types where

import           Control.Lens                 hiding (Level)
import           Control.Monad.Catch
import           Control.Monad.State
import           Language.Haskell.Interpreter
import           StaticAnalysis.CheckState
import           System.Console.Haskeline

data ReplState = ReplState {
  _filename   :: String,
  _forceLevel :: Maybe Level,
  _runTests   :: Bool,
  _nonStrict  :: Bool
}
  deriving (Show)

initialReplState :: ReplState
initialReplState = ReplState {
  _filename = "",
  _forceLevel = Nothing,
  _runTests = True,
  _nonStrict = False
}

makeLenses ''ReplState

type ReplInput = InputT IO
type ReplInterpreter = InterpreterT ReplInput
type Repl = StateT ReplState ReplInterpreter

runRepl :: ReplState -> Repl a -> IO (Either InterpreterError a)
runRepl state = runInputT defaultSettings . runInterpreter . (`evalStateT` state)

-- no idea if these instances are valid
-- they work in my small tests
instance (MonadIO m, MonadThrow m) => MonadThrow (InputT m) where
  throwM = throwIO

instance (MonadException m, MonadCatch m) => MonadCatch (InputT m) where
  catch a h = System.Console.Haskeline.catch a h

instance (MonadException m, MonadMask m) => MonadMask (InputT m) where
  mask a = lift $ mask $ \u -> runInputT defaultSettings (a (\b -> lift $ u (runInputT defaultSettings b)))
  uninterruptibleMask a = lift $ uninterruptibleMask $ \u -> runInputT defaultSettings (a (\b -> lift $ u (runInputT defaultSettings b)))


--this is quick and dirty using FlexibleInstances
--I think, this can be improved and generalized a
--little.
class Monad m => LiftableInput m where
  liftInput :: ReplInput a -> m a

class Monad m => LiftableInterpreter m where
  liftInterpreter :: ReplInterpreter a -> m a

class Monad m => LiftableRepl m where
  liftRepl :: Repl a -> m a

instance LiftableInput ReplInput where
  liftInput = id

instance LiftableInput ReplInterpreter where
  liftInput = lift

instance LiftableInput Repl where
  liftInput = lift . lift

instance LiftableInterpreter ReplInterpreter where
  liftInterpreter = id

instance LiftableInterpreter Repl where
  liftInterpreter = lift

instance LiftableRepl Repl where
  liftRepl = id
