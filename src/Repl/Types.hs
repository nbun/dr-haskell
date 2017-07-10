{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
module Repl.Types (
  ReplState(..),
  filename, forceLevel, runTests, nonStrict, customPrelude,
  initialReplState,
  initialLintReplState,
  ReplInput,
  ReplInterpreter,
  Repl,
  runRepl,
  liftInput,
  liftInterpreter,
  liftRepl,
) where

import           Control.Lens                 hiding (Level)
import           Control.Monad.Catch
import           Control.Monad.State
import           Language.Haskell.Interpreter
import           StaticAnalysis.CheckState
import           System.Console.Haskeline

-- includes all state that may be needed in the REPL
data ReplState = ReplState {
  _filename      :: String,
  _forceLevel    :: Maybe Level,
  _runTests      :: Bool,
  _nonStrict     :: Bool,
  _customPrelude :: Bool
}
  deriving (Show)

-- some sane defaults
initialReplState :: ReplState
initialReplState = ReplState {
  _filename      = "",
  _forceLevel    = Nothing,
  _runTests      = True,
  _nonStrict     = False,
  _customPrelude = True
}

-- some sane defaults
initialLintReplState :: ReplState
initialLintReplState = ReplState {
  _filename      = "",
  _forceLevel    = Nothing,
  _runTests      = False,
  _nonStrict     = False,
  _customPrelude = False
}

makeLenses ''ReplState

-- the REPL need functionality supplied by different monads.
-- IO in needed because the used monads need it
-- InputT is needed for the Haskeline pretty in-/output
-- InterpreterT is needed for the Hint Haskell interpreter
-- StateT ReplState carries our own information
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
  catch = System.Console.Haskeline.catch

instance (MonadException m, MonadMask m) => MonadMask (InputT m) where
  mask a = lift $
           mask $
           \u ->
             runInputT
              defaultSettings
                (a (lift . u . runInputT defaultSettings))
  uninterruptibleMask a = lift $
                          uninterruptibleMask $
                          \u -> runInputT
                                  defaultSettings
                                    (a (lift . u . runInputT defaultSettings))

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
