module Monads (
  StateError,
  runStateError,
  throw,
  
  -- De Monad.Trans.State
  get,
  put,
  modify
) where

import Commons

import Control.Monad.Trans.State (StateT, runStateT, get, put, modify)
import Control.Monad.Trans.Class (lift)

type StateError a = StateT State (Either Error) a

runStateError :: StateError a -> State -> Either Error (a, State)
runStateError = runStateT

throw :: Error -> StateError a
throw err = lift (Left err)

