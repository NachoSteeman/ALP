module Monads where

import AST
import Control.Monad -- Para ap
newtype StateError a = StateError {runStateError ::  State -> Either Error (a, State) }

instance Functor StateError where
  fmap f (StateError g) =
    StateError $ \st ->
      case g st of
        Left err        -> Left err
        Right (a, st')  -> Right (f a, st')

instance Applicative StateError where
  pure  = return
  (<*>) = ap

instance Monad StateError where 
  return a = StateError (\s-> Right (a, s))
  (StateError g) >>= f =
    StateError $ \st ->
      case g st of
        Left err        -> Left err
        Right (a, st')  ->
          runStateError (f a) st'



class Monad m => MonadError m where
    -- Lanza un error
    throw :: Error -> m a
instance MonadError StateError where
  throw err = StateError (\_ -> Left err)






class Monad m => MonadState m where
  get    :: m State
  put    :: State -> m ()
  modify :: (State -> State) -> m ()

instance MonadState StateError where
  get =
    StateError (\st -> Right (st, st))

  put newState =
    StateError (\_ -> Right ((), newState))

  modify f =
    StateError (\st -> Right ((), f st))