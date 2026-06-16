module Monads (
  StateError,
  runStateError,
  throw,
  
  -- De Monad.Trans.State
  get,
  put,
  modify,

  -- Helpers para el contexto
  getContext,
  getRels,
  getOps,

  modifyContext,
  modifyRels,
  modifyOps

) where

import Commons

import Control.Monad.Trans.State (StateT, runStateT, get, put, modify)
import Control.Monad.Trans.Class (lift)

type StateError a = StateT State (Either Error) a

runStateError :: StateError a -> State -> Either Error (a, State)
runStateError = runStateT


throw :: Error -> StateError a
throw err = lift (Left err)


-- Para obtener el contexto, relaciones y operaciones:
getContext :: StateError Context
getContext = ctxt <$> get

getRels :: StateError EnvRel
getRels = relaciones <$> getContext

getOps :: StateError EnvOp
getOps = operaciones <$> getContext


-- Para modificar el contexto, relaciones y operaciones:
modifyContext :: (Context -> Context) -> StateError ()
modifyContext f = modify (\s -> s { ctxt = f (ctxt s) })

modifyRels :: (EnvRel -> EnvRel) -> StateError ()
modifyRels f = modifyContext (\c -> c { relaciones = f (relaciones c) })

modifyOps :: (EnvOp -> EnvOp) -> StateError ()
modifyOps f = modifyContext (\c -> c { operaciones = f (operaciones c) }) 


