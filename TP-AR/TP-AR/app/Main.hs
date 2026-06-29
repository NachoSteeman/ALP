module Main (main)where

import System.Environment
import System.Console.Haskeline
import qualified Control.Monad.Catch as MC
import Control.Monad (when)
import Control.Monad.Trans.Class (lift)

import Commons
import UtilsMain

main :: IO ()
main = runInputT defaultSettings main'

main' :: InputT IO ()
main' = do
  args <- lift getArgs
  readevalprint args (S True "" emptyContext)

readevalprint :: [String] -> State -> InputT IO ()
readevalprint args state =
  let rec st = do
        mx <- MC.catch
          (if inter st then getInputLine iprompt
                       else lift $ fmap Just getLine)
          (lift . ioExceptionCatcher)
        case mx of
          Nothing -> return ()
          Just "" -> rec st
          Just x  -> do
            mst' <- compileExpr st x
            case mst' of
              Nothing  -> return ()
              Just st' -> rec st'
  in do
    state' <- compileFiles (prelude : args) state
    when (inter state') $ lift bienvenida
    rec state' { inter = True }
    