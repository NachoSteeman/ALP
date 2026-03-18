module Main where



import           Data.Maybe
import           Prelude                 hiding ( print )


import           System.Environment


import System.Console.Haskeline

import qualified Control.Monad.Catch as MC
import Control.Monad (when)

import Data.List (isPrefixOf, intersperse)
import Data.Char (isSpace)

import Control.Monad.Trans.Class (lift)


import Commons

import PrettyPrinter
import Monads
import UtilsMain

---------------------
--- Interpreter
---------------------
-- parse -> Expr -> eval -> prettyRelacion

main :: IO ()
main = runInputT defaultSettings main'

main' :: InputT IO ()
main' = do
  args <- lift getArgs
  readevalprint args (S True "" emptyContext) -- Llamo al loop principal



--  read-eval-print loop
readevalprint :: [String] -> State -> InputT IO ()
readevalprint args state@(S inter lfile _) =
  let rec st = do
        mx <- MC.catch
          -- Leo y proceso entrada capturando excepciones de ser necesario
          (if inter then getInputLine iprompt
                    else lift $ fmap Just getLine)
          (lift . ioExceptionCatcher)
        case mx of
          Nothing -> return ()
          Just "" -> rec st 
          Just x  -> do
            c   <- interpretCommand x
            st' <- handleCommand st c
            maybe (return ()) rec st'
  in  do
        state' <- compileFiles (prelude : args) state
        when inter $ lift $ bienvenida
        --  enter loop
        rec state' { inter = True }


--                ->  
--               /
-- Loop -> input -> Interpreto Comando -> handleo Comando
---------------------------------------------------------------
-- Interprerta los comandos que se escriben por consola:
---------------------------------------------------------------


interpretCommand :: String -> InputT IO Command
interpretCommand x = lift $ 
  -- Detectar asignación (para vistas): "nombre = expresion" 
  if isAssignment x
  then do
      let (name, rest) = break (== '=') x
          exprStr = tail rest  -- Quitamos el '='
      return (AssignRel (trim name) (trim exprStr))

  -- Si es un comando:
  else if isPrefixOf ":" x
  then do
    let (cmd, t') = break isSpace x
        t         = dropWhile isSpace t'
        matching = filter (\(Cmd cs _ _ _) -> any (isPrefixOf cmd) cs) commands
    case matching of
      [] -> do
        putStrLn
          ("Comando desconocido `" ++ cmd ++ "'. Escriba :? para recibir ayuda."
          )
        return Noop
      [Cmd _ _ f _] -> do
        return (f t)
      _ -> do
        putStrLn
          (  "Comando ambigüo, podría ser "
          ++ concat (intersperse ", " [ head cs | Cmd cs _ _ _ <- matching ])
          ++ "."
          )
        return Noop

  -- Si no machea con ningun comando de la terminal:
  else return (Compile (CompileInteractive x))



---------------------------------------------------------------
-- Maneja los comandos que previamente fueron interpretados:
---------------------------------------------------------------

handleCommand :: State -> Command -> InputT IO (Maybe State)
handleCommand state@(S inter lfile ctxt) cmd = case cmd of                -- VER

  Quit   -> lift $ when (not inter) (putStrLn "!@#$^&*") >> return Nothing
  
  Noop   -> return (Just state)
  
  Help   -> lift $ putStr (helpTxt commands) >> return (Just state)

  Clear  -> lift $ putStr "\ESC[2J\ESC[H" >> return (Just state)
  
  Browse -> lift $ do
    putStr (prettyContext ctxt )   -- VER (lift $) putStr ... ?
    return (Just state)
  
  Compile c -> do
    state' <- case c of
      CompileInteractive s -> compileExpr state s
      CompileFile        f -> compileFile (state { lfile = f }) f
    return (Just state')
  
  
  Recompile -> if null lfile
    then lift $ putStrLn "No hay un archivo cargado.\n" >> return (Just state)
    else handleCommand state (Compile (CompileFile lfile))
  
  FindExpr s -> do
    x' <- parseIO "<interactive>" s
    case x' of
      Nothing -> return (Just state)
      Just expr -> do
        newState <- lift $ execute expr state
        return (Just newState)

  DefineOP name exprStr -> do  -- Para no romper el parser lo paso como str a expr
  -- Parseo:
    mx <- parseIO "<defineOP>" exprStr
    case mx of
      -- Si hay error:
      Nothing -> return (Just state)
      -- Sino:
      Just expr ->
        case runStateError (defineOp name expr) state of
          Left err -> do
            lift $ putStrLn (show err)
            return (Just state)
          Right (_, newState) -> do
            lift $ putStrLn ("Operación `" ++ name ++ "` definida.")
            return (Just newState)
  
  CreateRel name attrs -> do
    case runStateError (createRel name attrs) state of
      Left err -> do
        lift $ putStrLn ("Error: " ++ show err)
        return (Just state)

      Right (_, newState) -> do
        lift $ putStrLn ("Relación `" ++ name ++ "` creada.")
        return (Just newState)

  
  InsertRel name tups -> do
    case runStateError (insertRel name tups) state of
      Left err -> do
        lift $ putStrLn ("Error: " ++ show err)
        return (Just state)

      Right (_, newState) -> do
        lift $ putStrLn ("Tuplas agregadas a `" ++ name ++ "`.")
        return (Just newState)


  -- Para asignar relaciones:
  AssignRel name exprStr -> do
    mx <- parseIO "<assign>" exprStr
    case mx of
      Nothing -> return (Just state)
      Just expr ->
        case runStateError (assignRel name expr) state of
          Left err -> do
            lift $ putStrLn ("Error: " ++ show err)
            return (Just state)
          Right (_, newState) -> do
            lift $ putStrLn ("Relación `" ++ name ++ "` definida.")
            return (Just newState)
  

-- Para eliminar relaciones:
  DropRel name ->
    case runStateError (dropRel name) state of
      Left err -> do
        lift $ putStrLn ("Error: " ++ show err)
        return (Just state)
      Right (_, newState) -> do
        lift $ putStrLn ("Relación `" ++ name ++ "` eliminada.")
        return (Just newState)

  _ -> do
        lift $ putStrLn "Comando no implementado."
        return (Just state)