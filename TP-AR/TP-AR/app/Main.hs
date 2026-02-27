module Main where

import           Control.Exception              ( catch
                                                , IOException
                                                )
import           Control.Monad.Except
import           Data.Char
import           Data.List
import           Data.Maybe
import           Prelude                 hiding ( print )
import           System.Console.Haskeline
import qualified Control.Monad.Catch           as MC

import           System.Environment
import           System.IO               hiding ( print )
import System.Directory (doesFileExist)

import           Text.PrettyPrint.HughesPJ      ( render
                                                , text
                                                )

import Control.Monad (when, foldM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)

import Control.Exception (SomeException)

import AST
import Parser
import Utils
import PrettyPrinter
import Eval
import Monads
---------------------
--- Interpreter
---------------------
-- parse → Expr → eval → prettyRelacion

main :: IO ()
main = runInputT defaultSettings main'

main' :: InputT IO ()
main' = do
  args <- lift getArgs
  readevalprint args (S True "" emptyContext)




bienvenida :: IO ()
bienvenida = do
  putStr "\ESC[2J\ESC[H" -- Para limpiar la consola
  putStrLn $ unlines
    [ "╔════════════════════════════════════════════════════════════════════════════════════╗"
    , "║ λλλ                              Relational Algebra                            λλλ ║"
    , "╠════════════════════════════════════════════════════════════════════════════════════╣"
    , "║                                                                                    ║"
    , "║                                                                                    ║"
    , "║                                                                                    ║"
    , "║                               Bienvenido al intérprete                             ║"
    , "║                                                                                    ║"
    , "║                                                                                    ║"
    , "║                                                                                    ║"
    , "╚════════════════════════════════════════════════════════════════════════════════════╝"
    , ""
    , "  Para más información ingrese :help"
    ]

iname, iprompt :: String
iname = "cálculo lambda simplemente tipado"
iprompt = "ST> "

ioExceptionCatcher :: IOException -> IO (Maybe a)
ioExceptionCatcher _ = return Nothing



--  read-eval-print loop
readevalprint :: [String] -> State -> InputT IO ()
readevalprint args state@(S inter lfile ctxt) =
  let rec st = do
        mx <- MC.catch
          (if inter then getInputLine iprompt else lift $ fmap Just getLine)
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

data Command = Compile CompileForm
              | Recompile
              | Browse
              | Quit
              | Help
              | Noop
              | FindExpr String

data CompileForm = CompileInteractive  String
                  | CompileFile         String

interpretCommand :: String -> InputT IO Command
interpretCommand x = lift $ if isPrefixOf ":" x
  then do
    let (cmd, t') = break isSpace x
    let t         = dropWhile isSpace t'
    --  find matching commands
    let matching = filter (\(Cmd cs _ _ _) -> any (isPrefixOf cmd) cs) commands
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
  else return (Compile (CompileInteractive x))



handleCommand :: State -> Command -> InputT IO (Maybe State)
handleCommand state@(S inter lfile ctxt) cmd = case cmd of                -- VER

  Quit   -> lift $ when (not inter) (putStrLn "!@#$^&*") >> return Nothing
  
  Noop   -> return (Just state)
  
  Help   -> lift $ putStr (helpTxt commands) >> return (Just state)
  
  Browse -> lift $ do
    putStr (prettyContext ctxt )   -- VER
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
      Just expr ->
        case runStateError (evalExpr expr) state of
          Left err -> do
            lift $ putStrLn ("Error: " ++ show err)
            return (Just state)

          Right (rel, newState) -> do
            lift $ putStrLn (prettyRelacion rel)
            return (Just newState)



data InteractiveCommand = Cmd [String] String (String -> Command) String

commands :: [InteractiveCommand]
commands =
  [ Cmd [":browse"] "" (const Browse) "Ver los nombres en scope"
  , Cmd [":load"]
        "<file>"
        (Compile . CompileFile)
        "Cargar un programa desde un archivo"
  , Cmd [":reload"]
        "<file>"
        (const Recompile)
        "Volver a cargar el último archivo"
  , Cmd [":quit"]       ""       (const Quit) "Salir del intérprete"
  , Cmd [":help", ":?"] ""       (const Help) "Mostrar esta lista de comandos"
  , Cmd [":type"]       "<term>" (FindExpr)   "Inferir el tipo de un término"
  ]

helpTxt :: [InteractiveCommand] -> String
helpTxt cs =
  "Lista de comandos:  Cualquier comando puede ser abreviado a :c donde\n"
    ++ "c es el primer caracter del nombre completo.\n\n"
    ++ "<expr>                  evaluar la expresión\n"
    ++ "def <var> = <expr>      definir una variable\n"
    ++ unlines
         (map
           (\(Cmd c a _ d) ->
             let
               ct =
                 concat
                   (intersperse ", "
                                (map (++ if null a then "" else " " ++ a) c)
                   )
             in  ct ++ replicate ((24 - length ct) `max` 2) ' ' ++ d
           )
           cs
         )





parseIO :: String -> String -> InputT IO (Maybe Expr)
parseIO msj input =
  MC.catch
    (do
        let ast = parse input
        return (Just ast)
    )
    handler
  where
    handler :: SomeException -> InputT IO (Maybe Expr)
    handler e = do
      outputStrLn (msj ++ ": " ++ show e)
      return Nothing

















-- Para que sea opcional el prelude
compileFiles xs s =
  foldM step s xs
  where
    step st file = do
      exists <- liftIO (doesFileExist file)
      if exists
        then compileFile st file
        else return st



compileFile :: State -> String -> InputT IO State
compileFile state@(S inter lfile v) f = do
  lift $ putStrLn ("Abriendo " ++ f ++ "...")
  let f' = reverse (dropWhile isSpace (reverse f))
  x <- lift $ Control.Exception.catch
    (readFile f')
    (\e -> do
      let err = show (e :: IOException)
      hPutStr stderr
              ("No se pudo abrir el archivo " ++ f' ++ ": " ++ err ++ "\n")
      return ""
    )
  expr <- parseIO f'  x   -- Parsear con lo mio 
  case expr of
    Nothing -> return state
    Just e  -> handleExpr state e




compileExpr :: State -> String -> InputT IO State
compileExpr state x = do
  mx <- parseIO "<interactive>" x
  case mx of
    Nothing -> return state                     -- VER
    Just e  -> handleExpr state e


handleExpr :: State -> Expr -> InputT IO State
handleExpr state expr =
  case runStateError (evalExpr expr) state of
    Left err -> do
      lift $ putStrLn ("Error: " ++ show err)
      return state

    Right (rel, newState) -> do
      lift $ putStrLn (prettyRelacion rel)
      return newState



prelude :: String
prelude = "Ejemplos/relacionesBase.hs"

it :: String
it = "it"


