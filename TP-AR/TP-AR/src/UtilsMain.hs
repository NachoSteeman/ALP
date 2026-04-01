module UtilsMain
  ( executeTop
  , compileExpr
  , compileFile
  , compileFiles
  , assignRel
  , createRel
  , insertRel
  , dropRel
  , defineOp
  , execute
  , helpText
  , bienvenida
  , iprompt
  , prelude
  , trim
  , ioExceptionCatcher
  ) where

import qualified Data.Map as Map
import qualified Data.Set as Set

import System.IO (hPutStr, stderr)
import System.Directory (doesFileExist)

import Data.Char (isSpace)

import System.Console.Haskeline

import Control.Exception (catch, IOException)

import Control.Monad (when, foldM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)

import Commons
import Monads
import PrettyPrinter
import Parser
import Eval
import Optimizador

---------------------------------------------------------------
-- Operaciones para el contexto:
---------------------------------------------------------------


-- Para manejar definicion de operaciones:
defineOp :: NombreOp -> Expr -> StateError ()
defineOp name expr = do
  st <- get
  let c   = ctxt st
      ops = operaciones c
  when (Map.member name ops) $ throw (OperacionYaExiste name)

  let newOps  = Map.insert name expr ops
      newCtxt = c { operaciones = newOps }
  put st { ctxt = newCtxt }


-- Para crear una relacion:
createRel :: NombreRel -> [(Atributo, Type)] -> StateError ()
createRel name attrs = do
  st <- get
  let c    = ctxt st
      rels = relaciones c
  when (Map.member name rels) $ throw (RelacionYaExiste name)
  let nuevaRel = R name attrs Set.empty
      newRels  = Map.insert name nuevaRel rels
      newCtxt  = c { relaciones = newRels }
  put st { ctxt = newCtxt }


insertRel :: NombreRel -> [[Valor]] -> StateError ()
insertRel name valss = do
  st <- get
  let c    = ctxt st
      rels = relaciones c
  case Map.lookup name rels of
    Nothing -> throw (RelacionNoExiste name)
    Just (R relName attrs oldTups) -> do
      when (any (\vs -> length vs /= length attrs) valss) $
        throw EsquemaIncompatible
      when (not $ all (checkTupleTypes attrs) valss) $
        throw TiposIncompatibles
      let newTuplas = map (\vs -> Map.fromList (zip (map fst attrs) vs)) valss
          newRel    = R relName attrs (Set.union oldTups (Set.fromList newTuplas))
      put st { ctxt = c { relaciones = Map.insert name newRel rels } }


checkTupleTypes ::  [(Atributo, Type)] ->  [Valor] -> Bool
checkTupleTypes [] [] = True 
checkTupleTypes ((_,t):attrs) (v:vs) = checkType t v && checkTupleTypes attrs vs
checkTupleTypes _ _ = False


checkType :: Type -> Valor -> Bool
checkType PInt (VInt _) = True
checkType PString (VString _) = True
checkType PBool (VBool _) = True
checkType _ _ = False



dropRel :: NombreRel -> StateError ()
dropRel name = do
  st <- get
  let c    = ctxt st
      rels = relaciones c
  when (not (Map.member name rels)) $
    throw (RelacionNoExiste name)
  let newRels = Map.delete name rels
      newCtxt = c { relaciones = newRels }
  put st { ctxt = newCtxt }


-- Para realizar una vista:
assignRel :: NombreRel -> Expr -> StateError ()
assignRel name expr = do
  optExpr <- optimizador expr
  rel <- evalExpr optExpr   -- optimizamos y evaluamos la expresión
  st  <- get
  
  let c     = ctxt st
      rels  = relaciones c
      -- insertamos o reemplazamos:
      newRel  = rel { nombre = name }
      newRels = Map.insert name newRel rels
      newCtxt = c { relaciones = newRels }
  put st { ctxt = newCtxt }


-- Ejecuta una expresion, si la pudo evaluar bien retorna el nuevo estado y muestra el resultado de la operacion 
execute :: Expr -> State -> IO State
execute expr st =
  case runStateError (optimizador expr >>= evalExpr) st of
    Left err -> do
      putStrLn ("Error: " ++ show err)
      return st
    Right (rel, newSt) -> do
      putStrLn (prettyRelacion rel)
      return newSt


executeTop :: State -> TopLevel -> InputT IO (Maybe State)
executeTop st top = case top of

  TExpr e ->
    Just <$> lift (execute e st)

  TAssign name expr ->
    case runStateError (assignRel name expr) st of
      Left err      -> lift $ putStrLn ("Error: " ++ show err) >> return (Just st)
      Right (_, s') -> lift $ putStrLn ("Relación `" ++ name ++ "` definida.") >> return (Just s')

  TCmd cmd -> executeCmd st cmd


executeCmd :: State -> Command -> InputT IO (Maybe State) -- Maybe porque si nos llega el comando quit salimos 
executeCmd st cmd = case cmd of

  Quit ->
    lift $ putStrLn "¡Hasta luego!" >> return Nothing

  Help ->
    lift $ putStrLn helpText >> return (Just st)

  Clear ->
    lift $ putStr "\ESC[2J\ESC[H" >> return (Just st)

  Browse ->
    lift $ putStr (prettyContext (ctxt st)) >> return (Just st)

  Reload ->
    case lfile st of
      "" -> lift $ putStrLn "No hay archivo cargado." >> return (Just st)
      f  -> Just <$> compileFile st f

  Compile f ->
    Just <$> compileFile (st { lfile = f }) f

  CreateRel name attrs ->
    case runStateError (createRel name attrs) st of
      Left err      -> lift $ putStrLn ("Error: " ++ show err) >> return (Just st)
      Right (_, s') -> lift $ putStrLn ("Relación `" ++ name ++ "` creada.") >> return (Just s')

  InsertRel name valss ->
    case runStateError (insertRel name valss) st of
      Left err      -> lift $ putStrLn ("Error: " ++ show err) >> return (Just st)
      Right (_, s') -> lift $ putStrLn ("Tuplas agregadas a `" ++ name ++ "`.") >> return (Just s')

  DropRel name ->
    case runStateError (dropRel name) st of
      Left err      -> lift $ putStrLn ("Error: " ++ show err) >> return (Just st)
      Right (_, s') -> lift $ putStrLn ("Relación `" ++ name ++ "` eliminada.") >> return (Just s')

  DefineOP name expr ->
    case runStateError (defineOp name expr) st of
      Left err      -> lift $ putStrLn ("Error: " ++ show err) >> return (Just st)
      Right (_, s') -> lift $ putStrLn ("Operación `" ++ name ++ "` definida.") >> return (Just s')

---------------------------------------------------------------
-- Para archivos:
---------------------------------------------------------------

compileFiles :: [String] -> State -> InputT IO State
compileFiles xs s = foldM step s xs
  where
    step st file = do
      exists <- liftIO (doesFileExist file)
      if exists then compileFile st file
                else return st


compileFile :: State -> String -> InputT IO State
compileFile state f = do
  lift $ putStrLn ("Abriendo " ++ f ++ "...")
  let f' = trim f
  x <- lift $ Control.Exception.catch (readFile f')
    (\e -> do
      let err = show (e :: IOException)
      hPutStr stderr ("No se pudo abrir el archivo " ++ f' ++ ": " ++ err ++ "\n")
      return ""
    )
  case parse' x of
    Left err  -> outputStrLn (f' ++ ": " ++ err) >> return state
    Right tops -> foldM step state tops
  where
    step st top = do
      mst <- executeTop st top
      case mst of
        Nothing  -> return st   -- :quit dentro de archivo no mata el proceso
        Just st' -> return st'


compileExpr :: State -> String -> InputT IO (Maybe State)
compileExpr state x =
  case parse' x of
    Left err   -> outputStrLn err >> return (Just state)
    Right tops -> foldM step (Just state) tops
  where
    step Nothing  _   = return Nothing
    step (Just s) top = executeTop s top

---------------------------------------------------------------
-- Helpers
---------------------------------------------------------------

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

helpText :: String
helpText = unlines
  [ "Comandos disponibles:"
  , "  :help                          Mostrar esta ayuda"
  , "  :quit                          Salir del intérprete"
  , "  :clear                         Limpiar la consola"
  , "  :browse                        Ver relaciones y operaciones en scope"
  , "  :compile \"archivo\"             Cargar un archivo"
  , "  :reload                        Recargar el último archivo"
  , "  :createRel nombre a:t, b:t     Crear una relación"
  , "  :insertRel nombre v,v ; v,v    Insertar tuplas"
  , "  :dropRel nombre                Eliminar una relación"
  , "  :defineOP nombre expr          Definir una operación"
  , "  nombre = expr                  Definir una vista"
  , "  expr                           Evaluar una expresión"
  ]

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

iprompt :: String
iprompt = "AR> "

prelude :: String
prelude = "Ejemplos/relacionesBase.ar"

ioExceptionCatcher :: IOException -> IO (Maybe a)
ioExceptionCatcher _ = return Nothing
