module Main where

import           Control.Exception              ( catch
                                                , IOException
                                                )

import Control.Exception (evaluate) -- Eso fuerza la evaluación dentro del catch.

import Text.Read (readMaybe)

import qualified Data.Map as Map
import qualified Data.Set as Set

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
isAssignment :: String -> Bool
isAssignment s =
    let trimmed = trim s
        hasEquals = '=' `elem` trimmed
        beforeEquals = takeWhile (/= '=') trimmed
        -- Es asignación si:
        -- 1. Tiene '='
        -- 2. Lo que está antes del '=' es un identificador simple (sin '[', '(', etc.)
        -- 3. No empieza con ':'
    in hasEquals 
       && not (isPrefixOf ":" trimmed)
       && all isValidIdChar beforeEquals
       && not (null beforeEquals)
  where
    isValidIdChar c = c `elem` (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_") || isSpace c


interpretCommand :: String -> InputT IO Command
interpretCommand x = lift $ 
  -- Detectar asignación: "nombre = expresion"
  if isAssignment x
  then do
      let (name, rest) = break (== '=') x
          exprStr = tail rest  -- Quitar el '='
      return (AssignRel (trim name) (trim exprStr))

  else if isPrefixOf ":" x
  -- Si tiene ":" como prefijo:
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


---------------------------------------------------------------
-- Operaciones para el contexto:
---------------------------------------------------------------
insertTupla :: [Atributo] -> [Valor] -> Either String Tupla
insertTupla attrs vals
    | length attrs /= length vals =
        Left "Cantidad de valores no coincide con atributos"
    | otherwise =
        Right $ Map.fromList (zip attrs vals)

insertar :: Relacion -> [[Valor]] -> Either String Relacion
insertar (R attrs ts nom) filas = do
    nuevas <- mapM (insertTupla attrs) filas
    let ts' = Set.union ts (Set.fromList nuevas)
    return (R attrs ts' nom)

-- Para manejar definicion de operaciones:

defineOp :: NombreOp -> Expr -> StateError ()
defineOp name expr = do
  st <- get
  let c       = ctxt st
      ops     = operaciones c
      
  when (Map.member name ops) $
      throw (OperacionYaExiste name)
  
  let newOps  = Map.insert name expr ops
      newCtxt = c { operaciones = newOps }
      
  put st { ctxt = newCtxt }


-- Para crear una relacion:

createRel :: NombreRel -> [Atributo] -> StateError ()
createRel name attrs = do
  st <- get
  let c     = ctxt st
      rels  = relaciones c

  when (Map.member name rels) $
    throw (RelacionYaExiste name)

  let nuevaRel = R attrs Set.empty name
      newRels  = Map.insert name nuevaRel rels
      newCtxt  = c { relaciones = newRels }

  put st { ctxt = newCtxt }


insertRel :: NombreRel -> [[String]] -> StateError ()
insertRel name rawTuplas = do
  st <- get
  let c     = ctxt st
      rels  = relaciones c

  case Map.lookup name rels of
    Nothing -> throw (RelacionNoExiste name)

    Just (R attrs oldTups rname) -> do

      let attrList =  attrs
          -- Validar longitud:
          valid = all (\vals -> length vals == length attrList) rawTuplas
      
      when (not valid) $ throw EsquemaIncompatible

      -- Construir Tuplas reales
      let newTuplas = map (\vals ->
                                    Map.fromList $ zip attrList (map parseValor vals)) rawTuplas

          newRel  = R attrs (Set.union oldTups (Set.fromList newTuplas)) rname
          newRels = Map.insert name newRel rels
          newCtxt = c { relaciones = newRels }
      put st { ctxt = newCtxt }


parseValor :: String -> Valor
parseValor s
  | map toLower s == "true"  = VBool True
  | map toLower s == "false" = VBool False
  | map toLower s == "null"  = VNull
  | otherwise =  case (readMaybe s) of
                    Just n  ->  VInt n
                    Nothing ->  VString s

dropRel :: NombreRel -> StateError ()
dropRel name = do
  st <- get
  let c     = ctxt st
      rels  = relaciones c

  when (not (Map.member name rels)) $
    throw (RelacionNoExiste name)

  let newRels = Map.delete name rels
      newCtxt = c { relaciones = newRels }

  put st { ctxt = newCtxt }

assignRel :: NombreRel -> Expr -> StateError ()
assignRel name expr = do
  rel <- evalExpr expr   -- evaluamos la expresión
  st  <- get
  let c     = ctxt st
      rels  = relaciones c
      -- insertamos o reemplazamos:
      newRel  = rel { nombre = name }
      newRels = Map.insert name newRel rels
      newCtxt = c { relaciones = newRels }
  put st { ctxt = newCtxt }


execute :: Expr -> State -> IO State
execute expr st =
  case runStateError (evalExpr expr) st of
    Left err -> do
      putStrLn ("Error: " ++ show err)
      return st
    Right (rel, newSt) -> do
      putStrLn (prettyRelacion rel)
      return newSt


parseIO :: String -> String -> InputT IO (Maybe Expr)
parseIO msj input =
  case parse input of
    Left err -> do
      outputStrLn (msj ++ ": " ++ err)
      return Nothing
    Right ast ->
      return (Just ast)



---------------------------------------------------------------
-- Para manejar comando help:
---------------------------------------------------------------

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

commands :: [InteractiveCommand]
commands =
  [ Cmd [":browse"] "" (const Browse) "Ver los nombres en scope"
  , Cmd [":load"]
        "<file>"
        (Compile . CompileFile)
        "Cargar un programa desde un archivo"
  
  , Cmd [":clear"] "" (const Clear) "Limpia la consola" 

  , Cmd [":reload"]
        "<file>"
        (const Recompile)
        "Volver a cargar el último archivo"
  , Cmd [":quit"]       ""       (const Quit) "Salir del intérprete"
  , Cmd [":help", ":?"] ""       (const Help) "Mostrar esta lista de comandos"
  , Cmd [":type"]       "<term>" (FindExpr)   "Inferir el tipo de un término"

  , Cmd [":defineOP"]  
        "<name> <expr>" 
        (\s ->
          let (name, rest) = break isSpace s
          in DefineOP name (dropWhile isSpace rest)
        )
        "Define un operador"


  , Cmd [":createRel"] 
        "<name> <atribs>" 
        (\s ->
          let (name, rest) = break isSpace s
              attrsStr = dropWhile isSpace rest
              attrs = parseAttrs attrsStr
          in CreateRel name attrs
        )
        "Crea una relacion con sus atributos"

  , Cmd [":insertRel"] 
        "<name> <tups>"   
        (\s ->
           let (name, rest) = break isSpace s
               tupsStr = dropWhile isSpace rest
               tups = parseTuplas tupsStr
           in InsertRel name tups
        )
         "Agrega tuplas a una relacion"
  
  , Cmd [":dropRel"]
      "<name>"
      (\s -> DropRel (trim s))
      "Elimina una relación"
  ]




splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn delim (c:cs)
  | c == delim = "" : rest
  | otherwise  = (c : head rest) : tail rest
  where
    rest = splitOn delim cs


parseAttrs :: String ->  [Atributo]
parseAttrs s = map trim (splitOn ',' s)

parseTuplas :: String -> [[String]]
parseTuplas s =
  map parseOneTuple $
    splitOn ';' s

parseOneTuple :: String -> [String]
parseOneTuple t =
  map trim (splitOn ',' t)


trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace










---------------------------------------------------------------
-- Para manejar el uso de archivos
---------------------------------------------------------------

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
handleExpr state expr = do
  newState <- lift $ execute expr state
  return newState

prelude :: String
prelude = "Ejemplos/relacionesBase.hs"

it :: String
it = "it"

















---------------------------------------------------------------
-- Para manejar comandos:
---------------------------------------------------------------
data InteractiveCommand = Cmd [String] String (String -> Command) String

data Command = -- Para comandos consola:
              Compile CompileForm
              | Clear
              | Recompile
              | Browse
              | Quit
              | Help
              | Noop

              -- 
              | FindExpr String
              | DefineOP NombreOp String
       
              -- Para trabajar con mis relaciones:
              | CreateRel NombreRel  [Atributo] 
              | InsertRel NombreRel [[String]]
              | DropRel NombreRel 
              | AssignRel NombreRel String

data CompileForm = CompileInteractive  String
                  | CompileFile         String

---------------------------------------------------------------
-- Para excepciones:
---------------------------------------------------------------
ioExceptionCatcher :: IOException -> IO (Maybe a)
ioExceptionCatcher _ = return Nothing



---------------------------------------------------------------
-- Para Mostrar mejor las cosas:
---------------------------------------------------------------
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
iprompt = "ST> "