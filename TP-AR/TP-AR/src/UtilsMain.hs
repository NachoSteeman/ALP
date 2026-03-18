module UtilsMain (
  -- Para comandos:
  InteractiveCommand(..),
  Command(..),
  helpTxt,
  commands,
  
  -- Para contexto:
  defineOp,
  createRel,
  insertRel,
  dropRel,
  assignRel,

  -- Para relizar una operacion:
  execute,

  -- Para manejar entrada:
  parseIO,
  compileExpr,
  isAssignment,

  bienvenida,
  iprompt,
  prelude,
  trim,


  -- Para archivos:
  CompileForm(..),
  compileFile,
  compileFiles,


  -- Para excepciones:
  ioExceptionCatcher


)
where

import qualified Data.Map as Map
import qualified Data.Set as Set

import System.IO (hPutStr, stderr)
import System.Directory (doesFileExist) 

import           Data.Char 
import           Data.List 

import           System.Console.Haskeline

import qualified Control.Monad.Catch           as MC  --
import           Control.Monad.Except --
import Control.Exception (evaluate, catch, IOException) -- evaluate fuerza la evaluación dentro del catch.

import Text.Read (readMaybe)


import Control.Monad (when, foldM)        --
import Control.Monad.Trans.Class (lift)  --
import Control.Monad.IO.Class (liftIO)  --

import Control.Exception (SomeException)

import Data.List (intercalate, nub)


import Commons
import Monads
import PrettyPrinter
import Parser
import Eval


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



-- Operaciones auxiliares para parserar algunas cosas de la entrada del REPL:
parseAttrs :: String -> [(Atributo,Type)]
parseAttrs s = map parseAttr (splitOn ',' s)

parseAttr :: String -> (Atributo,Type)
parseAttr str =
  case splitOn ':' (trim str) of
    [attr,typ] -> (trim attr, parseType (trim typ))
    _ -> error "Formato de atributo invalido. Use atributo:tipo"

parseType :: String -> Type
parseType s =
  case map toLower s of
    "int"    -> PInt
    "string" -> PString
    "bool"   -> PBool
    _ -> error ("Tipo desconocido: " ++ s)


parseTuplas :: String -> [[String]]
parseTuplas s =
  map parseOneTuple $
    splitOn ';' s

parseOneTuple :: String -> [String]
parseOneTuple t =
  map trim (splitOn ',' t)


splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn delim (c:cs)
  | c == delim = "" : rest
  | otherwise  = (c : head rest) : tail rest
  where
    rest = splitOn delim cs

-- Para eliminar espacios en blanco al principio y al final de un string
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace


---------------------------------------------------------------
-- Operaciones para el contexto:
---------------------------------------------------------------


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
createRel :: NombreRel -> [(Atributo, Type)] -> StateError ()
createRel name attrs = do
  st <- get
  let c     = ctxt st
      rels  = relaciones c

  when (Map.member name rels) $ throw (RelacionYaExiste name)

  let nuevaRel = R name attrs Set.empty 
      newRels  = Map.insert name nuevaRel rels
      newCtxt  = c { relaciones = newRels }

  put st { ctxt = newCtxt }



-- Para insertar tuplas en una relacion:
insertRel :: NombreRel -> [[String]] -> StateError ()
insertRel name rawTuplas = do
  st <- get
  let c     = ctxt st
      rels  = relaciones c

  case Map.lookup name rels of
    Nothing -> throw (RelacionNoExiste name)

    Just (R relName attrs oldTups ) -> do

      let attrList = attrs

      -- verificar longitud
      let validLength = all (\vals -> length vals == length attrList) rawTuplas
      when (not validLength) $
        throw EsquemaIncompatible

      -- parsear valores
      let parsedVals = map (map parseValor) rawTuplas

      -- verificar tipos
      let validTypes = all (checkTupleTypes attrList) parsedVals
      when (not validTypes) $
        throw EsquemaIncompatible

      -- construir tuplas
      let newTuplas =
            map (\vals ->
                  Map.fromList (zip (map fst attrList) vals)
                ) parsedVals

      let newRel =
            R relName
              attrs
              (Set.union oldTups (Set.fromList newTuplas))
              

      let newRels = Map.insert name newRel rels
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


checkType :: Type -> Valor -> Bool
checkType PInt (VInt _) = True
checkType PString (VString _) = True
checkType PBool (VBool _) = True
checkType _ _ = False

checkTupleTypes :: [(Atributo,Type)] -> [Valor] -> Bool
checkTupleTypes attrs vals =
  and $ zipWith checkType (map snd attrs) vals

-- Para eliminar una relacion:
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


-- Para realizar una vista:
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


-- Ejecuta una expresion, si la pudo evaluar bien retorna el nuevo estado y muestra el resultado de la operacion 
execute :: Expr -> State -> IO State
execute expr st =
  case runStateError (evalExpr expr) st of
    Left err -> do
      putStrLn ("Error: " ++ show err)
      return st
    Right (rel, newSt) -> do
      putStrLn (prettyRelacion rel)
      return newSt


-- Trata de parsear la entrada, si falla notifica el error, sino devuelve la expresion
parseIO :: String -> String -> InputT IO (Maybe Expr)
parseIO msg input =
  case parse input of
    Left err -> do
      outputStrLn (msg ++ ": " ++ err)
      return Nothing
    Right expr ->
      return (Just expr)


-- Funcion auxiliar para poder manejar las asignaciones en el REPL y reconocerlas:
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
  expr <- parseIO f'  x   
  case expr of
    Nothing -> return state
    Just e  -> handleExpr state e

compileExpr :: State -> String -> InputT IO State
compileExpr state x = do
  mx <- parseIO "<interactive>" x
  case mx of
    Nothing -> return state                     
    Just e  -> handleExpr state e


handleExpr :: State -> Expr -> InputT IO State
handleExpr state expr = do
  newState <- lift $ execute expr state
  return newState


prelude :: String
prelude = "Ejemplos/relacionesBase.hs"


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
iprompt = "AR> "
