algebra-relacional/
├── src/
│   ├── Types.hs           -- Tipos de datos fundamentales
│   ├── Parser.hs          -- Parser de relaciones y queries
│   ├── Operations.hs      -- Operaciones del álgebra
│   ├── Evaluator.hs       -- Evaluador de expresiones
│   ├── PrettyPrint.hs     -- Pretty printers
│   ├── Optimizer.hs       -- (Opcional) Optimización de queries
│   └── Main.hs            -- REPL/CLI
├── data/
│   ├── employees.rel      -- Relaciones de ejemplo
│   └── departments.rel
├── tests/
│   └── Spec.hs
└── README.md

📦 1. Tipos de Datos (Types.hs)

module Types where

import qualified Data.Set as Set
import qualified Data.Map as Map

-- ============================================================================
-- TIPOS BÁSICOS
-- ============================================================================

-- Nombre de atributo (columna)
type AttributeName = String

-- Valor en una celda (simplificado)
data Value = VInt Int
           | VString String
           | VBool Bool
           | VNull
           deriving (Eq, Ord, Show)

-- Tupla: mapeo de atributo → valor
type Tuple = Map.Map AttributeName Value

-- Relación: conjunto de tuplas + esquema
data Relation = Relation
    { schema :: Set.Set AttributeName  -- Nombres de columnas
    , tuples :: Set.Set Tuple          -- Filas
    } deriving (Eq, Show)

-- ============================================================================
-- ESQUEMA
-- ============================================================================

-- Tipo de dato de una columna (opcional, para validación)
data DataType = TInt | TString | TBool
    deriving (Eq, Show)

type Schema = Map.Map AttributeName DataType

-- ============================================================================
-- EXPRESIONES DEL ÁLGEBRA
-- ============================================================================

data Expr 
    -- Relación base (cargada desde archivo o definida)
    = ERelation String
    
    -- Operaciones fundamentales
    | ESelect Predicate Expr                    -- σ
    | EProject [AttributeName] Expr             -- π
    | ERename AttributeName AttributeName Expr  -- ρ
    | EProduct Expr Expr                        -- ×
    | EUnion Expr Expr                          -- ∪
    | EDifference Expr Expr                     -- −
    
    -- Operaciones derivadas
    | EIntersect Expr Expr                      -- ∩
    | EJoin Predicate Expr Expr                 -- ⋈
    | ENaturalJoin Expr Expr                    -- ⋈ (natural)
    | EDivision Expr Expr                       -- ÷
    | EAggregate [AttributeName]                -- Agrupamiento
                 [(AggFunction, AttributeName)] 
                 Expr
    
    deriving (Eq, Show)

-- ============================================================================
-- PREDICADOS (para SELECT y JOIN)
-- ============================================================================

data Predicate
    = PTrue
    | PFalse
    | PEq AttributeName Value                -- attr = valor
    | PNeq AttributeName Value               -- attr ≠ valor
    | PLt AttributeName Value                -- attr < valor
    | PGt AttributeName Value                -- attr > valor
    | PAttrEq AttributeName AttributeName    -- attr1 = attr2
    | PAnd Predicate Predicate               -- p1 AND p2
    | POr Predicate Predicate                -- p1 OR p2
    | PNot Predicate                         -- NOT p
    deriving (Eq, Show)

-- ============================================================================
-- FUNCIONES DE AGREGACIÓN
-- ============================================================================

data AggFunction
    = Count
    | Sum
    | Avg
    | Min
    | Max
    deriving (Eq, Show)

-- ============================================================================
-- CONTEXTO (ambiente de ejecución)
-- ============================================================================

-- Mapa de nombres de relaciones a relaciones
type Context = Map.Map String Relation

-- Mapa de macros/vistas definidas por el usuario
type MacroContext = Map.Map String Expr


⚙️ 2. Operaciones Fundamentales (Operations.hs)

module Operations where

import Types
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)

-- ============================================================================
-- OPERACIONES FUNDAMENTALES
-- ============================================================================

-- ----------------------------------------------------------------------------
-- SELECCIÓN (σ): filtra tuplas que cumplen el predicado
-- ----------------------------------------------------------------------------
selection :: Predicate -> Relation -> Relation
selection pred rel@(Relation sch tups) =
    Relation sch (Set.filter (evalPredicate pred) tups)

-- Evaluar predicado sobre una tupla
evalPredicate :: Predicate -> Tuple -> Bool
evalPredicate PTrue _ = True
evalPredicate PFalse _ = False

evalPredicate (PEq attr val) tup =
    case Map.lookup attr tup of
        Just v  -> v == val
        Nothing -> False

evalPredicate (PNeq attr val) tup =
    case Map.lookup attr tup of
        Just v  -> v /= val
        Nothing -> False

evalPredicate (PLt attr val) tup =
    case Map.lookup attr tup of
        Just v  -> v < val
        Nothing -> False

evalPredicate (PGt attr val) tup =
    case Map.lookup attr tup of
        Just v  -> v > val
        Nothing -> False

evalPredicate (PAttrEq attr1 attr2) tup =
    case (Map.lookup attr1 tup, Map.lookup attr2 tup) of
        (Just v1, Just v2) -> v1 == v2
        _                  -> False

evalPredicate (PAnd p1 p2) tup =
    evalPredicate p1 tup && evalPredicate p2 tup

evalPredicate (POr p1 p2) tup =
    evalPredicate p1 tup || evalPredicate p2 tup

evalPredicate (PNot p) tup =
    not (evalPredicate p tup)


-- ----------------------------------------------------------------------------
-- PROYECCIÓN (π): selecciona solo ciertas columnas
-- ----------------------------------------------------------------------------
projection :: [AttributeName] -> Relation -> Either String Relation
projection attrs (Relation sch tups)
    -- Validar que todos los atributos existen
    | not (all (`Set.member` sch) attrs) =
        Left $ "Atributos no existentes: " ++ show (filter (`Set.notMember` sch) attrs)
    | otherwise =
        let newSchema = Set.fromList attrs
            newTuples = Set.map (projectTuple attrs) tups
        in Right $ Relation newSchema newTuples

projectTuple :: [AttributeName] -> Tuple -> Tuple
projectTuple attrs tup = Map.filterWithKey (\k _ -> k `elem` attrs) tup


-- ----------------------------------------------------------------------------
-- RENOMBRAMIENTO (ρ): cambia el nombre de un atributo
-- ----------------------------------------------------------------------------
rename :: AttributeName -> AttributeName -> Relation -> Either String Relation
rename oldName newName (Relation sch tups)
    | not (Set.member oldName sch) =
        Left $ "Atributo '" ++ oldName ++ "' no existe"
    | Set.member newName sch && oldName /= newName =
        Left $ "Atributo '" ++ newName ++ "' ya existe"
    | otherwise =
        let newSchema = Set.insert newName (Set.delete oldName sch)
            newTuples = Set.map (renameTuple oldName newName) tups
        in Right $ Relation newSchema newTuples

renameTuple :: AttributeName -> AttributeName -> Tuple -> Tuple
renameTuple old new tup =
    case Map.lookup old tup of
        Nothing  -> tup
        Just val -> Map.insert new val (Map.delete old tup)


-- ----------------------------------------------------------------------------
-- PRODUCTO CARTESIANO (×): todas las combinaciones de tuplas
-- ----------------------------------------------------------------------------
product :: Relation -> Relation -> Either String Relation
product (Relation sch1 tups1) (Relation sch2 tups2)
    -- Verificar que no haya atributos en común (o renombrar automáticamente)
    | not (Set.null (Set.intersection sch1 sch2)) =
        Left $ "Esquemas tienen atributos en común: " ++ 
               show (Set.toList (Set.intersection sch1 sch2))
    | otherwise =
        let newSchema = Set.union sch1 sch2
            newTuples = Set.fromList 
                [ Map.union t1 t2 
                | t1 <- Set.toList tups1
                , t2 <- Set.toList tups2
                ]
        in Right $ Relation newSchema newTuples


-- ----------------------------------------------------------------------------
-- UNIÓN (∪): combina tuplas de ambas relaciones
-- ----------------------------------------------------------------------------
union :: Relation -> Relation -> Either String Relation
union (Relation sch1 tups1) (Relation sch2 tups2)
    | sch1 /= sch2 =
        Left "Las relaciones deben tener el mismo esquema"
    | otherwise =
        Right $ Relation sch1 (Set.union tups1 tups2)


-- ----------------------------------------------------------------------------
-- DIFERENCIA (−): tuplas en R1 pero no en R2
-- ----------------------------------------------------------------------------
difference :: Relation -> Relation -> Either String Relation
difference (Relation sch1 tups1) (Relation sch2 tups2)
    | sch1 /= sch2 =
        Left "Las relaciones deben tener el mismo esquema"
    | otherwise =
        Right $ Relation sch1 (Set.difference tups1 tups2)


-- ============================================================================
-- OPERACIONES DERIVADAS
-- ============================================================================

-- ----------------------------------------------------------------------------
-- INTERSECCIÓN (∩): R1 − (R1 − R2)
-- ----------------------------------------------------------------------------
intersection :: Relation -> Relation -> Either String Relation
intersection r1 r2 = do
    diff <- difference r1 r2
    difference r1 diff


-- ----------------------------------------------------------------------------
-- JOIN (⋈): producto cartesiano + selección
-- ----------------------------------------------------------------------------
joinOn :: Predicate -> Relation -> Relation -> Either String Relation
joinOn pred r1 r2 = do
    prod <- product r1 r2
    return $ selection pred prod


-- ----------------------------------------------------------------------------
-- NATURAL JOIN (⋈): join automático en atributos comunes
-- ----------------------------------------------------------------------------
naturalJoin :: Relation -> Relation -> Either String Relation
naturalJoin (Relation sch1 tups1) (Relation sch2 tups2) =
    let commonAttrs = Set.intersection sch1 sch2
        newSchema = Set.union sch1 sch2
        
        -- Tuplas compatibles son aquellas que coinciden en atributos comunes
        compatible t1 t2 =
            all (\attr -> Map.lookup attr t1 == Map.lookup attr t2) 
                (Set.toList commonAttrs)
        
        newTuples = Set.fromList
            [ Map.union t1 t2
            | t1 <- Set.toList tups1
            , t2 <- Set.toList tups2
            , compatible t1 t2
            ]
    in Right $ Relation newSchema newTuples


-- ----------------------------------------------------------------------------
-- DIVISIÓN (÷): R ÷ S = tuplas de R que están con TODAS las de S
-- ----------------------------------------------------------------------------
division :: Relation -> Relation -> Either String Relation
division (Relation schR tupsR) (Relation schS tupsS)
    | not (Set.isSubsetOf schS schR) =
        Left "El esquema del divisor debe ser subconjunto del dividendo"
    | otherwise =
        let attrsResult = Set.difference schR schS
            
            -- Proyectar R sobre atributos resultado
            projR = Set.map (projectTuple (Set.toList attrsResult)) tupsR
            
            -- Para cada tupla candidata, verificar que esté con todas las de S
            candidates = Set.filter (matchesAll tupsR tupsS schS) projR
            
        in Right $ Relation attrsResult candidates
  where
    matchesAll tupsR tupsS schS candidate =
        all (\tupS -> existsCombination tupsR candidate tupS) (Set.toList tupsS)
    
    existsCombination tupsR candidate tupS =
        let combined = Map.union candidate tupS
        in Set.member combined tupsR


-- ----------------------------------------------------------------------------
-- AGRUPAMIENTO + AGREGACIÓN (GROUP BY)
-- ----------------------------------------------------------------------------
aggregate :: [AttributeName]                     -- Atributos de agrupamiento
          -> [(AggFunction, AttributeName)]      -- Funciones de agregación
          -> Relation 
          -> Either String Relation
aggregate groupAttrs aggFuncs (Relation sch tups)
    | not (all (`Set.member` sch) groupAttrs) =
        Left "Atributos de agrupamiento no existen"
    | otherwise =
        let groups = groupBy groupAttrs tups
            resultTuples = Set.map (computeAggregates aggFuncs) groups
            resultSchema = Set.fromList (groupAttrs ++ map snd aggFuncs)
        in Right $ Relation resultSchema resultTuples

-- Agrupar tuplas por valores de ciertos atributos
groupBy :: [AttributeName] -> Set.Set Tuple -> Set.Set (Set.Set Tuple)
groupBy attrs tups =
    let grouped = Map.elems $ 
            Set.foldr insertGroup Map.empty tups
    in Set.fromList grouped
  where
    insertGroup tup acc =
        let key = projectTuple attrs tup
        in Map.insertWith Set.union key (Set.singleton tup) acc

-- Computar agregaciones sobre un grupo
computeAggregates :: [(AggFunction, AttributeName)] 
                  -> Set.Set Tuple 
                  -> Tuple
computeAggregates aggFuncs group =
    let groupList = Set.toList group
        results = map (applyAgg groupList) aggFuncs
    in Map.fromList results

applyAgg :: [Tuple] -> (AggFunction, AttributeName) -> (AttributeName, Value)
applyAgg tups (func, attr) =
    let values = mapMaybe (Map.lookup attr) tups
    in case func of
        Count -> (attr ++ "_count", VInt (length values))
        Sum   -> (attr ++ "_sum", sumValues values)
        Avg   -> (attr ++ "_avg", avgValues values)
        Min   -> (attr ++ "_min", minValue values)
        Max   -> (attr ++ "_max", maxValue values)

sumValues, avgValues, minValue, maxValue :: [Value] -> Value
sumValues vals = VInt (sum [n | VInt n <- vals])
avgValues vals = let ns = [n | VInt n <- vals]
                 in VInt (sum ns `div` length ns)
minValue vals = minimum vals
maxValue vals = maximum vals

🔍 3. Evaluador (Evaluator.hs)
module Evaluator where

import Types
import Operations
import qualified Data.Map as Map

-- Evaluar una expresión del álgebra en un contexto
eval :: Context -> MacroContext -> Expr -> Either String Relation
eval ctx macros expr = case expr of
    -- Relación base
    ERelation name ->
        case Map.lookup name ctx of
            Just rel -> Right rel
            Nothing  -> 
                -- Buscar en macros
                case Map.lookup name macros of
                    Just e  -> eval ctx macros e
                    Nothing -> Left $ "Relación no encontrada: " ++ name
    
    -- Operaciones fundamentales
    ESelect pred e -> do
        rel <- eval ctx macros e
        return $ selection pred rel
    
    EProject attrs e -> do
        rel <- eval ctx macros e
        projection attrs rel
    
    ERename old new e -> do
        rel <- eval ctx macros e
        rename old new rel
    
    EProduct e1 e2 -> do
        r1 <- eval ctx macros e1
        r2 <- eval ctx macros e2
        product r1 r2
    
    EUnion e1 e2 -> do
        r1 <- eval ctx macros e1
        r2 <- eval ctx macros e2
        Operations.union r1 r2
    
    EDifference e1 e2 -> do
        r1 <- eval ctx macros e1
        r2 <- eval ctx macros e2
        difference r1 r2
    
    -- Operaciones derivadas
    EIntersect e1 e2 -> do
        r1 <- eval ctx macros e1
        r2 <- eval ctx macros e2
        intersection r1 r2
    
    EJoin pred e1 e2 -> do
        r1 <- eval ctx macros e1
        r2 <- eval ctx macros e2
        joinOn pred r1 r2
    
    ENaturalJoin e1 e2 -> do
        r1 <- eval ctx macros e1
        r2 <- eval ctx macros e2
        naturalJoin r1 r2
    
    EDivision e1 e2 -> do
        r1 <- eval ctx macros e1
        r2 <- eval ctx macros e2
        division r1 r2
    
    EAggregate groupAttrs aggFuncs e -> do
        rel <- eval ctx macros e
        aggregate groupAttrs aggFuncs rel

🎨 4. Pretty Printer (PrettyPrint.hs)
module PrettyPrint where

import Types
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List (intercalate)

-- Imprimir relación como tabla ASCII
printRelation :: Relation -> String
printRelation (Relation sch tups)
    | Set.null tups = "(relación vacía con esquema: " ++ show (Set.toList sch) ++ ")"
    | otherwise =
        let attrs = Set.toList sch
            header = "| " ++ intercalate " | " attrs ++ " |"
            separator = "+" ++ concat (replicate (length attrs) "--------+")
            rows = map (printTuple attrs) (Set.toList tups)
        in unlines ([separator, header, separator] ++ rows ++ [separator])

printTuple :: [AttributeName] -> Tuple -> String
printTuple attrs tup =
    let values = map (\attr -> printValue (Map.lookup attr tup)) attrs
    in "| " ++ intercalate " | " values ++ " |"

printValue :: Maybe Value -> String
printValue Nothing = "NULL"
printValue (Just (VInt n)) = show n
printValue (Just (VString s)) = s
printValue (Just (VBool b)) = show b
printValue (Just VNull) = "NULL"

-- Imprimir expresión (para debugging)
printExpr :: Expr -> String
printExpr (ERelation name) = name
printExpr (ESelect _ e) = "σ(" ++ printExpr e ++ ")"
printExpr (EProject attrs e) = "π[" ++ intercalate "," attrs ++ "](" ++ printExpr e ++ ")"
printExpr (EProduct e1 e2) = "(" ++ printExpr e1 ++ " × " ++ printExpr e2 ++ ")"
printExpr (EUnion e1 e2) = "(" ++ printExpr e1 ++ " ∪ " ++ printExpr e2 ++ ")"
-- ... etc

📝 5. Parser Simple (Parser.hs)
module Parser where

import Types
import Text.Parsec
import qualified Data.Set as Set
import qualified Data.Map as Map

-- Parser básico usando Parsec
type Parser = Parsec String ()

-- Parsear archivo de relación
-- Formato:
-- SCHEMA: name, age, dept
-- (Alice, 30, IT)
-- (Bob, 25, HR)
parseRelationFile :: String -> Either String Relation
parseRelationFile input =
    case parse relationParser "" input of
        Left err -> Left (show err)
        Right rel -> Right rel

relationParser :: Parser Relation
relationParser = do
    schema <- schemaParser
    tuples <- many tupleParser
    return $ Relation (Set.fromList schema) (Set.fromList tuples)

schemaParser :: Parser [AttributeName]
schemaParser = do
    string "SCHEMA:"
    spaces
    attrs <- sepBy1 attributeName (char ',' >> spaces)
    newline
    return attrs

attributeName :: Parser String
attributeName = many1 (alphaNum <|> char '_')

tupleParser :: Parser Tuple
tupleParser = do
    char '('
    values <- sepBy1 valueParser (char ',' >> spaces)
    char ')'
    spaces
    -- Aquí necesitas el esquema para mapear valores a atributos
    -- En implementación real, pasa el esquema como parámetro
    return Map.empty  -- Placeholder

valueParser :: Parser Value
valueParser = 
    try (VInt <$> read <$> many1 digit) <|>
    (VString <$> many1 (noneOf ",()"))



🚀 6. REPL/CLI (Main.hs)
module Main where

import Types
import Operations
import Evaluator
import PrettyPrint
import Parser
import System.IO
import qualified Data.Map as Map

main :: IO ()
main = do
    putStrLn "Intérprete de Álgebra Relacional"
    putStrLn "Escribe 'help' para ayuda, 'quit' para salir"
    repl Map.empty Map.empty

repl :: Context -> MacroContext -> IO ()
repl ctx macros = do
    putStr "> "
    hFlush stdout
    line <- getLine
    case words line of
        ["quit"] -> putStrLn "¡Adiós!"
        ["help"] -> printHelp >> repl ctx macros
        ["load", file, name] -> do
            result <- loadRelation file name ctx
            case result of
                Left err -> putStrLn ("Error: " ++ err) >> repl ctx macros
                Right newCtx -> repl newCtx macros
        ("define":rest) -> do
            -- Definir macro
            repl ctx macros
        _ -> do
            -- Evaluar expresión
            putStrLn "Evaluando..."
            repl ctx macros

printHelp :: IO ()
printHelp = putStrLn $ unlines
    [ "Comandos disponibles:"
    , "  load <archivo> <nombre>  - Cargar relación desde archivo"
    , "  define <nombre> = <expr> - Definir macro/vista"
    , "  <expr>                   - Evaluar expresión"
    , "  quit                     - Salir"
    ]

loadRelation :: FilePath -> String -> Context -> IO (Either String Context)
loadRelation path name ctx = do
    content <- readFile path
    case parseRelationFile content of
        Left err -> return $ Left err
        Right rel -> return $ Right (Map.insert name rel ctx)