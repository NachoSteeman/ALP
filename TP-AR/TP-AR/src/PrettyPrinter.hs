module PrettyPrinter
    ( prettyRelacion
    , prettyExpr
    , prettyCond
    , prettyValor
    , prettyTupla
    ) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List (intercalate, sort)
import Text.Printf (printf)

import AST  -- Importa tus tipos

-- ============================================================================
-- PRETTY PRINTER PARA RELACIONES
-- ============================================================================

-- Imprime una relación como tabla ASCII
prettyRelacion :: Relacion -> String
prettyRelacion (R attrs tuplas nom)
    | Set.null tuplas = 
        printf "Relación '%s' (vacía)\nEsquema: [%s]\n" 
               nom 
               (intercalate ", " (Set.toList attrs))
    | otherwise =
        let attrsList = sort (Set.toList attrs)
            header = makeHeader nom attrsList
            separator = makeSeparator attrsList
            rows = map (makeRow attrsList) (Set.toList tuplas)
        in unlines ([separator, header, separator] ++ rows ++ [separator])

-- Crear encabezado con nombre de la relación
makeHeader :: String -> [Atributo] -> String
makeHeader nom attrs =
    let cols = map (\a -> pad 15 a) attrs
    in "| " ++ intercalate " | " cols ++ " |  (" ++ nom ++ ")"

-- Crear separador
makeSeparator :: [Atributo] -> String
makeSeparator attrs =
    "+" ++ intercalate "+" (replicate (length attrs) (replicate 17 '-')) ++ "+"

-- Crear fila de datos
makeRow :: [Atributo] -> Tupla -> String
makeRow attrs tupla =
    let values = map (\attr -> prettyValorPadded 15 (Map.lookup attr tupla)) attrs
    in "| " ++ intercalate " | " values ++ " |"

-- Padding para alinear columnas
pad :: Int -> String -> String
pad n s 
    | length s >= n = take n s
    | otherwise     = s ++ replicate (n - length s) ' '

-- Pretty print de un valor con padding
prettyValorPadded :: Int -> Maybe Valor -> String
prettyValorPadded n Nothing = pad n "NULL"
prettyValorPadded n (Just v) = pad n (prettyValor v)


-- ============================================================================
-- PRETTY PRINTER PARA VALORES
-- ============================================================================

prettyValor :: Valor -> String
prettyValor (VInt n)    = show n
prettyValor (VString s) = "\"" ++ s ++ "\""
prettyValor (VBool b)   = if b then "true" else "false"
prettyValor VNull       = "NULL"


-- ============================================================================
-- PRETTY PRINTER PARA TUPLAS
-- ============================================================================

prettyTupla :: Tupla -> String
prettyTupla tupla =
    let pairs = Map.toList tupla
        strs = map (\(k, v) -> k ++ ": " ++ prettyValor v) pairs
    in "{ " ++ intercalate ", " strs ++ " }"


-- ============================================================================
-- PRETTY PRINTER PARA EXPRESIONES
-- ============================================================================

-- Imprime una expresión del álgebra con notación matemática
prettyExpr :: Expr -> String
prettyExpr expr = prettyExprPrec 0 expr

-- Pretty print con precedencia (para paréntesis)
prettyExprPrec :: Int -> Expr -> String
prettyExprPrec _ (ERelacion nombre) = nombre 

prettyExprPrec p (ESeleccion cond e) =
    parensIf (p > 5) $ "σ[" ++ prettyCond cond ++ "](" ++ prettyExprPrec 0 e ++ ")"

prettyExprPrec p (EProyeccion attrs e) =
    parensIf (p > 5) $ "π[" ++ intercalate ", " attrs ++ "](" ++ prettyExprPrec 0 e ++ ")"

prettyExprPrec p (ERenombre old new e) =
    parensIf (p > 5) $ "ρ[" ++ old ++ " → " ++ new ++ "](" ++ prettyExprPrec 0 e ++ ")"

prettyExprPrec p (EUnion e1 e2) =
    parensIf (p > 2) $ prettyExprPrec 3 e1 ++ " ∪ " ++ prettyExprPrec 3 e2

prettyExprPrec p (EDiff e1 e2) =
    parensIf (p > 2) $ prettyExprPrec 3 e1 ++ " − " ++ prettyExprPrec 3 e2

prettyExprPrec p (EProd e1 e2) =
    parensIf (p > 3) $ prettyExprPrec 4 e1 ++ " × " ++ prettyExprPrec 4 e2

prettyExprPrec p (EInterseccion e1 e2) =
    parensIf (p > 2) $ prettyExprPrec 3 e1 ++ " ∩ " ++ prettyExprPrec 3 e2

prettyExprPrec p (EJoin cond e1 e2) =
    parensIf (p > 3) $ 
        prettyExprPrec 4 e1 ++ " ⋈[" ++ prettyCond cond ++ "] " ++ prettyExprPrec 4 e2

prettyExprPrec p (ENaturalJoin e1 e2) =
    parensIf (p > 3) $ prettyExprPrec 4 e1 ++ " ⋈ " ++ prettyExprPrec 4 e2

prettyExprPrec p (EDiv e1 e2) =
    parensIf (p > 3) $ prettyExprPrec 4 e1 ++ " ÷ " ++ prettyExprPrec 4 e2

prettyExprPrec p (EGroup attrs aggs e) =
    parensIf (p > 5) $ 
        "γ[" ++ intercalate ", " attrs ++ "; " ++ 
        intercalate ", " (map prettyAgg aggs) ++ "](" ++ 
        prettyExprPrec 0 e ++ ")"

-- Pretty print de agregaciones
prettyAgg :: (GroupOp, Atributo) -> String
prettyAgg (op, attr) = prettyGroupOp op ++ "(" ++ attr ++ ")"

prettyGroupOp :: GroupOp -> String
prettyGroupOp Count = "COUNT"
prettyGroupOp Sum   = "SUM"
prettyGroupOp Avg   = "AVG"
prettyGroupOp Min   = "MIN"
prettyGroupOp Max   = "MAX"

-- Agregar paréntesis si es necesario
parensIf :: Bool -> String -> String
parensIf True s  = "(" ++ s ++ ")"
parensIf False s = s


-- ============================================================================
-- PRETTY PRINTER PARA CONDICIONES
-- ============================================================================

prettyCond :: Cond -> String
prettyCond c = prettyCondPrec 0 c

prettyCondPrec :: Int -> Cond -> String
prettyCondPrec _ PTrue  = "⊤"
prettyCondPrec _ PFalse = "⊥"

prettyCondPrec _ (PEq attr val) =
    attr ++ " = " ++ prettyValor val

prettyCondPrec _ (PNeq attr val) =
    attr ++ " ≠ " ++ prettyValor val

prettyCondPrec _ (PLt attr val) =
    attr ++ " < " ++ prettyValor val

prettyCondPrec _ (PGt attr val) =
    attr ++ " > " ++ prettyValor val

prettyCondPrec _ (PAttrEq attr1 attr2) =
    attr1 ++ " = " ++ attr2

prettyCondPrec p (PAnd c1 c2) =
    parensIf (p > 1) $ prettyCondPrec 2 c1 ++ " ∧ " ++ prettyCondPrec 2 c2

prettyCondPrec p (POr c1 c2) =
    parensIf (p > 0) $ prettyCondPrec 1 c1 ++ " ∨ " ++ prettyCondPrec 1 c2

prettyCondPrec p (PNot c) =
    "¬" ++ prettyCondPrec 3 c


-- ============================================================================
-- PRETTY PRINTER PARA CONTEXTO (BASE DE DATOS)
-- ============================================================================

-- Imprime todas las relaciones en el contexto
prettyContext :: Context -> String
prettyContext ctx =
    let rels = Map.toList ctx
    in unlines $ map (\(name, rel) -> prettyRelacion rel ++ "\n") rels


-- ============================================================================
-- PRETTY PRINTER ESTILO SQL (BONUS)
-- ============================================================================

-- Convierte una expresión a SQL-like syntax (aproximado)
prettySQLLike :: Expr -> String
prettySQLLike (ESeleccion cond e) =
    "SELECT * FROM (" ++ prettySQLLike e ++ ") WHERE " ++ prettyCondSQL cond

prettySQLLike (EProyeccion attrs e) =
    "SELECT " ++ intercalate ", " attrs ++ " FROM (" ++ prettySQLLike e ++ ")"

prettySQLLike (EUnion e1 e2) =
    "(" ++ prettySQLLike e1 ++ ") UNION (" ++ prettySQLLike e2 ++ ")"

prettySQLLike (EDiff e1 e2) =
    "(" ++ prettySQLLike e1 ++ ") EXCEPT (" ++ prettySQLLike e2 ++ ")"

prettySQLLike (EProd e1 e2) =
    "(" ++ prettySQLLike e1 ++ ") CROSS JOIN (" ++ prettySQLLike e2 ++ ")"

prettySQLLike (ENaturalJoin e1 e2) =
    "(" ++ prettySQLLike e1 ++ ") NATURAL JOIN (" ++ prettySQLLike e2 ++ ")"

prettySQLLike (EJoin cond e1 e2) =
    "(" ++ prettySQLLike e1 ++ ") JOIN (" ++ prettySQLLike e2 ++ 
    ") ON " ++ prettyCondSQL cond

prettySQLLike _ = "<complex expression>"

prettyCondSQL :: Cond -> String
prettyCondSQL PTrue = "TRUE"
prettyCondSQL PFalse = "FALSE"
prettyCondSQL (PEq a v) = a ++ " = " ++ prettyValor v
prettyCondSQL (PNeq a v) = a ++ " != " ++ prettyValor v
prettyCondSQL (PLt a v) = a ++ " < " ++ prettyValor v
prettyCondSQL (PGt a v) = a ++ " > " ++ prettyValor v
prettyCondSQL (PAttrEq a1 a2) = a1 ++ " = " ++ a2
prettyCondSQL (PAnd c1 c2) = prettyCondSQL c1 ++ " AND " ++ prettyCondSQL c2
prettyCondSQL (POr c1 c2) = prettyCondSQL c1 ++ " OR " ++ prettyCondSQL c2
prettyCondSQL (PNot c) = "NOT (" ++ prettyCondSQL c ++ ")"
