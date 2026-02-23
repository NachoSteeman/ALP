module AST where

import qualified Data.Map as Map
import qualified Data.Set as Set

type Atributo = String
type Err      = String

-- Valores de una celda
data Valor = VInt Int
           | VString String
           | VBool Bool
           | VNull
           deriving (Eq, Ord, Show)

-- Tupla: Mapea un atributo con su valor
type Tupla = Map.Map Atributo Valor

-- Relacion: es un conjunto de Tuplas
data Relacion = R {
  atributos :: Set.Set Atributo, -- Nombre de las columnas
  tuplas    :: Set.Set Tupla,    -- Filas
  nombre    :: String
} deriving (Eq, Show)



-- Tipo de dato de una columna 
data Type = PInt
          | PString
          | PBool
          deriving (Eq, Show)

type Esquema = Map.Map Atributo Type


-- Expresiones Algebraicas: E
data Expr = ERelacion String
          -- Operaciones Elementales:
          | ESeleccion Cond Expr 
          | EProyeccion [Atributo] Expr
          | EUnion Expr Expr
          | EDiff  Expr Expr 
          | EProd  Expr Expr
          | ERenombre Atributo Atributo Expr

          -- Operaciones Derivadas:
          | EInterseccion Expr Expr
          | EJoin    Cond Expr Expr
          | ENaturalJoin  Expr Expr 
          | EDiv          Expr Expr
          | EGroup [Atributo] [(GroupOp, Atributo)] Expr
          deriving (Eq, Show)


-- Predicados para Select: P
data Cond
    = PTrue
    | PFalse
    | PEq  Atributo  Valor             -- attr = valor
    | PNeq Atributo  Valor             -- attr ≠ valor
    | PLt  Atributo  Valor             -- attr < valor
    | PGt  Atributo  Valor             -- attr > valor
    | PAttrEq Atributo Atributo        -- attr1 = attr2
    | PAnd Cond Cond                   -- p1 AND p2
    | POr  Cond Cond                   -- p1 OR p2
    | PNot Cond                        -- NOT p
    deriving (Eq, Show)

-- Predicado para Group:
data GroupOp = Count
             | Sum 
             | Avg
             | Min
             | Max
             deriving (Eq, Show)



-- Comandos del AR:

-- Comandos del interprete:


-- Mapa de nombres de relaciones a relaciones
type Context = Map.Map String Relacion         

-- Mapa de macros/vistas definidas por el usuario
type MacroContext = Map.Map String Expr

