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
  atributos :: Set.Set Atributo -- Nombre de las columnas
  tuplas    :: Set.Set Tupla    -- Filas
  nombre    :: String
} deriving (Eq, Show)



-- Tipo de dato de una columna 
data Type = TInt
          | TString
          | TBool
          deriving (Eq, Show)

type Esquema = Map.Map Atributo Type


-- Expresiones Algebraicas:
data Expr = ERelacion

          -- Operaciones Elementales:
          | ESeleccion Cond Expr
          | EProyeccion [Atributo] Expr
          | EUnion Expr Expr
          | EDiff  Expr Expr 
          | EProd  Expr Expr
          | ERenombre Atributo Atributo Expr

          -- Operaciones Derivadas:
          | EInterseccion Expr Expr
          | EJoin Cond Expr Expr
          | ENaturalJoin Expr Expr 
          | EDiv          Expr Expr
          | EGroup [Atributo] [(GroupOp, Atributo)] Expr
          deriving (Eq, Show)


-- Predicados para Select: 
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


-- Mapa de nombres de relaciones a relaciones
type Context = Map.Map String Relation         

-- Mapa de macros/vistas definidas por el usuario
type MacroContext = Map.Map String Expr











-- ELIMINAR DESDE ACA 


type Env = [(String, Relation)]
eval :: Env -> RAExpr -> Relation


import qualified Data.Set as Set

data Relation = Relation
    { relName   :: String
    , relSchema :: Schema
    , relTuples :: Set.Set Tuple
    }
    deriving (Show)


{-El AR trabaja sobre relaciones (tablas), y al operar obtengo nuevas 
  relaciones. Es la base teorica de SQL
  
  Relacion -> Tabla
  Atributo -> Columna
  Tupla    -> Fila 


         A1(Legajo)      A2 (nombre)        ...        An (edad)
T1      1                     Ana                           18
        
T2      2                     Luis                          22

...

Tm

-}
import Map.Map
import Set.Set


src/
  AST.hs
  Eval.hs
  Database.hs
  Pretty.hs
  Main.hs
Implementamos un EDSL deep embedding del Álgebra Relacional en Haskell, donde la sintaxis está representada mediante tipos algebraicos y la semántica se define a través de un intérprete que evalúa consultas sobre una base de datos representada como relaciones finitas.

data RelAlg
  = Table String
  | Select Condition RelAlg
  | Project [Atributo] RelAlg
  | Union RelAlg RelAlg
  | Diff RelAlg RelAlg
  | Product RelAlg RelAlg
  | Rename Atributo Atributo RelAlg
  --
  | Join RelAlg RelAlg
  | Intersect RelAlg RelAlg
  | Division RelAlg RelAlg


type Atributo = String
type Tuple = Map Atributo Value
type Relation = [Tuple]
