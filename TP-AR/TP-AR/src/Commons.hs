module Commons (
    Relacion(..),
    Tupla,
    Valor(..),
    Type(..),
    Atributo,
    Err,
    Expr(..),
    Cond(..),
    NombreRel,
    NombreOp,
    EnvRel,
    EnvOp,
    Context(..),
    State(..),
    emptyContext,
    Error(..),
    Command(..),
    TopLevel(..),
    subst
) where

import qualified Data.Map as Map
import qualified Data.Set as Set


-- Relacion: es un conjunto de Tuplas
data Relacion = R {
  nombre    :: String,
  atributos ::  [(Atributo, Type)], 
  tuplas    :: Set.Set Tupla        
} deriving (Eq, Show)

-- Tupla: Mapea un atributo con su valor
type Tupla = Map.Map Atributo Valor

-- Valores de una celda
data Valor = VInt Int
           | VString String
           | VBool Bool
           | VNull
           deriving (Eq, Ord, Show)

-- Tipo de dato de una columna 
data Type = PInt
          | PString
          | PBool
          deriving (Eq, Show)

type RelName  = String
type Atributo = String
type Err      = String



-- Expresiones Algebraicas: 
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
          | ENaturalJoin  Expr Expr 
          | EDiv          Expr Expr
          | ECall         NombreOp [Expr]
          deriving (Eq, Show)


-- Predicados para Select: 
data Cond = PTrue
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




-- Para manejar el contexto:

type NombreRel = String
type NombreOp  = String

-- Mapa de nombres de relaciones a relaciones
type EnvRel = Map.Map NombreRel Relacion         

-- Mapa de operaciones definidas por el usuario: guarda (Lista de Parámetros, Cuerpo de la Expresión)
type EnvOp  = Map.Map NombreOp ([Atributo], Expr)


-- El contexto nos sirve para llevar todas las relaciones y operaciones definidas por el usuario
data Context = Context
  { relaciones    :: EnvRel
  , operaciones   :: EnvOp
  }

-- El estado nos sirve para llevar el contexto y el modo de interaccion
data State = S
  { inter :: Bool
  , lfile :: String
  , ctxt  :: Context
  }


emptyContext :: Context
emptyContext =  Context
  { relaciones  = Map.empty
  , operaciones = Map.empty
  }




-- Para manejar Errores:

data Error = RelacionNoExiste NombreRel
           | RelacionYaExiste NombreRel
           | OperacionNoExiste NombreOp
           | OperacionYaExiste NombreOp
           | EsquemaIncompatible
           | AtributoNoExiste [Atributo]
           | AtributoYaExiste Atributo
           | MismoAtributo
           | Atributos
           | ErrorEvaluacion String

           | AtributosNoCompatibles
           | TiposIncompatibles
           | ErrorArgumentos NombreOp Int Int -- Nombre, Esperados, Recibidos
  deriving Show



-- Para manejar comandos:

data Command = Quit
             | Help
             | Clear
             | Browse
             | Reload
             | Compile String                       
             | CreateRel NombreRel [(Atributo, Type)]
             | InsertRel NombreRel [[Valor]]
             | DropRel   NombreRel
             | DefineOP  NombreOp [Atributo] Expr
             deriving Show


-- Diferenciamos la 
data TopLevel
  = TExpr    Expr
  | TAssign  NombreRel Expr
  | TCmd     Command
  deriving Show

-- Realiza la substitución de parámetros por argumentos en una expresión.
subst :: Map.Map RelName Expr -> Expr -> Expr
subst m (ERelacion name)    = case Map.lookup name m of 
                                Nothing -> ERelacion name
                                Just expr -> expr 
subst m (ESeleccion c e)    = ESeleccion c (subst m e)
subst m (EProyeccion as e)  = EProyeccion as (subst m e)
subst m (EUnion e1 e2)      = EUnion (subst m e1) (subst m e2)
subst m (EDiff e1 e2)       = EDiff (subst m e1) (subst m e2)
subst m (EProd e1 e2)       = EProd (subst m e1) (subst m e2)
subst m (EInterseccion e1 e2) = EInterseccion (subst m e1) (subst m e2)
subst m (ENaturalJoin e1 e2) = ENaturalJoin (subst m e1) (subst m e2)
subst m (EDiv e1 e2)        = EDiv (subst m e1) (subst m e2)
subst m (ERenombre o n e)   = ERenombre o n (subst m e)
subst m (ECall name args)   = ECall name (map (subst m) args)
