module Commons where

import qualified Data.Map as Map
import qualified Data.Set as Set







-- Relacion: es un conjunto de Tuplas
data Relacion = R {
  nombre    :: String,
  atributos ::  [(Atributo, Type)], -- Nombre de las columnas, Listas para dejarlo fijo y mantener orden
  tuplas    :: Set.Set Tupla        -- Filas
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


type Atributo = String
type Err      = String




---------------------------------------------------------------
-- Expresiones Algebraicas: 
---------------------------------------------------------------

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



---------------------------------------------------------------
-- Para manejar el contexto:
---------------------------------------------------------------

type NombreRel = String
type NombreOp  = String

-- Mapa de nombres de relaciones a relaciones
type EnvRel = Map.Map NombreRel Relacion         

-- Mapa de operaciones definidas por el usuario
type EnvOp  = Map.Map NombreOp Expr


data Context = Context
  { relaciones    :: EnvRel  -- Base de datos (relaciones cargadas)
  , operaciones   :: EnvOp   -- Vistas definidas por el usuario
  }

data State = S
  { inter :: Bool
  ,       -- True, si estamos en modo interactivo.
    lfile :: String
  ,     -- Ultimo archivo cargado (para hacer "reload")
    ctxt    :: Context  -- Entorno con variables globales y su valor  [(Name, (Value, Type))]
  }


emptyContext :: Context
emptyContext =  Context
  { relaciones  = Map.empty
  , operaciones = Map.empty
  }



---------------------------------------------------------------
-- Para manejar Errores:
---------------------------------------------------------------

data Error = RelacionNoExiste NombreRel
           | RelacionYaExiste NombreRel
           | OperacionNoExiste NombreOp
           | OperacionYaExiste NombreOp
           | EsquemaIncompatible
           | AtributoNoExiste [Atributo] -- Cuidado, lo hice lista
           | AtributoYaExiste Atributo --
           | MismoAtributo -- Para proyeccion 
           | Atributos
           | ErrorEvaluacion String -- Nuevo

           | AtributosNoCompatibles

           | TiposIncompatibles
  deriving Show


---------------------------------------------------------------
-- Para manejar comandos:
---------------------------------------------------------------
data InteractiveCommand = Cmd {
        alias   :: [String]
      , args    :: String
      , handler :: String -> Command 
      , help    :: String}


               -- Para comandos consola: 
data Command = Compile CompileForm
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
              | CreateRel NombreRel  [(Atributo, Type)]
              | InsertRel NombreRel [[String]]
              | DropRel   NombreRel 
              | AssignRel NombreRel String


---------------------------------------------------------------
-- Para manejar archivos:
---------------------------------------------------------------

data CompileForm = CompileInteractive  String
                  | CompileFile         String


