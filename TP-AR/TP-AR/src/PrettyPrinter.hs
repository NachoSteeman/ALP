module PrettyPrinter
    ( prettyRelacion
    , prettyExpr
    , prettyCond
    , prettyValor
    , prettyTupla
    , prettyContext
    ) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List (intercalate, sort)
import Text.Printf (printf)

import AST  


-- Imprime una relación como tabla 
prettyRelacion :: Relacion -> String
prettyRelacion (R atrib tuplas nom)
    | Set.null tuplas = 
        printf "Relación '%s' (vacía)\nEsquema: [%s]\n" 
               nom 
               (intercalate ", " (Set.toList atrib))
    | otherwise =
        let atribList = sort (Set.toList atrib)
            header = makeHeader nom atribList
            separator = makeSeparator atribList
            rows = map (makeRow atribList) (Set.toList tuplas)
        in unlines ([separator, header, separator] ++ rows ++ [separator])



-- Para imprimir Tuplas:
prettyTupla :: Tupla -> String
prettyTupla tupla =
    let pairs = Map.toList tupla
        strs = map (\(k, v) -> k ++ ": " ++ prettyValor v) pairs
    in "{ " ++ intercalate ", " strs ++ " }"



-- Para imprimir valores:
prettyValor :: Valor -> String
prettyValor (VInt n)    = show n
prettyValor (VString s) = "\"" ++ s ++ "\""
prettyValor (VBool b)   = if b then "true" else "false"
prettyValor VNull       = "NULL"



-- Imprime todas las relaciones en el contexto
prettyContext :: Context -> String
prettyContext st =
    let rels = Map.toList (relaciones st)
        ops  = Map.toList (operaciones st)

        relStr = if null rels
                 then "No hay relaciones cargadas.\n"
                 else "Relaciones:\n" ++
                      unlines (map (\(_, r) -> prettyRelacion r) rels)

        opStr  = if null ops
                 then "No hay operaciones definidas.\n"
                 else "Operaciones:\n" ++
                      unlines (map (\(n, _) -> n) ops)

    in relStr ++ "\n" ++ opStr





-- Para imprimir Condiciones:
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




-- Crear encabezado con nombre de la relación
makeHeader :: String -> [Atributo] -> String
makeHeader nom atrib =
    let cols = map (\a -> pad 15 a) atrib
    in "| " ++ intercalate " | " cols ++ " |  (" ++ nom ++ ")"

-- Crear separador
makeSeparator :: [Atributo] -> String
makeSeparator atrib =
    "+" ++ intercalate "+" (replicate (length atrib) (replicate 17 '-')) ++ "+"

-- Crear fila de datos
makeRow :: [Atributo] -> Tupla -> String
makeRow atrib tupla =
    let values = map (\attr -> prettyValorPadded 15 (Map.lookup attr tupla)) atrib
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











-- Imprime Expresiones:

prettyExpr :: Expr -> String
prettyExpr expr = prettyExprPrec 0 expr

-- Pretty print con precedencia (para paréntesis)
prettyExprPrec :: Int -> Expr -> String
prettyExprPrec _ (ERelacion nombre) = nombre 

prettyExprPrec p (ESeleccion cond e) =
    parensIf (p > 5) $ "σ[" ++ prettyCond cond ++ "](" ++ prettyExprPrec 0 e ++ ")"

prettyExprPrec p (EProyeccion atrib e) =
    parensIf (p > 5) $ "π[" ++ intercalate ", " atrib ++ "](" ++ prettyExprPrec 0 e ++ ")"

prettyExprPrec p (ERenombre old new e) =
    parensIf (p > 5) $ "ρ[" ++ old ++ " → " ++ new ++ "](" ++ prettyExprPrec 0 e ++ ")"

prettyExprPrec p (EUnion e1 e2) =
    parensIf (p > 2) $ prettyExprPrec 3 e1 ++ " U " ++ prettyExprPrec 3 e2

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

prettyExprPrec p (EGroup atrib aggs e) =
    parensIf (p > 5) $ 
        "γ[" ++ intercalate ", " atrib ++ "; " ++ 
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


