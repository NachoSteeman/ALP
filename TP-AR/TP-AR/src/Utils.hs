module Utils
( seleccion,
  union,
  diferencia,
  productoCartesiano,
  renombre,
  proyeccion,
  naturalJoin,
  division,
  interseccion,
  groupBy



) where

-- Operaciones Fundamentales:

import qualified Data.Map as Map
import qualified Data.Set as Set

import AST 

import Data.List (intercalate, nub)

-- =========================================================
-- SELECCION
-- =========================================================

seleccion :: Relacion -> Cond -> Relacion
seleccion (R a t n) c =  R a (Set.filter (filtrarCond c) t) n

filtrarCond :: Cond -> Tupla -> Bool
filtrarCond c t = case c of 

    PTrue  -> True
    PFalse -> False
    
    PEq a v ->
        case Map.lookup a t of 
            Just val -> compararIgual val v
            Nothing  -> False 

    PNeq a v ->
        case Map.lookup a t of
            Just val -> not (compararIgual val v)
            Nothing  -> False

    PLt a v ->
        case Map.lookup a t of
            Just val -> compararValor (<) val v
            Nothing  -> False

    PGt a v ->
        case Map.lookup a t of
            Just val -> compararValor (>) val v
            Nothing  -> False
    
    PAttrEq a0 a1 ->
        case (Map.lookup a0 t, Map.lookup a1 t) of
            (Just v0, Just v1) -> compararIgual v0 v1
            _ -> False

    PAnd c0 c1 ->
        filtrarCond c0 t && filtrarCond c1 t

    POr c0 c1 ->
        filtrarCond c0 t || filtrarCond c1 t

    PNot c ->
        not (filtrarCond c t)

compararValor :: (Int -> Int -> Bool) -> Valor -> Valor -> Bool
compararValor f (VInt x) (VInt y) = f x y
compararValor _ _ _ = False

compararIgual :: Valor -> Valor -> Bool
compararIgual (VInt x) (VInt y) = x == y
compararIgual (VString x) (VString y) = x == y
compararIgual (VBool x) (VBool y) = x == y
compararIgual VNull VNull = True
compararIgual _ _ = False


-- =========================================================
-- UNION
-- =========================================================

union :: Relacion -> Relacion -> Either String Relacion
union (R a0 t0 n0) (R a1 t1 n1)
    | a0 == a1  = Right (R a0 (Set.union t0 t1) (n0 ++ "U" ++ n1))
    | otherwise = Left "Atributos no compatibles"

-- =========================================================
-- DIFERENCIA
-- =========================================================

diferencia :: Relacion -> Relacion -> Either String Relacion 
diferencia (R a0 t0 n0) (R a1 t1 n1)
    | a0 == a1  = Right (R a0 (Set.difference t0 t1) (n0 ++ "-" ++ n1))
    | otherwise = Left "Error en la diferencia: atributos no compatibles"

-- =========================================================
-- PRODUCTO CARTESIANO
-- =========================================================

productoCartesiano :: Relacion -> Relacion -> Relacion
productoCartesiano (R a0 t0 n0) (R a1 t1 n1) = 
    let comunes = filter (`elem` a1) a0

        a0' = renombrarSoloComunes a0 n0 comunes
        a1' = renombrarSoloComunes a1 n1 comunes

        t0' = renombrarTuplasComunes t0 n0 comunes
        t1' = renombrarTuplasComunes t1 n1 comunes

        a   = nub (a0' ++ a1')

        t   = prodCartAux t0' t1'

        n   = n0 ++ "*" ++ n1

    in R a t n

renombrarSoloComunes :: [Atributo] -> String -> [Atributo] -> [Atributo]
renombrarSoloComunes attrs nombre comunes =
    map (\a -> if a `elem` comunes then a ++ "-" ++ nombre else a) attrs

renombrarTuplasComunes :: Set.Set Tupla -> String -> [Atributo] -> Set.Set Tupla
renombrarTuplasComunes ts nombre comunes =
    Set.map (\tupla ->
        Map.mapKeys (\a ->
            if a `elem` comunes
                then a ++ "-" ++ nombre
                else a
        ) tupla
    ) ts

prodCartAux :: Set.Set Tupla -> Set.Set Tupla -> Set.Set Tupla
prodCartAux setA setB =
    Set.fromList
        [ Map.union a b
        | a <- Set.toList setA
        , b <- Set.toList setB
        ]

-- =========================================================
-- RENOMBRE
-- =========================================================
renombre :: Atributo -> Atributo -> Relacion -> Either Err Relacion
renombre oldAttr newAttr r@(R attrs tups name)
    -- Validar que el atributo viejo existe:
    | notElem oldAttr attrs = Left $ "Atributo '" ++ oldAttr ++ "' no existe en relación"
    
    -- Validar que el atributo nuevo no existe ya:
    | elem newAttr attrs =  Left $ "Atributo '" ++ newAttr ++ "' ya existe en relación"

    -- Validar que el nombre es diferente:
    | oldAttr == newAttr = Left "El atributo antiguo y nuevo no pueden ser iguales"

    -- Si no hay problemas:
    | otherwise =  let a = map (\a -> if a == oldAttr then newAttr else a) attrs
                       t = Set.map (renameTupla oldAttr newAttr) tups 
                    in return (R a t name)

renameTupla :: Atributo -> Atributo -> Tupla -> Tupla
renameTupla oldAttr newAttr tup = case Map.lookup oldAttr tup of
        Nothing -> tup  -- Nunca ocurrira por como validamos la entrada
        Just val -> Map.delete oldAttr $ Map.insert newAttr val tup


renombre2 :: String -> Relacion -> Relacion
renombre2 nuevoNombre (R a t n) =
    let a' = map (renombreAux2 nuevoNombre) a

        t' = Set.map
                (\tup -> Map.mapKeys (renombreAux2 nuevoNombre) tup)
                t

    in R a' t' nuevoNombre

renombreAux2 :: String -> Atributo -> Atributo  
renombreAux2 nuevoNombre atrib =
    nuevoNombre ++ "." ++ atrib

-- =========================================================
-- PROYECCION
-- =========================================================

proyeccion :: [Atributo] -> Relacion -> Either String Relacion 
proyeccion atributosProy (R a t n)

    | not (all (`elem` a) atributosProy) =
        Left ("Atributos no existentes: " ++ show (filter (`notElem` a) atributosProy))

    | otherwise =
        let a' = atributosProy

            t' = Set.map (proyectarTupla atributosProy) t

            n' = "π[" ++ intercalate "," atributosProy ++ "](" ++ n ++ ")"

        in Right (R a' t' n')

proyectarTupla :: [Atributo] -> Tupla -> Tupla
proyectarTupla attrs tupla =
    Map.filterWithKey (\k _ -> k `elem` attrs) tupla

-- =========================================================
-- INTERSECCION 
-- =========================================================

interseccion :: Relacion -> Relacion -> Either String Relacion
interseccion r s = do
    diff <- diferencia r s
    diferencia r diff

-- =========================================================
-- JOIN NATURAL
-- =========================================================

naturalJoin :: Relacion -> Relacion -> Relacion
naturalJoin (R a0 t0 n0) (R a1 t1 n1) =

    let comunes = filter (`elem` a1) a0

        esquema = nub (a0 ++ a1)

        compatibles tupla0 tupla1 =
            all (\attr ->
                Map.lookup attr tupla0 == Map.lookup attr tupla1
            ) comunes

        tuplas =
            Set.fromList
                [ Map.union tupla0 tupla1
                | tupla0 <- Set.toList t0
                , tupla1 <- Set.toList t1
                , compatibles tupla0 tupla1
                ]

    in R esquema tuplas (n0 ++ " ⋈ " ++ n1)

-- =========================================================
-- DIVISION
-- =========================================================

division :: Relacion -> Relacion -> Either String Relacion
division r@(R a0 t0 n0) s@(R a1 t1 n1)

    | not (all (`elem` a0) a1) =
        Left "Error en la Division: el divisor debe ser subconjunto del dividendo"

    | otherwise =
        let atrib = filter (`notElem` a1) a0
        in do
            proy1 <- proyeccion atrib r
            let prod = productoCartesiano proy1 s
            diff  <- diferencia prod r
            proy2 <- proyeccion atrib diff
            diferencia proy1 proy2


-- =========================================================
-- GROUP BY
-- =========================================================

groupBy :: [Atributo] -> [(GroupOp, Atributo)] -> Relacion -> Either String Relacion
groupBy = undefined
