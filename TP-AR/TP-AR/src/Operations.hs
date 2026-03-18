module Operations
( seleccion,
  union,
  diferencia,
  productoCartesiano,
  renombre,
  proyeccion,
  naturalJoin,
  division,
  interseccion,
) where

-- Operaciones Fundamentales:

import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.List (intercalate, nub)


import Commons 

-- Para obtener el nombre de los atributos ignorando tipo
attrNames :: [(Atributo, Type)] -> [Atributo]
attrNames = map fst

-- =========================================================
-- SELECCION
-- =========================================================

seleccion :: Relacion -> Cond -> Either Error Relacion
seleccion (R n a t) c = do
    let tuplas = Set.toList t

    evaluadas <- mapM evaluar tuplas

    let filtradas = [tupla | (tupla, True) <- evaluadas]

    return (R n a (Set.fromList filtradas))

  where
    evaluar tupla = do
        b <- filtrarCond c tupla
        return (tupla, b)

        
filtrarCond :: Cond -> Tupla -> Either Error Bool
filtrarCond c t = case c of 

    PTrue  -> Right True
    PFalse -> Right False
    
    PEq a v ->
        case Map.lookup a t of 
            Just val -> compararIgual val v
            Nothing  -> Right False 

    PNeq a v ->
        case Map.lookup a t of
            Just val -> case (compararIgual val v) of 
                Right b  -> Right (not b)
                Left err -> Left err
            Nothing  -> Right False

    PLt a v ->
        case Map.lookup a t of
            Just val -> compararValor (<) val v
            Nothing  -> Right False

    PGt a v ->
        case Map.lookup a t of
            Just val -> compararValor (>) val v
            Nothing  -> Right False
    
    PAttrEq a0 a1 ->
        case (Map.lookup a0 t, Map.lookup a1 t) of
            (Just v0, Just v1) -> compararIgual v0 v1
            _ -> Right False

    PAnd c0 c1 -> do
        b0 <- filtrarCond c0 t
        b1 <- filtrarCond c1 t
        return (b0 && b1)

    POr c0 c1 -> do
        b0 <- filtrarCond c0 t
        b1 <- filtrarCond c1 t
        return (b0 || b1)

    PNot c -> do
        b <- filtrarCond c t
        return (not b)

compararValor :: (Int -> Int -> Bool) -> Valor -> Valor -> Either Error Bool
compararValor f (VInt x) (VInt y) = Right (f x y)
compararValor _ _ _ = Left TiposIncompatibles


compararIgual :: Valor -> Valor -> Either Error Bool
compararIgual (VInt x) (VInt y) = Right (x == y)
compararIgual (VString x) (VString y) = Right (x == y)
compararIgual (VBool x) (VBool y) = Right (x == y)
compararIgual VNull VNull = Right True
compararIgual _ _ =  Left TiposIncompatibles


-- =========================================================
-- UNION
-- =========================================================

union :: Relacion -> Relacion -> Either Error Relacion
union (R n0 a0 t0) (R n1 a1 t1)
    | a0 == a1  = Right (R (n0 ++ "U" ++ n1) a0 (Set.union t0 t1) )
    | otherwise = Left AtributosNoCompatibles

-- =========================================================
-- DIFERENCIA
-- =========================================================

diferencia :: Relacion -> Relacion -> Either Error Relacion 
diferencia (R n0 a0 t0) (R n1 a1 t1)
    | a0 == a1  = Right (R (n0 ++ "-" ++ n1) a0 (Set.difference t0 t1) )
    | otherwise = Left AtributosNoCompatibles

-- =========================================================
-- PRODUCTO CARTESIANO
-- =========================================================

productoCartesiano :: Relacion -> Relacion -> Relacion
productoCartesiano (R n0 a0 t0) (R n1 a1 t1) = 
    let comunes = filter (`elem` attrNames a1) (attrNames a0)

        a0' = renombrarSoloComunes a0 n0 comunes
        a1' = renombrarSoloComunes a1 n1 comunes

        t0' = renombrarTuplasComunes t0 n0 comunes
        t1' = renombrarTuplasComunes t1 n1 comunes

        a   =  (a0' ++ a1')

        t   = prodCartAux t0' t1'

        n   = n0 ++ "*" ++ n1

    in R n a t

renombrarSoloComunes :: [(Atributo,Type)] -> String -> [Atributo] -> [(Atributo,Type)]
renombrarSoloComunes attrs nombre comunes =
    map (\(a, t) -> if a `elem` comunes then (a ++ "-" ++ nombre, t) 
                                        else (a, t)) attrs

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
renombre :: Atributo -> Atributo -> Relacion -> Either Error Relacion
renombre oldAttr newAttr r@(R name attrs tups)
    -- Validar que el atributo viejo existe:
    | notElem oldAttr (attrNames attrs) = Left (AtributoNoExiste [oldAttr]) 
    
    -- Validar que el atributo nuevo no existe ya:
    | elem newAttr (attrNames attrs) =  Left (AtributoYaExiste newAttr )

    -- Validar que el nombre es diferente:
    | oldAttr == newAttr = Left MismoAtributo

    -- Si no hay problemas:
    | otherwise =  let a = map (\(a,t) -> if a == oldAttr then (newAttr, t) 
                                                          else (a, t)) attrs
                       
                       t = Set.map (renameTupla oldAttr newAttr) tups 
                    in return (R name a t )

renameTupla :: Atributo -> Atributo -> Tupla -> Tupla
renameTupla oldAttr newAttr tup = case Map.lookup oldAttr tup of
        Nothing -> tup  -- Nunca ocurrira por como validamos la entrada
        Just val -> Map.delete oldAttr $ Map.insert newAttr val tup




-- =========================================================
-- PROYECCION
-- =========================================================
-- Ver de hacer mas eficiente: 
proyeccion :: [Atributo] -> Relacion -> Either Error Relacion
proyeccion attrsProy (R n esquema tuplas)
    | not (null faltantes) =
        Left (AtributoNoExiste  faltantes)
    | otherwise =
        let esquema' = construirEsquema attrsProy esquema
            claves   = Set.fromList attrsProy

            tuplas'  =
                Set.map
                    (\t -> Map.restrictKeys t claves)
                    tuplas

            nombre' =
                "π[" ++ intercalate "," attrsProy ++ "](" ++ n ++ ")"

        in Right (R nombre' esquema' tuplas')

  where
    nombres = attrNames esquema
    faltantes = filter (`notElem` nombres) attrsProy

construirEsquema :: [Atributo] -> [(Atributo,Type)] -> [(Atributo,Type)]
construirEsquema attrs esquema =
    map buscar attrs
  where
    buscar a =
        case lookup a esquema of
            Just t  -> (a,t)
            Nothing -> error "imposible: validado antes"
-- =========================================================
-- INTERSECCION 
-- =========================================================

interseccion :: Relacion -> Relacion -> Either Error Relacion
interseccion r s = do
    diff <- diferencia r s
    diferencia r diff

-- =========================================================
-- JOIN NATURAL
-- =========================================================

naturalJoin :: Relacion -> Relacion -> Relacion
naturalJoin (R n0 a0 t0) (R n1 a1 t1) =

    let comunes = filter (`elem` attrNames a1) (attrNames a0)

        atribs = nub (a0 ++ a1)

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

    in R (n0 ++ " ⋈ " ++ n1) atribs tuplas 

-- =========================================================
-- DIVISION
-- =========================================================

division :: Relacion -> Relacion -> Either Error Relacion
division r@(R n0 a0 t0) s@(R n1 a1 t1)

    | not (all (`elem` attrNames a0) (attrNames a1)) =
        Left EsquemaIncompatible -- "Error en la Division: el divisor debe ser subconjunto del dividendo"

    | otherwise =
        let atrib = filter (`notElem` attrNames a1) (attrNames a0)
        in do
            proy1 <- proyeccion atrib r
            let prod = productoCartesiano proy1 s
            diff  <- diferencia prod r
            proy2 <- proyeccion atrib diff
            diferencia proy1 proy2


