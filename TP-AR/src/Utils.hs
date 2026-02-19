-- Operaciones Fundamentales:

-- OpUnarias:
seleccion :: Relacion -> Cond -> Relacion
seleccion (R a t n) c =  (R a (Set.filter (filtrarCond c) t) n)

filtrarCond :: Cond -> Tupla -> Bool
filtrarCond c t = case c of 
    
    PTrue  -> True

    PFalse -> False
    
    PEq a v -> case Map.lookup a t of 
                      Just val -> v == val
                      Nothing  -> False 

    PNeq    a v   -> case Map.lookup a t of
                        Nothing  -> False
                        Just val -> v /= val

    PLt     a v   -> case Map.lookup a t of
                        Nothing  -> False
                        Just val -> val < v  
                        
    PGt     a v   -> case Map.lookup a t of
                        Nothing  -> False
                        Just val -> val > v
    
    PAttrEq a0 a1 -> case Map.lookup a0 t of
                        Nothing  -> False
                        Just val0 -> case Map.lookup a1 t of
                                     Nothing  -> False
                                     Just val1 -> val1 == val0

    PAnd    c0 c1 -> let cond0 = filtrarCond c0 t
                         cond1 = filtrarCond c1 t
                     in cond1 && cond0  

    POr     c0 c1 -> let cond0 = filtrarCond c0 t
                         cond1 = filtrarCond c1 t
                     in cond1 || cond0

    PNot       c  -> not (filtrarCond c t)


union :: Relacion -> Relacion -> Either String Relacion
union (R a0 t0 n0) (R a1 t1 n1) = if a0 == a1 then return (R a0 (Set.union t0 t1) (n0 ++ "U" ++ n1)) 
                                        else Left "Atributos no compatibles"

diferencia :: Relacion -> Relacion -> Either Err Relacion 
diferencia (R a0 t0 n0)(R a1 t1 n1) = if a0 == a1 then return (R a0  (Set.difference t0 t1) (n0 ++ "-" ++ n1))
                                            else Left "Error en la diferencia: atributos no compatibles"




productoCartesiano :: Relacion -> Relacion -> Relacion
productoCartesiano (R a0 t0 n0) (R a1 t1 n1) = 
    let comunes = Set.intersection a0 a1  
        a0' = renombrarSoloComunes a0 n0 comunes
        a1' = renombrarSoloComunes a1 n1 comunes
        t0' = renombrarTuplasComunes t0 n0 comunes
        t1' = renombrarTuplasComunes t1 n1 comunes
        a   = Set.union a0' a1'
        t   = prodCartAux t0' t1' 
        n   = n0 ++ "*" ++ n1
    in R a t n

renombrarSoloComunes :: Set.Set Atributo -> String -> Set.Set Atributo -> Set.Set Atributo
renombrarSoloComunes as n comunes = 
    Set.map (\s -> if s `Set.member` comunes then s ++ "-" ++ n else s) as

renombrarTuplasComunes :: Set.Set Tupla -> String -> Set.Set Atributo -> Set.Set Tupla
renombrarTuplasComunes ts n comunes = 
    Set.map (\tupla -> Map.mapKeys (\s -> if s `Set.member` comunes 
                                          then s ++ "-" ++ n 
                                          else s) tupla) ts
prodCartAux :: Set.Set Tupla -> Set.Set Tupla -> Set.Set Tupla
prodCartAux setA setB = 
    Set.fromList [ Map.union a  b | a <- Set.toList setA, b <- Set.toList setB ]



-- renombramiento: asignamos el nuevo nombre a la relacion, y renombramos sus atributos poniendo como prefijo el nombre de la misma
renombramiento :: String -> Relacion -> Relacion
renombramiento nuevoNombre (R a t n) =  let a' = Set.map (renombramientoAux nuevoNombre) a
                                            t' = Set.map (\tup   -> Map.mapKeys (renombramientoAux nuevoNombre) tup) t
                                            n' = nuevoNombre
                                        in (R a' t' n')

renombramientoAux :: String -> Atributo -> Atributo  
renombramientoAux nuevoNombre atrib  = nuevoNombre ++ "." ++ atrib



-- Ver                   
proyeccion :: [Atributo] -> Relacion -> Either Err Relacion 
proyeccion atributosProy (R a t n) = let a' = Set.difference a (Set.toSet atributosProy)
                                         t' = Set.filter  (filtraTupla atributosProy) t 
                                         n' = "Proy-"++ n
                                     in (R a' t' n')

-- ============================================================================
-- ============================================================================
proyeccion :: [Atributo] -> Relacion -> Either String Relacion 
proyeccion atributosProy (R a t n) 
    -- Validar que los atributos existen
    | not (all (`Set.member` a) atributosProy) =
        Left $ "Atributos no existentes: " ++ 
               show (filter (`Set.notMember` a) atributosProy)
    | otherwise =
        let a' = Set.fromList atributosProy
            t' = Set.map (proyectarTupla atributosProy) t
            n' = "π[" ++ intercalate "," atributosProy ++ "](" ++ n ++ ")"
        in Right (R a' t' n')

-- Proyectar una tupla (mantener solo ciertas claves)
proyectarTupla :: [Atributo] -> Tupla -> Tupla
proyectarTupla attrs tupla = 
    Map.filterWithKey (\k _ -> k `elem` attrs) tupla

-- ============================================================================
-- ============================================================================





-- Operaciones Extras:
interseccion :: Relacion -> Relacion -> Either Err Relacion
interseccion r s = do diff <- diferencia r s
                      diferencia r diff


-- Precondicion: r y s no tienen atributos en comun => ProdNat = ProdCart
naturalJoin :: Relacion -> Relacion -> Either Err Relacion -- Ver tipos y si quiero que tire el error de la poyeccion o no... VER  
naturalJoin r@(R a0 t0 n0) s@(R a1 t1 n1) = let atribUnion   = Set.toList (Set.union a0 a1)
                                                atribComunes = Set.intersection a0 a1
                                                cond         = armarCondicion atribComunes n0 n1
                                                prod         = productoCartesiano r s 
                                                temp         = seleccion prod cond
                                             in proyeccion atribUnion temp  

-- Condicion: R.attr = S.attr para todos los atributos comunes
PAnd ... (PAnd  ((PAttrEq Atributo Atributo) PTrue))

agrupamiento = undefined

armarCondicion :: Set.Set Atributo -> String -> String -> Condicion
armarCondicion comunes nombreR nombreS =
    let pares = [(attr ++ "-" ++ nombreR, attr ++ "-" ++ nombreS) 
                | attr <- Set.toList comunes]
    in case pares of
        []     -> PTrue
        (a,b):rest -> foldr (\(x,y) acc -> PAnd (PAttrEq x y) acc) 
                            (PAttrEq a b) 
                            rest

-- ============================================================================
-- JOIN NATURAL (versión eficiente)
-- ============================================================================
joinNatural :: Relacion -> Relacion -> Relacion
joinNatural (R a0 t0 n0) (R a1 t1 n1) =
    let comunes = Set.intersection a0 a1
        esquema = Set.union a0 a1
        
        -- Tuplas compatibles en atributos comunes
        compatibles t1 t2 = all (\attr -> 
            Map.lookup attr t1 == Map.lookup attr t2) (Set.toList comunes)
        
        tuplas = Set.fromList
            [ Map.union t1 t2
            | t1 <- Set.toList t0
            , t2 <- Set.toList t1
            , compatibles t1 t2
            ]
        
    in R esquema tuplas (n0 ++ " ⋈ " ++ n1)
-- ============================================================================
--
-- ============================================================================

division :: Relacion -> Relacionn -> Either Err Relacion
division r@(R a0 t0 n0) s@(R a1 t1 n1) 
    | not (Set.isSubsetOf a1 a0) = Left "Error en la Division: el divisior debe ser subconjunto del dividendo"
    | otherwise = do atrib  <- Set.toList (Set.difference a0 a1) 
                     proy1  <- proyeccion  atrib r
                     prod   <- productoCartesiano proy1 s
                     diff   <- diferencia prod r
                     proy2  <- proyeccion atrib diff 
                     diferencia proy1 proy2  




-- Objetivo: hacer un interprete de AR
