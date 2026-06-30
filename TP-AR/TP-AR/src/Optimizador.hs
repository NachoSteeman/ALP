module Optimizador (optimizador) where

import Commons
import Monads
import qualified Data.Map as Map
import Eval (subst)

-- optimizador: optimiza la expresion
optimizador :: Expr -> StateError Expr
optimizador expr = case expr of
 
    ERelacion r     -> return (ERelacion r)

    ECall name args -> do
        ops <- getOps
        case Map.lookup name ops of
            Just (params, body) -> do
                if length params == length args
                then do
                    let subMap = Map.fromList (zip params args)
                        expanded = subst subMap body -- reemplazo los parametros por los argumentos
                    optimizador expanded  -- optimizo la expresion expandida

            -- Decidimos optimizar los argumentos en vez de reportar los Errores.
            -- Los errores los reportaremos solamente en el modulo Eval.
                -- Si la cantidad de parametros no coincide con la cantidad de argumentos:
                else ECall name <$> mapM optimizador args
            
            -- Si no encuentra la operacion:
            Nothing -> ECall name <$> mapM optimizador args

    -- Efectuar selecciones antes que las reuniones:
    ESeleccion c (ENaturalJoin e1 e2) ->  
        seleccionSobreProd ENaturalJoin (fnc c) e1 e2

    ESeleccion c (EProd e1 e2) -> 
        seleccionSobreProd EProd (fnc c) e1 e2

    -- Combino selecciones:
    ESeleccion c0 (ESeleccion c1 e) -> optimizador (ESeleccion (PAnd c0 c1) e)

    -- Elimino proyecciones demas:
    EProyeccion a1 (EProyeccion _ e) -> optimizador (EProyeccion a1 e)

    -- Operaciones comunes:
    ESeleccion cond e -> ESeleccion (fnc cond) <$> optimizador e

    EProyeccion attrs e -> EProyeccion attrs <$> optimizador e

    ERenombre a1 a2 e -> ERenombre a1 a2 <$> optimizador e

    EUnion e0 e1 -> EUnion <$> optimizador e0 <*> optimizador e1

    EDiff e0 e1  -> EDiff  <$> optimizador e0 <*> optimizador e1

    EProd e0 e1 -> EProd <$> optimizador e0 <*> optimizador e1

    EInterseccion e0 e1 -> EInterseccion <$> optimizador e0 <*> optimizador e1

    ENaturalJoin e0 e1 -> ENaturalJoin <$> optimizador e0 <*> optimizador e1

    EDiv e0 e1 -> EDiv <$> optimizador e0 <*> optimizador e1


-- seleccionSobreProd: aplica la seleccion sobre el producto
seleccionSobreProd :: (Expr -> Expr -> Expr) -> Cond -> Expr -> Expr -> StateError Expr
seleccionSobreProd constr c e1 e2 = do
    
    let attrsC = attrsCond c
    attrs1 <- attrsExpr e1 
    attrs2 <- attrsExpr e2 

    -- Vemos si los atributos de la condicion se pueden aplicar sobre e1 y e2
    let enE1 = all (`elem` attrs1) attrsC
        enE2 = all (`elem` attrs2) attrsC

    -- Si los argumentos de la condicion estan en e1, aplico la seleccion solo sobre e1
    if enE1 && not enE2 
        then do
             optE1 <- optimizador (ESeleccion c e1)
             optE2 <- optimizador e2
             return (constr optE1 optE2)

    -- Si los argumentos de la condicion estan en e2, aplico la seleccion solo sobre e2                                             
    else if enE2 && not enE1
        then do
             optE1 <- optimizador e1
             optE2 <- optimizador (ESeleccion c e2)
             return (constr optE1 optE2)
    
    -- Si los argumentos de la condicion estan en ambos, aplico la seleccion sobre ambos                                                                                        
    else do
         optE1 <- optimizador e1
         optE2 <- optimizador e2
         return (ESeleccion c (constr optE1 optE2))


-- attrsExpr: obtiene los atributos de una expresion
attrsExpr :: Expr -> StateError [Atributo]
attrsExpr expr = case expr of

  -- Relación base
  ERelacion name -> do
    rels <- getRels
    case Map.lookup name rels of
      Just rel -> return (map fst (atributos rel))                       
      Nothing  -> return [] -- Evitamos error en optimizador, lo manejará Eval

  ECall name args -> do
    ops <- getOps
    case Map.lookup name ops of
      Just (params, body) -> do
        if length params == length args
        then do
          let subMap = Map.fromList (zip params args)
              expanded = subst subMap body 
          attrsExpr expanded
        else return [] -- Error diferido a Eval
      Nothing -> return [] -- Error diferido a Eval

  -- Selección NO cambia atributos
  ESeleccion _ e ->
    attrsExpr e

  -- Proyección define los atributos
  EProyeccion attrs _ ->
    return attrs

  -- Renombre
  ERenombre old new e -> do
    ats <- attrsExpr e
    return (map (\a -> if a == old then new else a) ats)

  -- Producto cartesiano
  EProd e1 e2 -> do
    a1 <- attrsExpr e1
    a2 <- attrsExpr e2
    return (a1 ++ a2)

  -- Natural join (evita duplicados)
  ENaturalJoin e1 e2 -> do
    a1 <- attrsExpr e1
    a2 <- attrsExpr e2
    return (a1 ++ filter (`notElem` a1) a2)

  -- Unión (mismo esquema)
  EUnion e1 _ ->
    attrsExpr e1

  -- Diferencia
  EDiff e1 _ ->
    attrsExpr e1

  -- Intersección
  EInterseccion e1 _ ->
    attrsExpr e1

  -- División
  EDiv e1 e2 -> do
    a1 <- attrsExpr e1
    a2 <- attrsExpr e2
    return (filter (`notElem` a2) a1)


-- attrsCond: obtiene los atributos de una condicion
attrsCond :: Cond -> [Atributo]
attrsCond PTrue          = []
attrsCond PFalse         = []
attrsCond (PEq a _)      = [a]
attrsCond (PNeq a _)     = [a]
attrsCond (PLt a _)      = [a]
attrsCond (PGt a _)      = [a]
attrsCond (PAttrEq a b)  = [a,b]
attrsCond (PAnd c1 c2)   = attrsCond c1 ++ attrsCond c2 
attrsCond (POr c1 c2)    = attrsCond c1 ++ attrsCond c2
attrsCond (PNot c)       = attrsCond c


-- fnc: forma normal conjuntiva, distribuye el or sobre el and
fnc :: Cond -> Cond
fnc (POr c0 (PAnd c1 c2)) =
  fnc (PAnd (POr c0 c1) (POr c0 c2))

fnc (POr (PAnd c1 c2) c0) =
  fnc (PAnd (POr c1 c0) (POr c2 c0))

fnc (PAnd c1 c2) =
  PAnd (fnc c1) (fnc c2)

fnc (POr c1 c2) =
  POr (fnc c1) (fnc c2)

fnc c = c

