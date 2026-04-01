module Optimizador (optimizador) where

import Commons
import Monads
import qualified Data.Map as Map


optimizador :: Expr -> StateError Expr
optimizador expr = case expr of

    ERelacion r -> do
        st <- get
        case Map.lookup r (operaciones (ctxt st)) of
            Just exprOp -> optimizador exprOp
            Nothing     -> return (ERelacion r)

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


seleccionSobreProd :: (Expr -> Expr -> Expr) -> Cond -> Expr -> Expr -> StateError Expr
seleccionSobreProd constr c e1 e2 = do
    let attrsC = attrsCond c
    attrs1 <- attrsExpr e1 
    attrs2 <- attrsExpr e2 

    let enE1 = all (`elem` attrs1) attrsC
        enE2 = all (`elem` attrs2) attrsC

    if enE1 && not enE2 
        then do
             optE1 <- optimizador (ESeleccion c e1)
             optE2 <- optimizador e2
             return (constr optE1 optE2)
                                                                     
    else if enE2 && not enE1
        then do
             optE1 <- optimizador e1
             optE2 <- optimizador (ESeleccion c e2)
             return (constr optE1 optE2)
                                                                                          
    else do
         optE1 <- optimizador e1
         optE2 <- optimizador e2
         return (ESeleccion c (constr optE1 optE2))


attrsExpr :: Expr -> StateError [Atributo]
attrsExpr expr = case expr of

  -- Relación base
  ERelacion name -> do
    st <- get
    let c = ctxt st
    case Map.lookup name (relaciones c) of
      Just rel -> return (map fst (atributos rel))                       
      Nothing -> case Map.lookup name (operaciones c) of            -- Si no lo encontre lo busco como uno definido por el usuario
                   Just exprOp -> attrsExpr exprOp
                   Nothing     -> throw (RelacionNoExiste name)

  -- Selección NO cambia atributos
  ESeleccion _ e ->
    attrsExpr e

  -- Proyección DEFINE los atributos
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

