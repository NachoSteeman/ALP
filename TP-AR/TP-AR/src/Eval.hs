module Eval (
  evalExpr,
  subst
) where


import Commons
import Operations
import Monads
import qualified Data.Map as Map


-- Función auxiliar que realiza la substitución de parámetros por argumentos en una expresión.
subst :: Map.Map NombreRel Expr -> Expr -> Expr
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

-- evalExpr: evalua una expresion y devuelve un error si no se puede evaluar, o la relacion resultante junto al estado
evalExpr :: Expr -> StateError Relacion
evalExpr expr = case expr of

  ERelacion name -> do
    rels <- getRels
    
    case Map.lookup name rels of  -- Busco la relacion en el contexto
      Just r  -> return r
      Nothing -> throw (RelacionNoExiste name)

  -- Llamadas a operaciones definidas por el usuario:
  ECall name args -> do
    ops <- getOps
    case Map.lookup name ops of  -- Busco la operacion en el contexto
      Nothing -> throw (OperacionNoExiste name)
      Just (params, body) -> do
        if length args /= length params
          then throw (ErrorArgumentos name (length params) (length args))
          else do
            let subMap = Map.fromList (zip params args) -- Creo el mapa de substitucion
                exprSubst = subst subMap body           -- Realizo la substitucion
            evalExpr exprSubst

  -- Operaciones elementales:
  ESeleccion cond e -> do
    r <- evalExpr e
    evalEither (seleccion r cond)

  EProyeccion attrs e -> do
    r <- evalExpr e
    evalEither (proyeccion attrs r)

  EUnion e1 e2 -> do
    r1 <- evalExpr e1
    r2 <- evalExpr e2
    evalEither (union r1 r2)

  EDiff e1 e2 -> do
    r1 <- evalExpr e1
    r2 <- evalExpr e2
    evalEither (diferencia r1 r2)

  EProd e1 e2 -> do
    r1 <- evalExpr e1
    r2 <- evalExpr e2
    return (productoCartesiano r1 r2)


  -- Operaciones derivadas:
  EInterseccion e1 e2 -> do
    r1 <- evalExpr e1
    r2 <- evalExpr e2
    evalEither (interseccion r1 r2)

  ENaturalJoin e1 e2 -> do
    r1 <- evalExpr e1
    r2 <- evalExpr e2
    return (naturalJoin r1 r2)

  EDiv e1 e2 -> do
    r1 <- evalExpr e1
    r2 <- evalExpr e2
    evalEither (division r1 r2)

  ERenombre oldAttr newAttr e -> do
    r <- evalExpr e
    evalEither (renombre oldAttr newAttr r)


-- evalEither: devuelve la relacion o propaga el error
evalEither :: Either Error a -> StateError a
evalEither (Left err)  = throw err 
evalEither (Right val) = return val


