module Eval (
  evalExpr,
  evalAndPrint
) where


import Commons
import Operations
import PrettyPrinter
import Monads
import qualified Data.Map as Map




-- evalExpr: evalua una expresion y devuelve un error si no se puede evaluar o la relacion resultante junto al estado
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


-- evalAndPrint: muestra la relacion por pantalla o el error
evalAndPrint :: State -> Expr -> IO ()
evalAndPrint s expr =
  case runStateError (evalExpr expr) s of
    Left err -> print err
    Right (rel, _) -> putStrLn (prettyRelacion rel)

