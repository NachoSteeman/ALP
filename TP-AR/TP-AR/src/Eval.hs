module Eval where


import AST
import Utils
import PrettyPrinter
import Monads
import qualified Data.Map as Map

liftEither :: Either String a -> StateError a
liftEither (Left msg)  = throw (OperacionNoExiste msg)
liftEither (Right val) = return val

evalAndPrint :: State -> Expr -> IO ()
evalAndPrint s expr =
  case runStateError (evalExpr expr) s of
    Left err -> print err
    Right (rel, _) -> putStrLn (prettyRelacion rel)




evalExpr :: Expr -> StateError Relacion
evalExpr expr = case expr of

  ERelacion name -> do
    st <- get
    let ctx = ctxt st
    case Map.lookup name (relaciones ctx) of
      Nothing -> throw (RelacionNoExiste name)
      Just r  -> return r

  ESeleccion cond e -> do
    r <- evalExpr e
    return (seleccion r cond)

  EProyeccion attrs e -> do
    r <- evalExpr e
    liftEither (proyeccion attrs r)

  EUnion e1 e2 -> do
    r1 <- evalExpr e1
    r2 <- evalExpr e2
    liftEither (union r1 r2)

  EDiff e1 e2 -> do
    r1 <- evalExpr e1
    r2 <- evalExpr e2
    liftEither (diferencia r1 r2)

  EProd e1 e2 -> do
    r1 <- evalExpr e1
    r2 <- evalExpr e2
    return (productoCartesiano r1 r2)

  EInterseccion e1 e2 -> do
    r1 <- evalExpr e1
    r2 <- evalExpr e2
    liftEither (interseccion r1 r2)

  ENaturalJoin e1 e2 -> do
    r1 <- evalExpr e1
    r2 <- evalExpr e2
    return (joinNatural r1 r2)

  EDiv e1 e2 -> do
    r1 <- evalExpr e1
    r2 <- evalExpr e2
    liftEither (division r1 r2)

  _ -> throw (OperacionNoExiste "Operacion no soportada")