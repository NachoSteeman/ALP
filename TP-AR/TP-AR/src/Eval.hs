module Eval (
  evalExpr,
  evalAndPrint
) where


import Commons
import Operations
import PrettyPrinter
import Monads
import qualified Data.Map as Map





evalExpr :: Expr -> StateError Relacion
evalExpr expr = case expr of

  -- Operaciones basicas:
  
  ERelacion name -> do
    st <- get -- obtengo el estado del programa
    let rels = relaciones (ctxt st) -- obtengo las relaciones que estan en el contexto
    
    case Map.lookup name rels of    
      Nothing -> throw (RelacionNoExiste name)
      Just r  -> return r

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

  _ -> throw (OperacionNoExiste "Operacion no soportada")


evalEither :: Either Error a -> StateError a
evalEither (Left err)  = throw err 
evalEither (Right val) = return val

evalAndPrint :: State -> Expr -> IO ()
evalAndPrint s expr =
  case runStateError (evalExpr expr) s of
    Left err -> print err
    Right (rel, _) -> putStrLn (prettyRelacion rel)

