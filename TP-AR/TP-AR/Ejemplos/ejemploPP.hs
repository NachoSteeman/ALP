imoprt AST 

e17 :: Expr
e17 =
  EProyeccion ["Nombre"]
    (ESeleccion
        (PAnd
          (PGt "Edad" (VInt 18))
          (PNot (PEq "Ciudad" (VString "BuenosAires")))
        )
        (ENaturalJoin
            (ERelacion "Personas")
            (ERelacion "Inscripciones")
        )
    )

e15 :: Expr
e15 = EGroup
        ["Ciudad"]
        [(Count,"Nombre")]
        (ERelacion "Personas")

e5 :: Expr
e5 = ESeleccion
        (POr
          (PGt "Edad" (VInt 18))
          (PLt "Edad" (VInt 10))
        )
        (ERelacion "Personas")