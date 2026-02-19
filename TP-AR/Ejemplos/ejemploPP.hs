empleados :: Relacion
empleados = R 
    { atributos = Set.fromList ["nombre", "edad", "dpto"]
    , tuplas = Set.fromList
        [ Map.fromList [("nombre", VString "Ana"), ("edad", VInt 25), ("dpto", VString "IT")]
        , Map.fromList [("nombre", VString "Luis"), ("edad", VInt 30), ("dpto", VString "HR")]
        , Map.fromList [("nombre", VString "María"), ("edad", VInt 28), ("dpto", VString "IT")]
        ]
    , nombre = "Empleados"
    }

main :: IO ()
main = do
    putStrLn $ prettyRelacion empleados
    