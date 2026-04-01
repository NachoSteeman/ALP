import Parser
main = do
  content <- readFile "Ejemplos/relacionesBase.ar"
  print $ parse' content
