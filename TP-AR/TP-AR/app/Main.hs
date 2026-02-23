module Main where

import Parser
import Lexer
import AST

parseString :: String -> Expr
parseString = parseExpr . lexer


main :: IO ()
main = do
    let input = "group [a ; count(b), sum(c)] ( rename [a -> x] ( select [not (a = 5 and b > 3) or true] ( (A join [a = b] B) union (C naturaljoin D producto E) ) ) )"
    print (parseString input)

