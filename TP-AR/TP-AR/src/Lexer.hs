module Lexer where

import Token
import Data.Char

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)

    -- Ignorar espacios
    | isSpace c = lexer cs

    -- Símbolos simples
    | c == '('  = TLParen  : lexer cs
    | c == ')'  = TRParen  : lexer cs
    | c == '['  = TLBracket: lexer cs
    | c == ']'  = TRBracket: lexer cs
    | c == ','  = TComma   : lexer cs
    | c == ';'  = TSemicolon : lexer cs
    | c == '='  = TEq      : lexer cs
    | c == '<'  = TLt      : lexer cs
    | c == '>'  = TGt      : lexer cs

    -- Operador ->
    | c == '-' && not (null cs) && head cs == '>'
        = TArrow : lexer (tail cs)

    -- Operador !=
    | c == '!' && not (null cs) && head cs == '='
        = TNeq : lexer (tail cs)

    -- Números
    | isDigit c =
        let (num, rest) = span isDigit (c:cs)
        in TInt (read num) : lexer rest

    -- Strings entre comillas
    | c == '"' =
        let (str, rest) = span (/= '"') cs
        in TString str : lexer (tail rest)

    -- Identificadores o palabras reservadas
    | isAlpha c =
        let (word, rest) = span isAlphaNum (c:cs)
        in keyword word : lexer rest

    | otherwise = error ("Caracter inesperado: " ++ [c])


keyword :: String -> Token
keyword w = case w of
    "select"      -> TSelect
    "project"     -> TProject
    "rename"      -> TRename
    "group"       -> TGroup
    "union"       -> TUnion
    "diferencia"  -> TDiferencia
    "intersec"    -> TInterseccion
    "producto"    -> TProducto
    "division"    -> TDivision
    "naturaljoin" -> TNaturalJoin
    "join"        -> TJoin
    "and"         -> TAnd
    "or"          -> TOr
    "not"         -> TNot
    "true"        -> TTrue
    "false"       -> TFalse
    "count"       -> TCount
    "sum"         -> TSum
    "avg"         -> TAvg
    "min"         -> TMin
    "max"         -> TMax
    "null"        -> TNull
    _             -> TIdentifier w