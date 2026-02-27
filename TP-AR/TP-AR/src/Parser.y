{
module Parser where
import AST
import Data.Char
}




%name parseExpr
%tokentype { Token }
%error { parseError }

%token
    -- Operaciones unarias:
    select      { TSelect }
    project     { TProject }
    rename      { TRename }
    group       { TGroup }

    -- Operaciones binarias:
    union       { TUnion }
    diferencia  { TDiferencia }
    intersec    { TInterseccion }
    producto    { TProducto }
    division    { TDivision }

    naturaljoin { TNaturalJoin }
    join        { TJoin }


    -- Condiciones:
    and         { TAnd }
    or          { TOr }
    not         { TNot }
    true        { TTrue }
    false       { TFalse }
    '='         { TEq }
    '!='        { TNeq }
    '<'         { TLt }
    '>'         { TGt }


    -- Predicados Group:
    count       { TCount }
    sum         { TSum }
    avg         { TAvg }
    min         { TMin }
    max         { TMax }

    '('         { TLParen }
    ')'         { TRParen }
    '['         { TLBracket }
    ']'         { TRBracket }
    ','         { TComma }
    ';'         { TSemicolon }
    '->'        { TArrow }

    
    -- Valores:
    null        { TNull }
    ident       { TIdentifier $$ }
    int         { TInt $$ }
    string      { TString $$ }


-- Precedencias:
%left union diferencia intersec
%left producto division
%left naturaljoin join
%left or
%left and
%right not

%%


-- Reglas Gramaticales:

-- Para Expresiones:

-- Prioridad baja: Union, Interseccion, Diferencia
-- Prioridad media: Producto, Division
-- Prioridad alta: OpUnarias
Expr
    : BinExpr { $1 }

BinExpr
    : BinExpr union      JoinExpr { EUnion $1 $3 }
    | BinExpr intersec   JoinExpr { EInterseccion $1 $3 }
    | BinExpr diferencia JoinExpr { EDiff $1 $3 }
    | JoinExpr { $1 }

JoinExpr
    : JoinExpr naturaljoin       ProdExpr { ENaturalJoin $1 $3 }
    | JoinExpr join '[' Cond ']' ProdExpr { EJoin $4 $1 $6 }
    | ProdExpr { $1 }

ProdExpr
    : ProdExpr producto BaseExpr { EProd $1 $3 }
    | ProdExpr division BaseExpr { EDiv $1 $3 }
    | BaseExpr { $1 }

BaseExpr
    : select '[' Cond ']' '(' Expr ')' { ESeleccion $3 $6 }
    | project '[' AttrList ']' '(' Expr ')' { EProyeccion $3 $6 }
    | rename '[' ident '->' ident ']' '(' Expr ')' { ERenombre $3 $5 $8 }
    | group '[' AttrList ';' AggList ']' '(' Expr ')' { EGroup $3 $5 $8 }
    | '(' Expr ')' { $2 }
    | ident { ERelacion $1 }
    
-- Para Obtener argumentos de una lista:
AttrList
    : ident                    { [$1] }
    | AttrList ',' ident       { $1 ++ [$3] }

AggList
    : Agg                      { [$1] }
    | AggList ',' Agg          { $1 ++ [$3] }

Agg
    : count '(' ident ')'      { (Count, $3) }
    | sum '(' ident ')'        { (Sum, $3) }
    | avg '(' ident ')'        { (Avg, $3) }
    | min '(' ident ')'        { (Min, $3) }
    | max '(' ident ')'        { (Max, $3) }

-- Para obtener las condiciones:
Cond
    : Cond and Cond            { PAnd $1 $3 }
    | Cond or Cond             { POr $1 $3 }
    | not Cond                 { PNot $2 }

    | ident '=' ident          { PAttrEq $1 $3 }
    | ident '=' Value          { PEq  $1 $3 }
    | ident '!=' Value         { PNeq $1 $3 }
    | ident '<' Value          { PLt  $1 $3 }
    | ident '>' Value          { PGt  $1 $3 }

    | true                     { PTrue }
    | false                    { PFalse }

    | '(' Cond ')'             { $2 }


-- Para obtener los valores:
Value
    : int                      { VInt $1 }
    | string                   { VString $1 }
    | true                     { VBool True }
    | false                    { VBool False }
    | null                     { VNull }



{
parseError :: [Token] -> a
parseError tokens = error ("Error en parser: " ++ show tokens)

-------------------------------------------------------------
-- Lexer
-------------------------------------------------------------

data Token
    = TSelect
    | TProject
    | TRename
    | TGroup
    | TUnion
    | TDiferencia
    | TInterseccion
    | TProducto
    | TDivision
    | TNaturalJoin
    | TJoin
    | TAnd
    | TOr
    | TNot
    | TTrue
    | TFalse
    | TEq
    | TNeq
    | TLt
    | TGt
    | TCount
    | TSum
    | TAvg
    | TMin
    | TMax
    | TLParen
    | TRParen
    | TLBracket
    | TRBracket
    | TComma
    | TSemicolon
    | TArrow
    | TNull
    | TIdentifier String
    | TInt Int
    | TString String
    deriving (Show, Eq)


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

parse :: String -> Expr
parse = parseExpr . lexer
}


