{
module Parser where
import AST
import Token
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

}