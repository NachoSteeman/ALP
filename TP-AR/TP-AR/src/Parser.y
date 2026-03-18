{
module Parser where
import Commons
import Data.Char
}



%name parseExpr
%tokentype { Token }

%error { parseError }
%monad { Either String } { >>= } { return }

%token
    -- Operaciones unarias:
    seleccion      { TSelect }
    proyeccion     { TProject }
    renombre      { TRename }

    -- Operaciones binarias:
    union       { TUnion }
    diferencia  { TDiferencia }
    interseccion    { TInterseccion }
    producto    { TProducto }
    division    { TDivision }

    naturaljoin { TNaturalJoin }


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

    '('         { TLParen }
    ')'         { TRParen }
    '['         { TLBracket }
    ']'         { TRBracket }
    ','         { TComma }
--    ';'         { TSemicolon }
    '->'        { TArrow }

    
    -- Valores:
    null        { TNull }
    ident       { TIdentifier $$ }
    int         { TInt $$ }
    string      { TString $$ }


-- Precedencias:
%left union diferencia interseccion
%left producto division
%left naturaljoin 
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
    | BinExpr interseccion   JoinExpr { EInterseccion $1 $3 }
    | BinExpr diferencia JoinExpr { EDiff $1 $3 }
    | JoinExpr { $1 }

JoinExpr
    : JoinExpr naturaljoin       ProdExpr { ENaturalJoin $1 $3 }
    | ProdExpr { $1 }

ProdExpr
    : ProdExpr producto BaseExpr { EProd $1 $3 }
    | ProdExpr division BaseExpr { EDiv $1 $3 }
    | BaseExpr { $1 }

BaseExpr
    : seleccion '[' Cond ']' '(' Expr ')' { ESeleccion $3 $6 }
    | proyeccion '[' AttrList ']' '(' Expr ')' { EProyeccion $3 $6 }
    | renombre '[' ident '->' ident ']' '(' Expr ')' { ERenombre $3 $5 $8 }
    | '(' Expr ')' { $2 }
    | ident { ERelacion $1 }
    
-- Para Obtener argumentos de una lista:
AttrList
    : ident                    { [$1] }
    | AttrList ',' ident       { $1 ++ [$3] }

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
parseError :: [Token] -> Either String a
parseError [] =
  Left "Error de sintaxis: fin inesperado de la entrada"

parseError (tok:_) =
  Left ("Error de sintaxis cerca de: " ++ show tok)
-------------------------------------------------------------
-- Lexer
-------------------------------------------------------------
data Token
    = TSelect
    | TProject
    | TRename
    | TUnion
    | TDiferencia
    | TInterseccion
    | TProducto
    | TDivision
    | TNaturalJoin
    | TAnd
    | TOr
    | TNot
    | TTrue
    | TFalse
    | TEq
    | TNeq
    | TLt
    | TGt
    | TLParen
    | TRParen
    | TLBracket
    | TRBracket
    | TComma
--    | TSemicolon
    | TArrow
    | TNull
    | TIdentifier String
    | TInt Int
    | TString String
    deriving (Show, Eq)






lexer :: String -> Either Err [Token]
lexer [] = Right []
lexer (c:cs)

  | isSpace c = lexer cs

  | c == '('  = add TLParen
  | c == ')'  = add TRParen
  | c == '['  = add TLBracket
  | c == ']'  = add TRBracket
  | c == ','  = add TComma
--  | c == ';'  = add TSemicolon
  | c == '='  = add TEq
  | c == '<'  = add TLt
  | c == '>'  = add TGt

  | c == '-' && not (null cs) && head cs == '>'
    = prepend TArrow (tail cs)

  | c == '!' && not (null cs) && head cs == '='
      = prepend TNeq (tail cs)

  | isDigit c =
      let (num, rest) = span isDigit (c:cs)
      in prepend (TInt (read num)) rest

  | c == '"' =
      case span (/= '"') cs of
        (str, '"':rest) -> prepend (TString str) rest
        _ -> Left "String sin cerrar"

  | isAlpha c || c == '_' =
      let (word, rest) = span (\x -> isAlphaNum x || x == '_') (c:cs)
      in prepend (keyword word) rest

  | otherwise = Left ("Caracter inesperado: " ++ [c])

  where
    add tok = prepend tok cs

    prepend tok rest =
      case lexer rest of
        Left err -> Left err
        Right ts -> Right (tok : ts)

keyword :: String -> Token
keyword w = case w of
    "seleccion"       -> TSelect
    "proyeccion"   -> TProject
    "renombre"       -> TRename
    "union"        -> TUnion
    "diferencia"   -> TDiferencia
    "interseccion" -> TInterseccion
    "producto"     -> TProducto
    "division"     -> TDivision
    "productoNatural"  -> TNaturalJoin
    "and"          -> TAnd
    "or"           -> TOr
    "not"          -> TNot
    "true"         -> TTrue
    "false"        -> TFalse
    "null"         -> TNull
    _              -> TIdentifier w



parse :: String -> Either String Expr
parse input = do
  toks <- lexer input
  parseExpr toks

}