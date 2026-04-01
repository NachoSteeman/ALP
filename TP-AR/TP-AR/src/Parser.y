{
module Parser where
import Commons
import Data.Char
}

%name parseProgram
%tokentype { Token }
%error { parseError }
%monad { Either String } { >>= } { return }

%token
    -- Operaciones unarias:
    seleccion        { TSelect }
    proyeccion       { TProject }
    renombre         { TRename }

    -- Operaciones binarias:
    union            { TUnion }
    diferencia       { TDiferencia }
    interseccion     { TInterseccion }
    producto         { TProducto }
    division         { TDivision }
    productoNatural  { TNaturalJoin }

    -- Condiciones:
    and          { TAnd }
    or           { TOr }
    not          { TNot }
    true         { TTrue }
    false        { TFalse }
    '='          { TEq }
    '!='         { TNeq }
    '<'          { TLt }
    '>'          { TGt }

    -- Símbolos:
    '('          { TLParen }
    ')'          { TRParen }
    '['          { TLBracket }
    ']'          { TRBracket }
    ','          { TComma }
    ';'          { TSemicolon }
    '->'         { TArrow }
    ':'          { TColon }


    -- Comandos REPL (el ':' está incorporado al token):
    quit         { TQuit }
    help         { THelp }
    clear        { TClear }
    browse       { TBrowse }
    compile      { TCompile }
    reload       { TReload }
    createRel    { TCreateRel }
    insertRel    { TInsertRel }
    dropRel      { TDropRel }
    defineOP     { TDefineOP }

    -- Literales:
    null         { TNull }
    ident        { TIdentifier $$ }
    int          { TInt $$ }
    string       { TString $$ }

%left union diferencia interseccion
%left productoNatural
%left producto division
%left or
%left and
%right not

%%

Program
    : TopList       { $1 }

TopList
    : Top                   { [$1] }
    | TopList ';' Top       { $1 ++ [$3] }
    | TopList ';'           { $1 }

Top
    : ident '=' Expr        { TAssign $1 $3 }
    | Expr                  { TExpr $1 }
    | Cmd                   { TCmd $1 }

-- -------------------------------------------------------
-- Comandos REPL
-- -------------------------------------------------------
Cmd
    : quit                          { Quit }
    | help                          { Help }
    | clear                         { Clear }
    | browse                        { Browse }
    | reload                        { Reload }
    | compile string                { Compile $2 }
    | createRel ident AttrDefList   { CreateRel $2 $3 }
    | insertRel ident TuplaList     { InsertRel $2 $3 }
    | dropRel   ident               { DropRel $2 }
    | defineOP  ident Expr          { DefineOP $2 $3 }

AttrDefList
    : AttrDef                       { [$1] }
    | AttrDefList ',' AttrDef       { $1 ++ [$3] }

AttrDef
    : ident ':' ident               { ($1, parseType $3) }

TuplaList
    : TuplaExp                      { [$1] }
    | TuplaList ',' TuplaExp        { $1 ++ [$3] }

TuplaExp
    : '(' Tupla ')'                 { $2 }

Tupla
    : TuplaVal                      { [$1] }
    | Tupla ',' TuplaVal            { $1 ++ [$3] }

TuplaVal
    : Value                         { $1 }
    | ident                         { VString $1 }

-- -------------------------------------------------------
-- Expresiones
-- -------------------------------------------------------
Expr
    : BinExpr { $1 }

BinExpr
    : BinExpr union        JoinExpr  { EUnion $1 $3 }
    | BinExpr interseccion JoinExpr  { EInterseccion $1 $3 }
    | BinExpr diferencia   JoinExpr  { EDiff $1 $3 }
    | JoinExpr                       { $1 }

JoinExpr
    : JoinExpr productoNatural ProdExpr  { ENaturalJoin $1 $3 }
    | ProdExpr                           { $1 }

ProdExpr
    : ProdExpr producto  BaseExpr  { EProd $1 $3 }
    | ProdExpr division  BaseExpr  { EDiv $1 $3 }
    | BaseExpr                     { $1 }

BaseExpr
    : seleccion  '[' Cond     ']' '(' Expr ')'          { ESeleccion $3 $6 }
    | proyeccion '[' AttrList ']' '(' Expr ')'          { EProyeccion $3 $6 }
    | renombre   '[' ident '->' ident ']' '(' Expr ')'  { ERenombre $3 $5 $8 }
    | '(' Expr ')'                                       { $2 }
    | ident                                              { ERelacion $1 }

AttrList
    : ident                    { [$1] }
    | AttrList ',' ident       { $1 ++ [$3] }

-- -------------------------------------------------------
-- Condiciones
-- -------------------------------------------------------
Cond
    : Cond and Cond            { PAnd $1 $3 }
    | Cond or  Cond            { POr  $1 $3 }
    | not Cond                 { PNot $2 }
    | ident '=' ident          { PAttrEq $1 $3 }
    | ident '=' Value          { PEq  $1 $3 }
    | ident '!=' Value         { PNeq $1 $3 }
    | ident '<' Value          { PLt  $1 $3 }
    | ident '>' Value          { PGt  $1 $3 }
    | true                     { PTrue }
    | false                    { PFalse }
    | '(' Cond ')'             { $2 }

-- -------------------------------------------------------
-- Valores
-- -------------------------------------------------------
Value
    : int                      { VInt $1 }
    | string                   { VString $1 }
    | true                     { VBool True }
    | false                    { VBool False }
    | null                     { VNull }

{

-- -------------------------------------------------------
-- Parsers Auxiliares
-- -------------------------------------------------------


parseType :: String -> Type
parseType s = case map toLower s of
  "int"    -> PInt
  "string" -> PString
  "bool"   -> PBool
  _        -> error ("Tipo desconocido: " ++ s)

parseError :: [Token] -> Either String a
parseError [] =
  Left "Error de sintaxis: fin inesperado de la entrada. Revisa si faltan cerrar paréntesis o comillas."
parseError (tok:_) =
  let hint = case tok of
               TColon -> ". Comando no reconocido. Para ver los comandos disponibles ingrese: \':help\'" 
               _      -> ""
  in Left $ "Error de sintaxis cerca de " ++ showToken tok ++ hint

showToken :: Token -> String
showToken t = case t of
    TSelect       -> "la palabra 'seleccion'"
    TProject      -> "la palabra 'proyeccion'"
    TUnion        -> "la palabra 'union'"
    TDiferencia   -> "la palabra 'diferencia'"
    TInterseccion -> "la palabra 'interseccion'"
    TProducto     -> "la palabra 'producto'"
    TNaturalJoin  -> "la palabra 'productoNatural'"
    TDivision     -> "la palabra 'division'"
    TRename       -> "la palabra 'renombre'"
    TAnd          -> "el operador 'and'"
    TOr           -> "el operador 'or'"
    TNot          -> "el operador 'not'"
    TTrue         -> "el valor 'true'"
    TFalse        -> "el valor 'false'"
    TNull         -> "el valor 'null'"
    TEq           -> "el signo '='"
    TNeq          -> "el signo '!='"
    TLt           -> "el signo '<'"
    TGt           -> "el signo '>'"
    TLParen       -> "el paréntesis de apertura '('"
    TRParen       -> "el paréntesis de cierre ')'"
    TLBracket     -> "el corchete '['"
    TRBracket     -> "el corchete ']'"
    TComma        -> "la coma ','"
    TSemicolon    -> "el punto y coma ';'"
    TArrow        -> "la flecha '->'"
    TColon        -> "los dos puntos ':'"
    TIdentifier s -> "el identificador '" ++ s ++ "'"
    TInt n        -> "el número '" ++ show n ++ "'"
    TString s     -> "el texto \"" ++ s ++ "\""
    TQuit         -> "el comando ':quit'"
    THelp         -> "el comando ':help'"
    TCompile      -> "el comando ':compile'"
    TReload       -> "el comando ':reload'"
    TCreateRel    -> "el comando ':createRel'"
    TInsertRel    -> "el comando ':insertRel'"
    TDropRel      -> "el comando ':dropRel'"
    TDefineOP     -> "el comando ':defineOP'"
    _             -> "el símbolo desconocido (" ++ show t ++ ")"

-- -------------------------------------------------------
-- Tokens
-- -------------------------------------------------------
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
    | TSemicolon
    | TArrow
    | TColon
    -- Comandos REPL:
    | TQuit
    | THelp
    | TClear
    | TBrowse
    | TCompile
    | TReload
    | TCreateRel
    | TInsertRel
    | TDropRel
    | TDefineOP
    -- Literales:
    | TNull
    | TIdentifier String
    | TInt Int
    | TString String
    deriving (Show, Eq)

-- -------------------------------------------------------
-- Lexer
-- -------------------------------------------------------
lexer :: String -> Either Err [Token]
lexer [] = Right []
lexer (c:cs)
  | isSpace c = lexer cs
  | c == '('  = add TLParen
  | c == ')'  = add TRParen
  | c == '['  = add TLBracket
  | c == ']'  = add TRBracket
  | c == ','  = add TComma
  | c == ';'  = add TSemicolon
  | c == '='  = add TEq
  | c == '<'  = add TLt
  | c == '>'  = add TGt

  | c == '-' && not (null cs) && head cs == '>'
      = prepend TArrow (tail cs)

  -- Para trabajar numeros negativos:  
  | c == '-' && not (null cs) && isDigit (head cs) =
      let (num, rest) = span isDigit cs
      in prepend (TInt (read ("-" ++ num))) rest


  | c == '!' && not (null cs) && head cs == '='
      = prepend TNeq (tail cs)

  -- ':' seguido de letras → comando REPL si es conocido, sino TColon + keyword
  | c == ':' && not (null cs) && isAlpha (head cs) =
      let (word, rest) = span isAlphaNum cs
      in case keywordCmd word of
           Just tok -> prepend tok rest
           Nothing  -> case lexer rest of
                         Left err -> Left err
                         Right ts -> Right (TColon : keyword word : ts)

  -- ':' solo → separador atrib:tipo
  | c == ':' = add TColon

  -- Comentarios de línea:
  | c == '/' && not (null cs) && head cs == '/'
      = lexer (dropWhile (/= '\n') cs)

  | isDigit c =
      let (num, rest) = span isDigit (c:cs)
      in prepend (TInt (read num)) rest

  | c == '"' =
      case span (/= '"') cs of
        (str, '"':rest) -> prepend (TString str) rest
        _               -> Left "String sin cerrar"

  | isAlpha c || c == '_' =
      let (word, rest) = span (\x -> isAlphaNum x || x == '_') (c:cs)
      in prepend (keyword word) rest

  | otherwise = Left ("Caracter inesperado: " ++ [c])
  where
    add tok     = prepend tok cs
    prepend tok rest =
      case lexer rest of
        Left err -> Left err
        Right ts -> Right (tok : ts)

keyword :: String -> Token
keyword w = case w of
    "seleccion"       -> TSelect
    "proyeccion"      -> TProject
    "renombre"        -> TRename
    "union"           -> TUnion
    "diferencia"      -> TDiferencia
    "interseccion"    -> TInterseccion
    "producto"        -> TProducto
    "division"        -> TDivision
    "productoNatural" -> TNaturalJoin
    "and"             -> TAnd
    "or"              -> TOr
    "not"             -> TNot
    "true"            -> TTrue
    "false"           -> TFalse
    "null"            -> TNull
    _                 -> TIdentifier w

-- El ':' ya fue consumido, recibe solo la palabra:
keywordCmd :: String -> Maybe Token
keywordCmd w = case w of
    "quit"      -> Just TQuit
    "q"         -> Just TQuit
    "help"      -> Just THelp
    "h"         -> Just THelp
    "clear"     -> Just TClear
    "browse"    -> Just TBrowse
    "compile"   -> Just TCompile
    "load"      -> Just TCompile
    "l"         -> Just TCompile
    "c"         -> Just TCompile
    "reload"    -> Just TReload
    "r"         -> Just TReload
    "createRel" -> Just TCreateRel
    "insertRel" -> Just TInsertRel
    "dropRel"   -> Just TDropRel
    "defineOP"  -> Just TDefineOP
    _           -> Nothing

-- -------------------------------------------------------
-- Funciones exportadas
-- -------------------------------------------------------
parse' :: String -> Either String [TopLevel]
parse' input = do
  toks <- lexer input
  parseProgram toks

parse :: String -> Either String Expr
parse input = do
  tops <- parse' input
  case tops of
    [TExpr e] -> return e
    []        -> Left "Entrada vacía"
    _         -> Left "Se esperaba una única expresión"
}
