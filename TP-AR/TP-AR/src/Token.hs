module Token where

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
