namespace FsLexYacc.Basic.Ast

type Expr = 
    | Minus   of Expr * Expr
    | Plus    of Expr * Expr
    | Int     of int

type ParsedLine = ParsedLine of Expr list