namespace FsLexYacc.Basic.Ast

type MathExpr = 
    | Minus   of MathExpr * MathExpr
    | Plus    of MathExpr * MathExpr
    | Int     of int

type ParsedMathLine = ParsedMathLine of MathExpr list