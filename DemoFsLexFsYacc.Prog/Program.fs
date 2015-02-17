namespace FsLexYacc.Demo

open System
open System.Text
open System.Collections.Generic
open Parser
open Microsoft.FSharp.Text.Lexing
open Microsoft.FSharp.Text.Parsing
open FsLexYacc.Ast
open FsLexYacc.Parser
open FsLexYacc.Basic.Ast
open FsLexYacc.Basic.Parser

module FsLexYacc = 

    let ParseMyValue2 value (props: (string * string) list) =
        try
            match value with
            | ""    -> ""
            | _     ->
                let lexbuf = LexBuffer<_>.FromString value

                let GetExprList prog =
                    match prog with
                    | ParsedLine(exprList) -> exprList

                let rec LexToString lex =
                    let rec GetResult lex1 (sb: StringBuilder) =
                        match lex1 with
                        | head :: tail  ->
                            let expression = GetStringFromExpr (head: Expr) (props)
                            sb.Append(expression: string) |> ignore
                            GetResult tail sb
                        | []            -> sb.ToString()
                    GetResult lex <| new StringBuilder("")
                LexToString (GetExprList <| FsLexYacc.Parser.start FsLexYacc.Lexer.token lexbuf)

        with
            | _ -> failwith value


    let ParseMyValue1 value=
        try
            match value with
            | ""    -> ""
            | _     ->
                let GetExprList  =
                    function
                    | ParsedMathLine(exprList) -> exprList

                let lexbuf = LexBuffer<_>.FromString value
                let test =  start FsLexYacc.Basic.Lexer.token lexbuf
                
                let rec ParseValue toParse=
                    match toParse with
                    | Minus(first, second)  -> (ParseValue first) - (ParseValue second)
                    | Plus(first, second)   -> (ParseValue first) + (ParseValue second)
                    | Int(value)            -> value

                let res = ParseValue (test |> GetExprList).Head
                res.ToString()


        with
            | _ -> failwith value


    let RunSimpleProg input =
        ParseMyValue1 input

    let RunCompleteProg input = 
        let mutable props = [("company" , "Microsoft");
                        ("address", "Redmond");
                        ("number", "12345")];

        ParseMyValue2 input props

    let GetFuncToUse = function
        | "1" -> RunSimpleProg
        | "2" -> RunCompleteProg
        | _   -> RunSimpleProg

    [<EntryPoint>]
    let main argv = 

        Console.WriteLine("Choose your Program: ")
        Console.WriteLine("1 -> Simple arithmetic expression ")
        Console.WriteLine("2 -> Complete example")
        Console.WriteLine("")
        let funcToUse = (Console.ReadLine() |> GetFuncToUse)

        Console.WriteLine("")
        Console.WriteLine("Enter the input you want to parse: ")
        let result = Console.ReadLine() |> funcToUse

        Console.WriteLine("Result: {0}", result)
 
        Console.ReadLine() |> ignore

        0
