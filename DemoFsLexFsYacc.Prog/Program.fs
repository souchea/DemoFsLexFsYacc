namespace FsLexYacc.Demo

open System
open System.Text
open System.Collections.Generic
open Parser
open Microsoft.FSharp.Text.Lexing
open Microsoft.FSharp.Text.Parsing
open FsLexYacc.Ast
open FsLexYacc.Parser

module FsLexYacc = 

    let ParseMyValue value (props: (string * string) list) =
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
                LexToString (GetExprList <| start FsLexYacc.Lexer.token lexbuf)

        with
            | _ -> failwith value


    [<EntryPoint>]
    let main argv = 
        let mutable props = [("company" , "Microsoft");
                             ("address", "Redmond");
                             ("number", "12345")];

        Console.Write("Enter your sentence: ")

        let input = Console.ReadLine()
 
        let result = ParseMyValue input props

        Console.WriteLine("Result: {0}", result)
 
        Console.ReadLine() |> ignore

        0
