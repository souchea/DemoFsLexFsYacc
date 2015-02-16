// Implementation file for parser generated by fsyacc
module FsLexYacc.Basic.Parser
#nowarn "64";; // turn off warnings that type variables used in production annotations are instantiated to concrete type
open Microsoft.FSharp.Text.Lexing
open Microsoft.FSharp.Text.Parsing.ParseHelpers
# 1 "Parser.fsy"

 open FsLexYacc.Basic.Ast
 
# 10 "Parser.fs"
// This type is the type of tokens accepted by the parser
type token = 
  | EOF
  | MINUS
  | PLUS
  | INT of (System.Int32)
// This type is used to give symbolic names to token indexes, useful for error messages
type tokenId = 
    | TOKEN_EOF
    | TOKEN_MINUS
    | TOKEN_PLUS
    | TOKEN_INT
    | TOKEN_end_of_input
    | TOKEN_error
// This type is used to give symbolic names to token indexes, useful for error messages
type nonTerminalId = 
    | NONTERM__startstart
    | NONTERM_start
    | NONTERM_ParsedMathLine
    | NONTERM_MathExpr
    | NONTERM_ExprList

// This function maps tokens to integers indexes
let tagOfToken (t:token) = 
  match t with
  | EOF  -> 0 
  | MINUS  -> 1 
  | PLUS  -> 2 
  | INT _ -> 3 

// This function maps integers indexes to symbolic token ids
let tokenTagToTokenId (tokenIdx:int) = 
  match tokenIdx with
  | 0 -> TOKEN_EOF 
  | 1 -> TOKEN_MINUS 
  | 2 -> TOKEN_PLUS 
  | 3 -> TOKEN_INT 
  | 6 -> TOKEN_end_of_input
  | 4 -> TOKEN_error
  | _ -> failwith "tokenTagToTokenId: bad token"

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
let prodIdxToNonTerminal (prodIdx:int) = 
  match prodIdx with
    | 0 -> NONTERM__startstart 
    | 1 -> NONTERM_start 
    | 2 -> NONTERM_ParsedMathLine 
    | 3 -> NONTERM_MathExpr 
    | 4 -> NONTERM_MathExpr 
    | 5 -> NONTERM_MathExpr 
    | 6 -> NONTERM_ExprList 
    | 7 -> NONTERM_ExprList 
    | _ -> failwith "prodIdxToNonTerminal: bad production index"

let _fsyacc_endOfInputTag = 6 
let _fsyacc_tagOfErrorTerminal = 4

// This function gets the name of a token as a string
let token_to_string (t:token) = 
  match t with 
  | EOF  -> "EOF" 
  | MINUS  -> "MINUS" 
  | PLUS  -> "PLUS" 
  | INT _ -> "INT" 

// This function gets the data carried by a token as an object
let _fsyacc_dataOfToken (t:token) = 
  match t with 
  | EOF  -> (null : System.Object) 
  | MINUS  -> (null : System.Object) 
  | PLUS  -> (null : System.Object) 
  | INT _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
let _fsyacc_gotos = [| 0us; 65535us; 1us; 65535us; 0us; 1us; 1us; 65535us; 0us; 2us; 4us; 65535us; 0us; 7us; 3us; 8us; 9us; 5us; 10us; 6us; 1us; 65535us; 0us; 3us; |]
let _fsyacc_sparseGotoTableRowOffsets = [|0us; 1us; 3us; 5us; 10us; |]
let _fsyacc_stateToProdIdxsTableElements = [| 1us; 0us; 1us; 0us; 1us; 1us; 2us; 2us; 7us; 1us; 3us; 3us; 4us; 4us; 5us; 3us; 4us; 5us; 5us; 3us; 4us; 5us; 6us; 3us; 4us; 5us; 7us; 1us; 4us; 1us; 5us; |]
let _fsyacc_stateToProdIdxsTableRowOffsets = [|0us; 2us; 4us; 6us; 9us; 11us; 15us; 19us; 23us; 27us; 29us; |]
let _fsyacc_action_rows = 11
let _fsyacc_actionTableElements = [|1us; 32768us; 3us; 4us; 0us; 49152us; 0us; 16385us; 1us; 16386us; 3us; 4us; 0us; 16387us; 2us; 16388us; 1us; 9us; 2us; 10us; 2us; 16389us; 1us; 9us; 2us; 10us; 2us; 16390us; 1us; 9us; 2us; 10us; 2us; 16391us; 1us; 9us; 2us; 10us; 1us; 32768us; 3us; 4us; 1us; 32768us; 3us; 4us; |]
let _fsyacc_actionTableRowOffsets = [|0us; 2us; 3us; 4us; 6us; 7us; 10us; 13us; 16us; 19us; 21us; |]
let _fsyacc_reductionSymbolCounts = [|1us; 1us; 1us; 1us; 3us; 3us; 1us; 2us; |]
let _fsyacc_productionToNonTerminalTable = [|0us; 1us; 2us; 3us; 3us; 3us; 4us; 4us; |]
let _fsyacc_immediateActions = [|65535us; 49152us; 16385us; 65535us; 16387us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; |]
let _fsyacc_reductions ()  =    [| 
# 94 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data :  FsLexYacc.Basic.Ast.ParsedMathLine )) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (Microsoft.FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : '_startstart));
# 103 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'ParsedMathLine)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 14 "Parser.fsy"
                                              _1 
                   )
# 14 "Parser.fsy"
                 :  FsLexYacc.Basic.Ast.ParsedMathLine ));
# 114 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'ExprList)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 16 "Parser.fsy"
                                                 ParsedMathLine(_1) 
                   )
# 16 "Parser.fsy"
                 : 'ParsedMathLine));
# 125 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : System.Int32)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 18 "Parser.fsy"
                                          Int(_1) 
                   )
# 18 "Parser.fsy"
                 : 'MathExpr));
# 136 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'MathExpr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'MathExpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 19 "Parser.fsy"
                                                   Minus(_1, _3) 
                   )
# 19 "Parser.fsy"
                 : 'MathExpr));
# 148 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'MathExpr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'MathExpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 20 "Parser.fsy"
                                                  Plus(_1, _3) 
                   )
# 20 "Parser.fsy"
                 : 'MathExpr));
# 160 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'MathExpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 23 "Parser.fsy"
                                          [_1] 
                   )
# 23 "Parser.fsy"
                 : 'ExprList));
# 171 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'ExprList)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'MathExpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 24 "Parser.fsy"
                                             _2 :: _1  
                   )
# 24 "Parser.fsy"
                 : 'ExprList));
|]
# 184 "Parser.fs"
let tables () : Microsoft.FSharp.Text.Parsing.Tables<_> = 
  { reductions= _fsyacc_reductions ();
    endOfInputTag = _fsyacc_endOfInputTag;
    tagOfToken = tagOfToken;
    dataOfToken = _fsyacc_dataOfToken; 
    actionTableElements = _fsyacc_actionTableElements;
    actionTableRowOffsets = _fsyacc_actionTableRowOffsets;
    stateToProdIdxsTableElements = _fsyacc_stateToProdIdxsTableElements;
    stateToProdIdxsTableRowOffsets = _fsyacc_stateToProdIdxsTableRowOffsets;
    reductionSymbolCounts = _fsyacc_reductionSymbolCounts;
    immediateActions = _fsyacc_immediateActions;
    gotos = _fsyacc_gotos;
    sparseGotoTableRowOffsets = _fsyacc_sparseGotoTableRowOffsets;
    tagOfErrorTerminal = _fsyacc_tagOfErrorTerminal;
    parseError = (fun (ctxt:Microsoft.FSharp.Text.Parsing.ParseErrorContext<_>) -> 
                              match parse_error_rich with 
                              | Some f -> f ctxt
                              | None -> parse_error ctxt.Message);
    numTerminals = 7;
    productionToNonTerminalTable = _fsyacc_productionToNonTerminalTable  }
let engine lexer lexbuf startState = (tables ()).Interpret(lexer, lexbuf, startState)
let start lexer lexbuf :  FsLexYacc.Basic.Ast.ParsedMathLine  =
    Microsoft.FSharp.Core.Operators.unbox ((tables ()).Interpret(lexer, lexbuf, 0))
