module Ast

open Token
open System.Collections.Generic

type Identifier = Identifier of string

type Expression =
    | IntegerLiteralExpr of int
    | IdentifierExpr of Identifier

type Statement =
    | LetStmt of Identifier * Expression
    | ReturnStmt of Expression
    | SyntaxError


type Program = Statement seq
type TokenRunner = IEnumerator<Token * Token>


type Parser = TokenRunner -> Statement option

let parseExpr (tokens: TokenRunner) =
    match tokens.Current |> fst with
    | INT i -> IntegerLiteralExpr i |> Some
    | IDENT id -> Identifier id |> IdentifierExpr |> Some
    | _ -> None

let parseLet (tokens: IEnumerator<Token * Token>) =
    match tokens.Current with
    | LET, IDENT name ->
        if tokens.MoveNext() then
            match parseExpr tokens with
            | None -> SyntaxError |> Some
            | Some expr -> LetStmt(Identifier name, expr) |> Some
        else
            SyntaxError |> Some
    | _ -> None


let parseReturn (tokens: TokenRunner) =
    match tokens.Current with
    | RETURN, IDENT name ->
        if tokens.MoveNext() then
            match parseExpr tokens with
            | Error err -> Error err
            | Ok expr -> LetStmt(Identifier name, expr) |> Ok
        else
            Error FatalError
    | _ -> Error Failed

let orElse parser1 parser2 : Parser =
    fun tokens ->
        match parser1 tokens with
        | Ok stmt -> Ok stmt
        | Error Failed -> parser2 tokens
        | Error FatalError -> Error FatalError

let choice tokens : Parser= tokens |> List.reduce orElse


let parse (tokens: Token seq) : Program = seq {
    let enumerator = tokens |> Seq.pairwise |> _.GetEnumerator() //  last token will be end of file so it's ok it will never be in the current token
    while enumerator.MoveNext() do
        let x = enumerator|> choice [
            parseLet
            parseReturn
        ]

        x
}