module ParsingTest

open Xunit
open Token
open Ast

[<Fact>]
let ``test let statements`` =
    let input =
        """
let x = 5;
let y = 10;
let foobar = 838383;
"""

    let output = input |> Lexer.tokenize |> parse

    let expected = [
        LetStmt(Identifier "x", IntegerLiteralExpr 5)
        LetStmt(Identifier "y", IntegerLiteralExpr 10)
        LetStmt(Identifier "foobar", IntegerLiteralExpr 838383)
    ]

    Assert.Equal<Program>(expected, output)
