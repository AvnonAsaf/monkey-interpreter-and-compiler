module LexerTest

open Xunit
open Token

[<Fact>]
let ``test next token``() =
    
    let input = """
let five = 5;
let ten = 10;

let add = fn(x, y) {
    x + y;
};

let result = add(five, ten);
!-/*5;
5 < 10 > 5;

if (5 < 10) {
    return true;
} else {
    return false;
}

10 == 10;
10 != 9;
"""

    let supposed_result = [
        LET
        IDENT "five" 
        ASSIGN
        INT 5
        SEMICOLON
        LET
        IDENT "ten"
        ASSIGN
        INT 10
        SEMICOLON
        LET
        IDENT "add"
        ASSIGN
        FUNCTION
        LPAREN
        IDENT "x" 
        COMMA
        IDENT "y"
        RPAREN
        LBRACE
        IDENT "x"
        PLUS  
        IDENT "y"
        SEMICOLON
        RBRACE
        SEMICOLON
        LET
        IDENT "result"
        ASSIGN
        IDENT "add"
        LPAREN
        IDENT "five"
        COMMA
        IDENT "ten"
        RPAREN
        SEMICOLON
        BANG
        MINUS
        SLASH
        ASTERISK
        INT 5
        SEMICOLON
        INT 5
        LT
        INT 10
        GT
        INT 5
        SEMICOLON
        IF
        LPAREN
        INT 5
        LT
        INT 10
        RPAREN
        LBRACE
        RETURN
        TRUE
        SEMICOLON
        RBRACE
        ELSE
        LBRACE
        RETURN
        FALSE
        SEMICOLON
        RBRACE
        INT 10
        EQ
        INT 10
        SEMICOLON
        INT 10
        NOT_EQ
        INT 9
        SEMICOLON
        EOF
    ]

    let result = Lexer.tokenize(input)
    Assert.Equal<Token list>(supposed_result, result |> Seq.toList)

