module Token

open System

type Token =
    | ILLEGAL
    | EOF
    // Identifires + literals
    | IDENT of string // add, foobar, x, y
    | INT of int // 123456
    // Operators
    | ASSIGN
    | PLUS
    | MINUS
    | BANG
    | ASTERISK
    | SLASH
    // Delimiters
    | COMMA // = ','
    | SEMICOLON // = ';'
    // paranse
    | LPAREN // = '('
    | RPAREN // = ')'
    | LBRACE // = '{'
    | RBRACE // = '}'
    // Comparison
    | EQ
    | NOT_EQ
    | LT
    | GT
    | GE
    | LT_EQ
    | GT_EQ
    // Keywords
    | FUNCTION
    | LET
    | TRUE
    | FALSE
    | IF
    | ELSE
    | RETURN



type Lexer(input: string) =
    let mutable position = 0

    member __.Position
        with get () = position
        and set newPosition = position <- newPosition

    member __.Input = input

    member __.Char =
        if position >= input.Length then
            char (0)
        else
            input[position]

    member self.ReadChar() = self.ReadChars 1

    member __.ReadChars n =
        if position < input.Length then
            position <- position + n




module Lexer =


    let readSpaces (lexer: Lexer) =
        let isSpace char =
            match char with
            | ' '
            | '\n'
            | '\t'
            | '\r' -> true
            | _ -> false

        while isSpace lexer.Char do
            lexer.ReadChar()

    type TokenReader = Lexer -> Token option

    let charTokenizer char token : TokenReader =
        fun lexer ->
            let token = if lexer.Char = char then Some token else None

            if token.IsSome then
                lexer.ReadChar()

            token

    let stringTokenizer (str: string) token : TokenReader =
        fun lexer ->
            let isEqual =
                str.Length <= lexer.Input.Length - lexer.Position
                && lexer.Input
                    .AsSpan(lexer.Position, str.Length)
                    .Equals(str, StringComparison.Ordinal)

            let token = if isEqual then Some token else None

            if token.IsSome then
                lexer.ReadChars str.Length

            token

    let anySeqTokenizer isValid binder : TokenReader =
        fun lexer ->
            let position = lexer.Position

            while isValid lexer.Char do
                lexer.ReadChar()

            let length = lexer.Position - position

            let token =
                if length <= 0 then
                    None
                else
                    lexer.Input[position .. lexer.Position - 1] |> binder

            if token.IsNone then
                lexer.Position <- position

            token

    let orElse (tokenizer1: TokenReader) (tokenizer2: TokenReader) : TokenReader =
        fun lexer -> lexer |> tokenizer1 |> Option.orElseWith (fun () -> lexer |> tokenizer2)

    let choice readers = readers |> List.reduce orElse

    let tokenizeKewwords: TokenReader =
        choice [
            stringTokenizer "fn" FUNCTION
            stringTokenizer "let" LET
            stringTokenizer "true" TRUE
            stringTokenizer "false" FALSE
            stringTokenizer "if" IF
            stringTokenizer "else" ELSE
            stringTokenizer "return" RETURN
        ]


    let tokenizOperators: TokenReader =
        choice [
            stringTokenizer "==" EQ
            stringTokenizer "!=" NOT_EQ
            stringTokenizer "<=" LT_EQ
            stringTokenizer ">=" GT_EQ
            charTokenizer '=' ASSIGN
            charTokenizer '+' PLUS
            charTokenizer '-' MINUS
            charTokenizer '!' BANG
            charTokenizer '*' ASTERISK
            charTokenizer '/' SLASH
            charTokenizer '<' LT
            charTokenizer '>' GT
        ]

    let tokenizeParanse: TokenReader =
        choice [
            charTokenizer '(' LPAREN
            charTokenizer ')' RPAREN
            charTokenizer '{' LBRACE
            charTokenizer '}' RBRACE
        ]

    let tokenizeDelimiter: TokenReader =
        choice [ charTokenizer ';' SEMICOLON; charTokenizer ',' COMMA ]


    let tokenizeIdent: TokenReader =
        let isLetter letter =
            System.Char.IsLetter letter || letter = '_'

        anySeqTokenizer isLetter (IDENT >> Some)

    let tokenizeInt: TokenReader =
        let isDigit c =
            match c with
            | _ when c >= '0' && c <= '9' -> true
            | '-'
            | '+'
            | '_' -> true
            | _ -> false

        let tryParse (str: string) =
            match str.Replace("_", "") |> System.Int32.TryParse with
            | (true, n) -> n |> INT |> Some
            | _ -> None

        anySeqTokenizer isDigit tryParse

    let tokenizeEOF: TokenReader = charTokenizer (char 0) EOF

    let NextToken lexer =
        lexer |> readSpaces

        lexer
        |> choice [
            tokenizeKewwords
            tokenizOperators
            tokenizeDelimiter
            tokenizeParanse
            tokenizeInt
            tokenizeIdent
            tokenizeEOF
        ]
        |> Option.defaultValue ILLEGAL

    let tokenize input =
        seq {
            let lexer = Lexer input
            let mutable running = true

            while running do
                let token = lexer |> NextToken
                yield token
                running <- token <> EOF && token <> ILLEGAL
        }
