module Repel

open Token
open System
open System.IO

let start (reader: TextReader) (writer: TextWriter) = task {
    let prompt = ">> "
    
    while true do
        do! writer.WriteAsync(prompt)
        let! line = reader.ReadLineAsync()
        for token in Lexer.tokenize line do
            do! writer.WriteLineAsync(token.ToString())
}

