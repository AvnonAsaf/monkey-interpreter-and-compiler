open Repel

open System

start Console.In Console.Out
|> Async.AwaitTask
|> Async.RunSynchronously