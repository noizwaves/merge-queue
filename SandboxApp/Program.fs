﻿module MergeQueue.App

open System
open Suave
open Suave.Filters
open Suave.Operators
open Suave.Utils


let run (port: int) =
    let local = Suave.Http.HttpBinding.createSimple HTTP "0.0.0.0" port

    let config =
        { defaultConfig with bindings = [ local ] }

    // quick and dirty data store
    let mutable state = Domain.emptyMergeQueue
    let load(): Domain.State = state
    let save (newState: Domain.State): unit = state <- newState

    let app: WebPart =
        choose
            [ GET >=> path "/api/" >=> request (SimpleSandboxApi.view load)
              GET >=> pathScan "/api/enqueue/%i" (SimpleSandboxApi.enqueue load save)
              GET >=> pathScan "/api/fireandforget/%i" (SimpleSandboxApi.fireAndForget load save)
              GET >=> pathScan "/api/dequeue/%i" (SimpleSandboxApi.dequeue load save)
              GET >=> path "/api/start" >=> request (SimpleSandboxApi.start load save)
              GET >=> path "/api/finish" >=> request (SimpleSandboxApi.finish load save)
              RequestErrors.NOT_FOUND "404" ]
    startWebServer config app
    0

[<EntryPoint>]
let main _argv =
    let port =
        Environment.GetEnvironmentVariable "PORT"
        |> Parse.int32
        |> Choice.fold id (fun _ -> 8080)

    printfn "Hello World from F#!"

    run port