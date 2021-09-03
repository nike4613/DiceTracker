
module DiceTracker.Website.App

open Bolero
open Microsoft.JSInterop
open Elmish
open Bolero.Html
open Bolero.Remoting.Client
open Bolero.Templating.Client
open System.Diagnostics

type AppModel =
    | Initializing
    | Running of Main.Model

type AppMessage =
    | InitCompiler
    | CompilerInitialized of Compiler
    | Message of Main.Message
    | Error of exn

let update (js: IJSInProcessRuntime) message model =
    match message with
    | InitCompiler ->
        model, Cmd.OfAsync.either (Async.WithYield << Compiler.create) Main.defaultSource CompilerInitialized Error
    | CompilerInitialized compiler ->
        Running (Main.initModel compiler Main.defaultSource), Cmd.none
    | Message msg ->
        match model with
        | Initializing -> model, Cmd.none
        | Running model ->
            let model, cmd = Main.update js msg model
            Running model, Cmd.map Message cmd
    | Error exn ->
        eprintfn "%A\nil offsets: %A" exn ((StackTrace exn).GetFrames() |> Array.map (fun e -> e.GetILOffset()) |> Array.toList)
        model, Cmd.none

type Loader = Template<"loading.html">

let view model dispatch = 
    cond model <| function
        | Initializing -> Loader().Text("Initializing compiler...").Elt()
        | Running model -> Main.view model (Message >> dispatch)

type Application() =
    inherit ProgramComponent<AppModel, AppMessage>()

    override this.Program =
        let update = update (this.JSRuntime :?> _)
        Program.mkProgram (fun _ -> Initializing, Cmd.ofMsg InitCompiler) update view
        |> Program.withConsoleTrace
        |> Program.withErrorHandler (fun (msg, exn) -> printfn "%s: %A" msg exn)
#if DEBUG
        |> Program.withHotReload
#endif