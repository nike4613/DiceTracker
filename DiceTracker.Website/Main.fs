module DiceTracker.Website.Main

open Elmish
open Bolero
open Bolero.Html
open Bolero.Remoting.Client
open Microsoft.JSInterop
open XPlot.Plotly
open FSharp.Compiler.Diagnostics

type Model =
    {
        compiler: Compiler
        source: string
        plotScript: (string * string) option
        compilerMessages: FSharpDiagnostic seq option
        warnings: string list option
        errors: string list option
    }

type Message =
    | UpdateText of string
    | PlotChart of PlotlyChart
    | Compile
    | Compiled of Compiler
    | Evaluate of System.Reflection.MemberInfo
    | EvalJs of string
    | EvalFinished of unit
    | Error of exn
    
let defaultSource = "module Dice\nlet result = ()"

let initModel compiler source =
    {
        compiler = compiler
        source = source
        plotScript = None
        compilerMessages = None
        warnings = None
        errors = None
    }

let update (js: IJSRuntime) message model =
    match message with
    | UpdateText t ->  { model with source = t }, Cmd.none
    | PlotChart c -> 
        { model with plotScript = Some (c.Id, c.GetPlottingJS()) }, Cmd.none
    | Compile ->
        let compiler, run = model.compiler.Run model.source
        { model with compiler = compiler }, Cmd.OfAsync.either (run >> Async.WithYield) () Compiled Error
    | Compiled ({ status = Succeeded(_, Some memb, msgs) } as compiler) ->
        { model with
            compiler = compiler
            compilerMessages = Some (msgs :> _)
            warnings = None
            errors = None }, Cmd.ofMsg (Evaluate memb)
    | Compiled ({ status = Succeeded(_, None, msgs) } as compiler) ->
        { model with
            compiler = compiler
            compilerMessages = Some (msgs :> _)
            warnings = None
            errors = Some ["No value Dice.result defined."] }, Cmd.none
    | Compiled ({ status = Failed (Choice1Of2 msgs) } as compiler) ->
        { model with
            compiler = compiler
            compilerMessages = Some (msgs :> _)
            warnings = None
            errors = None }, Cmd.none
    | Compiled ({ status = Failed (Choice2Of2 msgs) } as compiler) ->
        { model with
            compiler = compiler
            compilerMessages = None
            warnings = None
            errors = Some msgs }, Cmd.none
    | Compiled compiler ->
        { model with
            compiler = compiler
            compilerMessages = None
            warnings = None
            errors = None }, Cmd.none
    | Evaluate memb -> { model with warnings = Some ["todo: evaluate member " + string memb] }, Cmd.none
    | EvalJs script -> model, Cmd.OfJS.either js "eval" [| script |] EvalFinished Error
    | EvalFinished() -> model, Cmd.none
    | Error exn ->
        eprintfn "%s" (Utils.getErrorMessage exn)
        { model with 
            compilerMessages = None
            warnings = None
            errors = Some [ Utils.getErrorMessage exn ] }, Cmd.none

type Main = Template<"main.html">

let compilerMsg (msg: FSharpDiagnostic) =
    Main.CompilerMessage()
        .Severity(string msg.Severity)
        .StartLine(string msg.StartLine)
        .StartColumn(string msg.StartColumn)
        .EndLine(string msg.EndLine)
        .EndColumn(string msg.EndColumn)
        .Message(msg.Message)
        .Select(fun _ -> ())
        .Elt()

let simpleMsg (severity: string) (msg: string) =
    Main.SimpleMessage()
        .Severity(severity)
        .Message(msg)
        .Elt()

let view (model: Model) dispatch =
    Main()
        .PlotlyUrl("placeholder")
        .EditorText(model.source, Action.ofValFn (UpdateText >> dispatch))
        .Evaluate(fun _ -> dispatch Compile)
        .Messages(concat [
            text <|
                match model.compiler.status with
                | CompilerStatus.Standby -> "Ready."
                | CompilerStatus.Running -> "Compiling..."
                | CompilerStatus.Succeeded _ -> "Compilation finished."
                | CompilerStatus.Failed _ -> "Compilation failed."
            cond model.errors <| function
                | None -> empty
                | Some msgs -> forEach msgs (simpleMsg "Error")
            cond model.warnings <| function
                | None -> empty
                | Some msgs -> forEach msgs (simpleMsg "Warning")
            cond model.compilerMessages <| function
                | None -> empty
                | Some msgs -> forEach (msgs |> Seq.sortBy (fun m -> m.Severity)) (compilerMsg)
        ])
        .Elt()
