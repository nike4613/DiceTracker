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
    | Evaluate
    | EvaluateFinished of Evaluation.EvaluationResult
    | EvalJs of string
    | EvalFinished of unit
    
let defaultSource = "module Dice = let result = ()"

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
    | Evaluate -> model, Cmd.OfAsync.perform Evaluation.evaluate model.source EvaluateFinished
    | EvaluateFinished result -> model, Cmd.none
    | EvalJs script -> model, Cmd.OfJS.perform js "eval" [| script |] EvalFinished
    | EvalFinished() -> model, Cmd.none

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
        .Evaluate(fun _ -> dispatch Evaluate)
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
