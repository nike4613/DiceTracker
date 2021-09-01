
module Evaluation

open XPlot.Plotly
open FSharp.Compiler.Interactive.Shell
open System.IO
open FSharp.Compiler.Diagnostics
open DiceTracker
open System

let private config = FsiEvaluationSession.GetDefaultConfiguration()
// specify fsi.exe here so that it doesn't try to call Process.*
let private args = [| "fsi.exe"; "--noninteractive"; "--nologo"; "--gui-" |]

let private checkWarnings (warnings: FSharpDiagnostic seq) =
    if not (Seq.isEmpty warnings) then
        warnings
        |> Seq.map (fun w -> $"[{w.StartLine},{w.StartColumn}-{w.EndLine},{w.EndColumn}] {w.Message}")
        |> Seq.reduce (fun a b -> a + "\n" + b)
        |> failwith

let private checkResult result =
    match result with
    | Choice1Of2 _ -> ()
    | Choice2Of2 exn -> failwith $"exception {exn}"

let private checkEval (result, warnings) = 
    checkWarnings warnings
    checkResult result

type EvaluationResult =
    | Success of PlotlyChart
    | Errors of FSharpDiagnostic seq
    | Exception of exn
    | Message of string

let evaluate expr : EvaluationResult =
    printfn "evaluating %s" expr
    
    try
        use inStream = new StringReader("");
        use outStream = new StringWriter();
        use errStream = new StringWriter();
        let session = FsiEvaluationSession.Create(config, args, inStream, outStream, errStream, collectible=true)

        session.EvalInteractionNonThrowing("#r \"netstandard\"") |> checkEval
        session.EvalInteractionNonThrowing("#r \"System.Private.CoreLib\"") |> checkEval
        session.EvalInteractionNonThrowing("#r \"FSharp.Core\"") |> checkEval
        session.EvalInteractionNonThrowing("#r \"DiceTracker\"") |> checkEval
        session.EvalInteractionNonThrowing("open DiceTracker") |> checkEval

        let res, warnings = session.EvalInteractionNonThrowing(expr)
        if not (Seq.isEmpty warnings) then
            Errors warnings
        else
        match res with
        | Choice2Of2 ex -> Exception ex
        | Choice1Of2 None -> Message "no value returned"
        | Choice1Of2 (Some v) ->
        let results =
            match v.ReflectionValue with
            | :? OutputValue as ov -> Processing.processOne ov |> Choice1Of2 
            | :? seq<OutputValue> as ovs -> Processing.processMany ovs |> Choice1Of2
            | _ -> Choice2Of2 v.FSharpType
        match results with
        | Choice2Of2 t -> Message $"Unexpected type {t}"
        | Choice1Of2 results ->
        
        raise (NotImplementedException())
    with
    | ex -> Exception ex

