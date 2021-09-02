
module Evaluation

open XPlot.Plotly
open FSharp.Compiler.Interactive.Shell
open System.IO
open FSharp.Compiler.Diagnostics
open DiceTracker
open System
open System.Reflection
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text

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

let checker = FSharpChecker.Create()

let evaluate expr : Async<EvaluationResult> =
    async {
        printfn "evaluating %s" expr
        
        try
            return Message "none"
        with
        | ex -> return Exception ex
    }

