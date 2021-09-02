
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
        printfn "current location: %s" (Directory.GetCurrentDirectory ())
        
        try
            (*use inStream = new StringReader("");
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
            | Choice1Of2 (Some v) ->*)

            let expr =
                """
                module Input
                open DiceTracker
                """ + expr

            let source = SourceText.ofString expr
            let! result = checker.ParseFile("input.fs", source, FSharpParsingOptions.Default, cache=false, userOpName="parse")
            if result.Diagnostics.Length > 0 then
                return Errors result.Diagnostics 
            else

            use outStream = new StringWriter()
            use errStream = new StringWriter()
            let! (errors, exitCode, assembly) =
                checker.CompileToDynamicAssembly([result.ParseTree], "input", ["FSharp.Core"; "DiceTracker"; "System.Private.CoreLib"], Some(outStream :> _, errStream :> _), noframework=true, userOpName="compile")

            if errors.Length > 0 then
                return Errors errors
            else
            match assembly with
            | None -> return Message "No assembly returned!"
            | Some assembly ->

            let ``type`` = assembly.GetType("Input", true);
            let prop = ``type``.GetProperty("result", BindingFlags.Static + BindingFlags.Public)
            if prop = null then
                return Message "Could not find 'result' binding!"
            else

            let value = prop.GetValue(null)

            let results =
                match value with
                | :? OutputValue as ov -> Processing.processOne ov |> Choice1Of2 
                | :? seq<OutputValue> as ovs -> Processing.processMany ovs |> Choice1Of2
                | v -> Choice2Of2 (v.GetType())
            match results with
            | Choice2Of2 t -> return Message $"Unexpected type {t}"
            | Choice1Of2 results ->
            
            return Exception (NotImplementedException())
        with
        | ex -> return Exception ex
    }

