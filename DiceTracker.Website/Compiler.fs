namespace DiceTracker.Website

open System
open System.IO
open System.Diagnostics
open System.Reflection
open FSharp.Compiler
open FSharp.Compiler.Text
open FSharp.Compiler.Symbols
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.CodeAnalysis
open Microsoft.JSInterop

type CompilerStatus =
    | Standby
    | Running
    | Failed of Choice<FSharpDiagnostic[], string list>
    | Succeeded of Assembly * result:MemberInfo option * FSharpDiagnostic[]

type FileResults =
    {
        parse: FSharpParseFileResults
        check: FSharpCheckFileResults
    }
module FileResults = let ofRes(parse, check) = { parse = parse ; check = check }

type Compiler =
    {
        checker: FSharpChecker
        options: FSharpProjectOptions
        checkResults: FSharpCheckProjectResults
        mainFile: FileResults
        sequence: int
        status: CompilerStatus
    }

module Compiler =

    // force the F# compiler to use /tmp/ as its bindir
    do Environment.SetEnvironmentVariable("FSHARP_COMPILER_BIN", "/tmp/")
    
    let libPath = "/tmp/lib/"

    let project = "/tmp/input.fsproj"
    let inFile = "/tmp/input.fs"
    let outFile = "/tmp/output.dll"

    let basicDependencies = 
        [
            "FSharp.Core"
            "Microsoft.CSharp"
            "mscorlib"
            "netstandard"
            "System"
            "System.Buffers"
            "System.Core"
            "System.Collections"
            "System.Data"
            "System.Data.Common"
            "System.Diagnostics.Debug"
            "System.Diagnostics.Tools"
            "System.Diagnostics.Tracing"
            "System.IO"
            "System.Linq"
            "System.Memory"
            "System.Net"
            "System.Net.Requests"
            "System.Net.WebClient"
            "System.Numerics"
            "System.Private.CoreLib"
            "System.Runtime"
            "System.Runtime.Numerics"
            "System.Runtime.Extensions"
            "System.ValueTuple"

            "MathNet.Numerics"
            "MathNet.Numerics.FSharp"
            "DiceTracker"
        ]

    let mkOptions (checker: FSharpChecker) outFile =
        (*{*) checker.GetProjectOptionsFromCommandLineArgs(project, [|
            yield "-o:" + outFile
            yield! basicDependencies |> List.toArray |> Array.map (fun s -> $"-r:{libPath}{s}.dll")
            yield! [|
                "--simpleresolution"
                "--optimize-"
                "--noframework"
                "--fullpaths"
                //$"--lib:{libPath}"
                "--warn:3"
                "--target:library"
                "--deterministic+"
                "--targetprofile:netcore"
                "--nocopyfsharpcore"
                "--warnaserror:3239"
                "--platform:anycpu"
                "--define:DEBUG"
                "--define:TRACE"
                #if NET
                "--define:NET"
                #endif
                #if NETCOREAPP
                "--define:NETCOREAPP"
                #endif
                #if NET6_0
                "--define:NET6_0"
                #endif
                #if NET5_0
                "--define:NET5_0"
                #endif
                #if NET5_0_OR_GREATER
                "--define:NET5_0_OR_GREATER"
                #endif
                #if NET6_0_OR_GREATER
                "--define:NET6_0_OR_GREATER"
                #endif
                "--define:NETCOREAPP1_0_OR_GREATER"
                "--define:NETCOREAPP1_1_OR_GREATER"
                "--define:NETCOREAPP2_0_OR_GREATER"
                "--define:NETCOREAPP2_1_OR_GREATER"
                "--define:NETCOREAPP2_2_OR_GREATER"
                "--define:NETCOREAPP3_0_OR_GREATER"
                "--define:NETCOREAPP3_1_OR_GREATER"
                inFile
            |]
        |]) //with SourceFiles = [|inFile|] }

    let create source = async {
        let checker = FSharpChecker.Create(keepAssemblyContents = false, suggestNamesForErrors = true)
        let options = mkOptions checker outFile
        File.WriteAllText(inFile, source)

        // start a check on creation so that we're ready to go when we finish initializing
        let! checkProjRes = checker.ParseAndCheckProject(options)
        let! (parseRes, checkRes) = checker.GetBackgroundCheckResultsForFileInProject(inFile, options)
        let! (diagnostics, code, assembly) = checker.CompileToDynamicAssembly(options.OtherOptions, None, "preCompile")

        printfn "%i %A" code assembly
        diagnostics |> Seq.iter (printfn "%A")

        return {
            checker = checker
            options = options
            checkResults = checkProjRes
            mainFile = FileResults.ofRes(parseRes, checkRes)
            sequence = 0
            status = Standby
        }
    }

    let isFailure (errors: FSharpDiagnostic seq) =
        errors |> Seq.exists (fun x -> x.Severity = FSharpDiagnosticSeverity.Error)

    let resultExpectedTypeOne = "DiceTracker.Probability.OutputValue"
    let resultExpectedTypeMany = "Microsoft.FSharp.Collections.seq<" + resultExpectedTypeOne + ">"

    let findResultMember (asmSig: FSharpAssemblySignature) =
        match asmSig.FindEntityByPath ["Dice"] with
        | None -> None
        | Some ent ->
            ent.MembersFunctionsAndValues
            |> Seq.filter (fun v ->
                printfn "%A (%A) -> %s : %s" v (v.TryGetFullDisplayName ()) v.CompiledName (v.FullType.Format FSharpDisplayContext.Empty)
                v.IsValue &&
                v.LogicalName = "result" &&
                    let typen = v.FullType.Format FSharpDisplayContext.Empty
                    typen = resultExpectedTypeOne || typen = resultExpectedTypeMany
            ) |> Seq.tryExactlyOne

    let checkDelay = Delayer(500) // the delayer to use for checking user input

open Compiler

type Compiler with
    member comp.Run (source: string) =
        { comp with status = Running },
        fun () -> async {
            let sw = Stopwatch.StartNew()
            let outfile = $"/tmp/out{comp.sequence}.dll"
            File.WriteAllText(inFile, source)

            let options = Compiler.mkOptions comp.checker outfile

            printfn "Starting compilation %A" options

            let! projectCheckResult = comp.checker.ParseAndCheckProject(options, "checkProject")

            let! parseResult, checkResult = comp.checker.GetBackgroundCheckResultsForFileInProject(inFile, options, "getParseResults")
            
            projectCheckResult.Diagnostics |> Seq.iter (printfn "%A")

            if isFailure projectCheckResult.Diagnostics then return { comp with status = Failed <| Choice1Of2 projectCheckResult.Diagnostics } else
                
            printfn "Finished first check."

            match findResultMember projectCheckResult.AssemblySignature with
            | None -> return { comp with status = Failed <| Choice2Of2 ["No result member found"] }
            | Some minfo ->

            // OtherOptions contains all of our actual options
            let! errors, errCode, assembly = comp.checker.CompileToDynamicAssembly(options.OtherOptions, None, "compile")
            sw.Stop()
            printfn "Compile took %A" sw.Elapsed

            printfn "%i %A" errCode assembly
            errors |> Seq.iter (printfn "%A")

            if isFailure errors || errCode <> 0 then return { comp with status = Failed <| Choice1Of2 errors } else
            match assembly with
            | None -> return { comp with status = Failed <| Choice2Of2 ["No assembly returned despite the apparently successful compilation"] }
            | Some assembly ->

            let diceModule = assembly.GetType(minfo.DeclaringEntity 
                                                |> Option.map (fun e -> e.CompiledName)
                                                |> Option.orElse (Some "Dice")
                                                |> Option.get) |> Option.ofObj
            let resultMember = diceModule |> Option.map (fun m -> m.GetMember(minfo.CompiledName, BindingFlags.Static ||| BindingFlags.Public))
            let resultMember = resultMember |> Option.map Array.tryHead |> Option.flatten

            return 
                { comp with
                    sequence = comp.sequence + 1
                    status = Succeeded(assembly, resultMember, errors)
                    mainFile = FileResults.ofRes(parseResult, checkResult) }
        }
