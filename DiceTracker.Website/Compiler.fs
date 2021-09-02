﻿namespace DiceTracker.Website

open System
open System.IO
open System.Reflection
open FSharp.Compiler
open FSharp.Compiler.Symbols
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.CodeAnalysis
open Microsoft.JSInterop

type CompilerStatus =
    | Standby
    | Running
    | Failed of FSharpDiagnostic[]
    | Succeeded of string * FSharpDiagnostic[]

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
    
    let project = "/tmp/input.fsproj"
    let inFile = "/tmp/input.fs"
    let outFile = "/tmp/output.dll"

    let mkOptions (checker: FSharpChecker) outFile =
        checker.GetProjectOptionsFromCommandLineArgs(project, [|
            "--simpleresolution"
            "--optimize-"
            "--noframework"
            "--fullpaths"
            "--warn:3"
            "--target:dll"
            inFile
            // Necessary standard library
            "-r:/tmp/FSharp.Core.dll"
            "-r:/tmp/mscorlib.dll"
            "-r:/tmp/netstandard.dll"
            "-r:/tmp/System.dll"
            "-r:/tmp/System.Core.dll"
            "-r:/tmp/System.IO.dll"
            "-r:/tmp/System.Numerics.dll"
            "-r:/tmp/System.Runtime.dll"
            "-o:" + outFile
        |])

    let create source = async {
        let checker = FSharpChecker.Create(keepAssemblyContents = true, suggestNamesForErrors = true)
        let options = mkOptions checker outFile
        File.WriteAllText(inFile, source)

        // start a check on creation so that we're ready to go when we finish initializing
        let! checkProjRes = checker.ParseAndCheckProject(options)
        let! (parseRes, checkRes) = checker.GetBackgroundCheckResultsForFileInProject(inFile, options)
        let! (diagnostics, code, assembly) = checker.CompileToDynamicAssembly([parseRes.ParseTree], "output", 
                                                [
                                                    "FSharp.Core";"mscorlib";"netstandard"
                                                    "System";"System.Core";"System.IO";"System.Runtime"
                                                ],
                                                None, noframework = true)
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
    let resultExpectedTypeMany = "System.Collections.Generic.IEnumerable<" + resultExpectedTypeOne + ">"

    let findResultMember (checkRes: FSharpCheckProjectResults) =
        match checkRes.AssemblySignature.FindEntityByPath ["System"] with
        | None -> None
        | Some ent ->
            ent.MembersFunctionsAndValues
            |> Seq.filter (fun v -> 
                v.IsValue &&
                v.LogicalName = "result" &&
                    let typen = v.FullType.Format(FSharpDisplayContext.Empty) 
                    typen = resultExpectedTypeOne || typen = resultExpectedTypeMany
            ) |> Seq.tryExactlyOne

    let checkDelay = Delayer(500) // the delayer to use for checking user input