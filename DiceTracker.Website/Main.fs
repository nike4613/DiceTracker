module DiceTracker.Website.Main

open Elmish
open Bolero
open Bolero.Html
open Bolero.Remoting.Client
open Microsoft.JSInterop
open XPlot.Plotly
open FSharp.Compiler.Diagnostics
open System.Net
open System.Net.Http
open FSharp.Compiler.EditorServices
open System

type Page =
    | MessagesPage
    | ResultsPage

type Model =
    {
        compiler: Compiler
        source: string
        plotScript: (string * string) option
        compilerMessages: FSharpDiagnostic seq option
        warnings: string list option
        errors: string list option
        currentPage: Page
        enablePageSwitching: bool
        evaluating: bool
        snippetId: string
        lastCompleter: IDisposable option
    }

type Message =
    | UpdateText of string
    | PlotChart of PlotlyChart
    | Compile
    | Compiled of Compiler
    | Evaluate of System.Reflection.MemberInfo
    | EvalJs of string
    | EvalFinished of unit
    | Checked of Compiler * FSharpDiagnostic[]
    | Complete of line:int * col:int * lineText:string * callback:(DeclarationListItem[] -> IDisposable)
    | CompletionSent of IDisposable
    | Error of exn
    | SelectPage of Page
    | LoadSnippet of id:string
    | SnippetLoaded of string
    | SelectMessage of FSharpDiagnostic
    
let defaultSource = "module Dice\nlet result = ()"

let defaultSnippetId = "COG"

let snippets = [
    "COG", "COG Rolling System"
]

let initModel compiler source snippet =
    {
        compiler = compiler
        source = source
        plotScript = None
        compilerMessages = None
        warnings = None
        errors = None
        currentPage = MessagesPage
        enablePageSwitching = false
        evaluating = false
        snippetId = ""
        lastCompleter = None
    }

let update (js: IJSInProcessRuntime) (http: HttpClient) message model =
    match message with
    | SelectPage p -> { model with currentPage = p }, Cmd.none
    | UpdateText t ->
        { model with source = t },
        Cmd.ofSub <| fun dispatch ->
            model.compiler.TriggerCheck(t, Checked >> dispatch)
    | PlotChart c -> 
        { model with
            plotScript = Some (c.Id, c.GetPlottingJS())
            evaluating = false
            enablePageSwitching = true
            currentPage = ResultsPage }, Cmd.none
    | Compile ->
        let compiler, run = model.compiler.Run model.source
        { model with
            compiler = compiler
            currentPage = MessagesPage 
            enablePageSwitching = false },
        Cmd.OfAsync.either (run >> Async.WithYield) () Compiled Error
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
    | Checked(compiler, errors) ->
        { model with
            compiler = compiler
            compilerMessages = Some (errors |> Array.toSeq) },
        Cmd.OfFunc.attempt (Ace.SetAnnotations js) errors Error
    | Evaluate memb -> { model with evaluating = true }, Cmd.OfAsync.either Evaluation.evaluate memb PlotChart Error
    | EvalJs script -> model, Cmd.OfJS.either js "eval" [| script |] EvalFinished Error
    | EvalFinished() -> model, Cmd.none
    | LoadSnippet id ->
        { model with snippetId = id },
        Cmd.OfTask.either (fun (s: string) -> http.GetStringAsync s) (sprintf "samples/%s.fsx" id) SnippetLoaded Error
    | SnippetLoaded text ->
        { model with source = text },
        Cmd.OfFunc.attempt (fun () ->
            js.InvokeVoid("DiceTracker.setText", text)
            js.InvokeVoid("DiceTracker.setQueryParam", "snippet", model.snippetId)
        ) () Error
    | SelectMessage msg ->
        model, Cmd.OfFunc.attempt (Ace.SelectMessage js) msg Error
    | Complete(line, col, lineText, callback) ->
        model.lastCompleter |> Option.iter (fun d -> d.Dispose())
        { model with lastCompleter = None },
        Cmd.OfFunc.either (fun (line, col, lineText) ->
            model.compiler.Autocomplete line col lineText |> callback
        ) (line, col, lineText) CompletionSent Error
    | CompletionSent completer ->
        { model with lastCompleter = Some completer }, Cmd.none
    | Error exn ->
        eprintfn "%s" (Utils.getErrorMessage exn)
        { model with
            errors = Some [ Utils.getErrorMessage exn ] }, Cmd.none

type Main = Template<"main.html">

let compilerMsg dispatch (msg: FSharpDiagnostic) =
    Main.CompilerMessage()
        .Severity(string msg.Severity)
        .StartLine(string msg.StartLine)
        .StartColumn(string msg.StartColumn)
        .EndLine(string msg.EndLine)
        .EndColumn(string msg.EndColumn)
        .Message(msg.Message)
        .Select(fun _ -> SelectMessage msg |> dispatch)
        .Elt()

let simpleMsg (severity: string) (msg: string) =
    Main.SimpleMessage()
        .Severity(severity)
        .Message(msg)
        .Elt()

let snippetOption (id: string, label: string) =
    option [attr.value id] [text label]

let view (model: Model) dispatch =
    Main()
        .PlotlyUrl(Html.DefaultPlotlySrc)
        //.EditorText(model.source, Action.ofValFn (UpdateText >> dispatch))
        .Evaluate(fun _ -> dispatch Compile)
        .SelectedSnippet(model.snippetId, Action.ofValFn (LoadSnippet >> dispatch))
        .Snippets(forEach snippets snippetOption)
        .PageSelection(
            if model.enablePageSwitching then
                concat [
                    button [
                        on.click (fun _ -> SelectPage MessagesPage |> dispatch)
                        attr.disabled (model.currentPage = MessagesPage)
                    ] [ text "Messages" ]
                    button [
                        on.click (fun _ -> SelectPage ResultsPage |> dispatch)
                        attr.disabled (model.currentPage = ResultsPage)
                    ] [ text "Results" ]
                ]
            else
                empty
        )
        .Status(
            match model.compiler.status, model.evaluating with
            | CompilerStatus.Standby, false -> "Ready."
            | CompilerStatus.Running, false -> "Compiling..."
            | CompilerStatus.Succeeded _, false -> "Compilation finished."
            | CompilerStatus.Failed _, false -> "Compilation failed."
            | _, true -> "Evaluating..."
            |> text
        )
        .CurrentPage(
            match model.currentPage with
            | MessagesPage ->
                Main.Messages()
                    .Messages(concat [
                        cond model.errors <| function
                            | None -> empty
                            | Some msgs -> forEach msgs (simpleMsg "Error")
                        cond model.warnings <| function
                            | None -> empty
                            | Some msgs -> forEach msgs (simpleMsg "Warning")
                        cond model.compilerMessages <| function
                            | None -> empty
                            | Some msgs -> forEach (msgs |> Seq.sortBy (fun m -> m.Severity)) (compilerMsg dispatch)
                    ])
                    .Elt()
            | ResultsPage ->
                match model.plotScript with
                | Some(id, src) ->
                    concat [
                        div [attr.id id; attr.classes ["chart"]] []
                        script [attr.defer true] [text src]
                    ]
                | None -> span [] [ text "No results are available" ]
        )
        .Elt()
