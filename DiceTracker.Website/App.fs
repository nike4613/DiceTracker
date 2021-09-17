
module DiceTracker.Website.App

open Bolero
open Microsoft.AspNetCore.Components
open Microsoft.JSInterop
open Elmish
open Bolero.Html
open Bolero.Remoting.Client
open Bolero.Templating.Client
open System.Diagnostics
open System.Net.Http
open System.IO

type AppModel =
    | Initializing of string
    | Running of Main.Model
    | InitFailed of exn

type AppMessage =
    | StartInit
    | GotAssetList of string[]
    | InitCompiler
    | CompilerInitialized of Compiler
    | InitEditor of string option
    | Message of Main.Message
    | Error of exn

let downloadAssets (http: HttpClient) assets = async {
    Directory.CreateDirectory Compiler.libPath |> ignore
    let downloadFile uri = async {
        let! response = http.AsyncGet uri
        let filen = Path.GetFileName uri
        use file = File.OpenWrite <| sprintf "%s%s" Compiler.libPath filen
        do! response.Content.CopyToAsync file |> Async.AwaitTask
        return ()
    }
    let sw = Stopwatch.StartNew()
    do! assets |> Seq.map downloadFile |> Async.Parallel |> Async.Ignore
    sw.Stop()
    printfn "Finished downloading assets in %A" sw.Elapsed
    return ()
}

let sourceDuringLoad snippetId =
    if Option.isSome snippetId then "" else Main.defaultSource

let update (js: IJSInProcessRuntime) (http: HttpClient) message model =
    match message with
    | StartInit -> 
        Trace.Listeners.Add (new ConsoleTraceListener()) |> ignore
        Initializing "Downloading libraries...", Cmd.OfJS.either js "eval" [|"MONO.loaded_files"|] GotAssetList Error
    | GotAssetList assets -> model, Cmd.OfAsync.either (downloadAssets http) assets (fun () -> InitCompiler) Error
    | InitCompiler ->
        Initializing "Initializing compiler...",
        Cmd.OfAsync.either (Async.WithYield << Compiler.create) Main.defaultSource CompilerInitialized Error
    | CompilerInitialized compiler ->
        let snippetId = js.GetQueryParam "snippet"
        let initSrc = sourceDuringLoad snippetId
        let initSnippetId = defaultArg snippetId Main.defaultSnippetId
        let model = Main.initModel compiler initSrc initSnippetId
        Running model, Cmd.ofMsg (InitEditor snippetId)
    | InitEditor snippetId ->
        model,
        Cmd.ofSub (fun dispatch ->
            let onEdit = Main.UpdateText >> Message >> dispatch
            js.InvokeVoid("DiceTracker.initAce", "editor",
                sourceDuringLoad snippetId,
                Callback.ofStr onEdit,
                null (*autocompleter*))
            let onSetSnip = Option.ofObj >> Option.defaultValue Main.defaultSnippetId >> Main.LoadSnippet >> Message >> dispatch
            Option.iter onSetSnip snippetId
            js.ListenToQueryParam("snippet", onSetSnip)
        )
    | Message msg ->
        match model with
        | Initializing _ -> model, Cmd.none
        | InitFailed _ -> model, Cmd.none
        | Running model ->
            let model, cmd = Main.update js http msg model
            Running model, Cmd.map Message cmd
    | Error exn ->
        eprintfn "%s" (Utils.getErrorMessage exn)
        InitFailed exn, Cmd.none

type Loader = Template<"loading.html">

let view model dispatch = 
    cond model <| function
        | Initializing s -> Loader.Loader().Text(s).Elt()
        | Running model -> Main.view model (Message >> dispatch)
        | InitFailed exn -> Loader.LoadError().Error(Utils.getErrorMessage exn).Elt()

type Application() =
    inherit ProgramComponent<AppModel, AppMessage>()

    [<Inject>]
    member val Http = Unchecked.defaultof<HttpClient> with get, set

    override this.Program =
        let update = update (this.JSRuntime :?> _) this.Http
        Program.mkProgram (fun _ -> Initializing "Initializing...", Cmd.ofMsg StartInit) update view
        |> Program.withConsoleTrace
        |> Program.withErrorHandler (fun (msg, exn) -> printfn "%s: %A" msg exn)
#if DEBUG
        |> Program.withHotReload
#endif