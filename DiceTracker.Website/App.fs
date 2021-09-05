
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
    | Message of Main.Message
    | Error of exn

let downloadAssets (http: HttpClient) assets = async {
    Directory.CreateDirectory Compiler.libPath |> ignore
    let downloadFile uri = async {
        let! response = http.AsyncGet uri
        let filen = Path.GetFileName uri
        use file = File.OpenWrite <| sprintf "%s%s" Compiler.libPath filen
        let! _ = response.Content.CopyToAsync file |> Async.AwaitTask
        return ()
    }
    let sw = Stopwatch.StartNew()
    let! _ = assets |> Seq.map downloadFile |> Async.Parallel |> Async.Ignore
    sw.Stop()
    printfn "Finished downloading assets in %A" sw.Elapsed
    return ()
}

let update (js: IJSRuntime) (http: HttpClient) message model =
    match message with
    | StartInit -> Initializing "Downloading libraries...", Cmd.OfJS.either js "eval" [|"MONO.loaded_files"|] GotAssetList Error
    | GotAssetList assets -> model, Cmd.OfAsync.either (downloadAssets http) assets (fun () -> InitCompiler) Error
    | InitCompiler ->
        Initializing "Initializing compiler...",
        Cmd.OfAsync.either (Async.WithYield << Compiler.create) Main.defaultSource CompilerInitialized Error
    | CompilerInitialized compiler ->
        Running (Main.initModel compiler Main.defaultSource), Cmd.none
    | Message msg ->
        match model with
        | Initializing _ -> model, Cmd.none
        | InitFailed _ -> model, Cmd.none
        | Running model ->
            let model, cmd = Main.update js msg model
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
        let update = update this.JSRuntime this.Http
        Program.mkProgram (fun _ -> Initializing "Initializing...", Cmd.ofMsg StartInit) update view
        //|> Program.withConsoleTrace
        |> Program.withErrorHandler (fun (msg, exn) -> printfn "%s: %A" msg exn)
#if DEBUG
        |> Program.withHotReload
#endif