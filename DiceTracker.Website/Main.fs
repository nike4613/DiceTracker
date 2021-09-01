module DiceTracker.Website.Main

open Elmish
open Bolero
open Bolero.Html
open Bolero.Remoting.Client
open Microsoft.JSInterop
open XPlot.Plotly


type Model =
    {
        text: string
        plotScript: (string * string) option
        messages: string list option
    }

type Message =
    | UpdateText of string
    | PlotChart of PlotlyChart
    | Evaluate
    | EvalJs of string
    | EvalFinished of unit
    
let initModel =
    {
        text = "// TODO: examples"
        plotScript = None
        messages = None
    }, Cmd.none

let update (js: IJSRuntime) message model =
    match message with
    | UpdateText t ->  { model with text = t }, Cmd.none
    | PlotChart c -> 
        { model with plotScript = Some (c.Id, c.GetPlottingJS()) }, Cmd.none
    | Evaluate -> 
        match Evaluation.evaluate model.text with
        | Evaluation.Message m -> { model with messages = Some [m] }, Cmd.none
        | Evaluation.Exception ex -> { model with messages = Some [ ex.ToString() ] }, Cmd.none
        | Evaluation.Errors errs -> { model with messages = errs |> Seq.map (fun e -> $"[{e.StartLine}] {e.Message}") |> Seq.toList |> Some }, Cmd.none
        | Evaluation.Success chart -> model, Cmd.ofMsg (PlotChart chart)
    | EvalJs script -> model, Cmd.OfJS.perform js "eval" [| script |] EvalFinished
    | EvalFinished() -> model, Cmd.none

let view model dispatch =
    concat [
        script [ attr.src Html.DefaultPlotlySrc ] []
        textarea [
            attr.id "editor"
            bind.input.string model.text (UpdateText >> dispatch)
        ] []
        div [attr.id "controls"] [
            button [ on.click (fun _ -> dispatch Evaluate) ] [ text "Evaluate" ]
        ]
        cond model.plotScript <| function
            | Some(id, script) -> 
                EvalJs script |> dispatch
                concat [ div [attr.id id] [] ]
            | None -> empty
        cond model.messages <| function
            | Some list -> div [attr.id "errors"] [ forEach list (fun m -> div [] [text m]) ]
            | None -> empty
    ]

type Application() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        let update = update this.JSRuntime
        Program.mkProgram (fun _ -> initModel) update view
        |> Program.withConsoleTrace
        |> Program.withErrorHandler (fun (msg, exn) -> printfn "%s: %A" msg exn)