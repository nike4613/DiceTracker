module DiceTracker.Website.Main

open Elmish
open Bolero
open Bolero.Html
open Microsoft.JSInterop
open XPlot.Plotly


type Model =
    {
        text: string
        plotScript: string option
    }

type Message =
    | UpdateText of string
    | PlotChart of PlotlyChart
    | Evaluate
    
let initModel =
    {
        text = "// TODO: examples"
        plotScript = None
    }, Cmd.none

let update (js: IJSRuntime) message model =
    match message with
    | UpdateText t ->  { model with text = t }, Cmd.none
    | PlotChart c -> 
        let chart = c |> Chart.WithId "results"
        { model with plotScript = Some (chart.GetPlottingJS()) }, Cmd.none
    | Evaluate -> model, Cmd.none

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
        div [attr.id "results"] []
        cond model.plotScript <| function
            | Some s -> script [] [ text s ]
            | None -> empty
    ]

type Application() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        let update = update this.JSRuntime
        Program.mkProgram (fun _ -> initModel) update view