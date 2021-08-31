module DiceTracker.Website.Main

open Elmish
open Bolero
open Bolero.Html
open Microsoft.JSInterop

type Model =
    {
        monaco: Monaco.Model
    }

type Message =
    | Ping
    | MonacoMessage of Monaco.Message
    
let initModel =
    {
        monaco = { editor = None ; value = "" }
    }, Cmd.none

let update message model =
    match message with
    | Ping -> model, Cmd.none
    | MonacoMessage msg -> 
        let (mdl, cmd) = Monaco.update msg model.monaco
        { model with monaco = mdl }, Cmd.map MonacoMessage cmd

let view model dispatch =
    div [attr.``class`` "main"] [
        ecomp<Monaco.Editor,_,_> [] model.monaco (MonacoMessage >> dispatch)
        div [attr.id "controls"] []
        div [attr.id "results"] []
    ]

type Application() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        Program.mkProgram (fun _ -> initModel) update view