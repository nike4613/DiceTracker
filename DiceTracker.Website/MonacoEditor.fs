
module DiceTracker.Website.Monaco

open Elmish
open Bolero
open Bolero.Html
open Microsoft.JSInterop
open Microsoft.AspNetCore.Components

type Model =
    {
        editor: IJSObjectReference option
        value: string
    }
    
let getEditorValue (editor: IJSObjectReference) = editor.InvokeAsync<string>("getValue", []).AsTask() |> Async.AwaitTask
let setEditorValue (editor: IJSObjectReference) (value: string) = editor.InvokeVoidAsync("setValue", [value]).AsTask() |> Async.AwaitTask
    
type Message =
    | CreateEditor of Editor
    | EditorCreated of IJSObjectReference

and Editor() =
    inherit ElmishComponent<Model, Message>()
    
    let elemRef = HtmlRef()

    [<Inject>]
    member val JSRuntime = Unchecked.defaultof<IJSRuntime> with get, set

    override _.ShouldRender(oldModel, newModel) =
        oldModel.value <> newModel.value || Option.isNone newModel.editor

    override this.View model dispatch =
        match model.editor with
        | Some editor -> setEditorValue editor model.value |> ignore
        | None -> CreateEditor this |> dispatch

        div [ attr.ref elemRef ] []
        
let update message model =
    match message with
    | CreateEditor editor ->
        let js = editor.JSRuntime
        let cmd =
            Cmd.OfTask.perform (fun args -> js.InvokeAsync("monacoCreate", args).AsTask())
                [| s; "// this is the default text"; "fsharp" |] EditorCreated
        model, cmd
    | EditorCreated ref -> { model with editor = Some ref }, Cmd.none