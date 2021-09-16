
module DiceTracker.Website.Monaco

open Elmish
open Bolero
open Bolero.Html
open Microsoft.JSInterop
open Microsoft.AspNetCore.Components
open Bolero.Remoting.Client

type Model =
    { 
        editor: IJSObjectReference option
        value: string
        elemRef: HtmlRef
        delayer: Delayer
    }
module Model =
    let init = {
        editor = None
        value = ""
        elemRef = HtmlRef()
        delayer = Delayer(250)
    }
    
let getEditorValue (editor: IJSObjectReference) = editor.InvokeAsync<string>("getValue", []).AsTask() |> Async.AwaitTask
let setEditorValue (editor: IJSObjectReference) (value: string) = async {
    do! editor.InvokeVoidAsync("setValue", [value]).AsTask() |> Async.AwaitTask
    return value
}
    
type Message = 
    private
    | CreateEditor
    | EditorCreated of IJSObjectReference
    | EditorTextChanged of string
    | SetEditorText of string
    | Error of exn
module Message =
    let init = CreateEditor

let private view model _ =
    div [ attr.ref model.elemRef ] []

let private update (js: IJSRuntime) dispatch message model =
    match message with
    | CreateEditor -> model, Cmd.OfJS.perform js "editor.create" [| model.elemRef.Value; model.value; "fsharp" |] EditorCreated
    | EditorCreated ref -> 
        { model with editor = Some ref }, Cmd.OfJS.attempt js "editor.onChange"
            [|
                ref
                Callback.ofFn (fun editor -> 
                    model.delayer.Trigger(async {
                        let! text = getEditorValue editor
                        text |> EditorTextChanged |> dispatch
                    }))
            |] Error
    | SetEditorText s ->
        model, 
        match model.editor with
        | Some editor -> Cmd.OfAsync.either (setEditorValue editor) s EditorTextChanged Error
        | None -> Cmd.ofMsg (EditorTextChanged s)
    | EditorTextChanged s -> { model with value = s }, Cmd.none
    | Error e -> 
        eprintfn "%s" (Utils.getErrorMessage e)
        model, Cmd.none

type Editor() =
    inherit ElmishComponent<Model, Message>()

    [<Inject>]
    member val JS = Unchecked.defaultof<IJSRuntime> with get, set

    override _.View model dispatch = view model dispatch
    member this.Update message model = update this.JS this.Dispatch message model

let editor attrs model dispatch = ecomp<Editor,_,_> attrs model dispatch

let SetText s = SetEditorText s
