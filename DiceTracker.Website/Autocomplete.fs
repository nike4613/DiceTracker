namespace DiceTracker.Website

open System
open System.Threading.Tasks
open FSharp.Compiler.EditorServices
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.Text
open Microsoft.JSInterop

type Completion =
    {
        caption: string
        value: string
        meta: string
        index: int
        cache: DotNetObjectReference<CompletionCache>
    }
and CompletionCache(items: DeclarationListItem[], js: IJSInProcessRuntime) =

    let tooltips = Array.create items.Length None

    // Somehow `item.DescriptionTextAsync |> Async.RunSynchronously` or `Async.StartAsTask` freezes,
    // even though `item.DescriptionTextAsync |> Async.Start` runs nicely.
    // So we work around it by saving tooltips from async runs and calling `updateTooltip()` from `Async.Start`.
    [<JSInvokable>]
    member this.GetTooltip(index: int) =
        match tooltips.[index] with
        | None ->
            async {
                let (ToolTipText ttitems) = items.[index].Description
                let tt = String.concat "\n" <| seq {
                    for ttitem in ttitems do
                        match ttitem with
                        | ToolTipElement.CompositionError e -> yield e
                        | ToolTipElement.None -> ()
                        | ToolTipElement.Group ttelts ->
                            for ttelt in ttelts do
                                yield! ttelt.MainDescription |> Array.map (fun t -> t.Text)
                }
                tooltips.[index] <- Some tt
                js.Invoke("DiceTracker.updateTooltip")
            }
            |> Async.Start
            "Loading..."
        | Some tt -> tt

type Autocompleter(dispatch: int * int * string * (DeclarationListItem[] -> IDisposable) -> unit, js) =
    [<JSInvokable>]
    member this.Complete(line, col, lineText) =
        printfn "completing for %i: '%s' at %i" line lineText col
        let tcs = TaskCompletionSource<Completion[]>()
        dispatch (line, col, lineText, fun items ->
            let cache = DotNetObjectReference.Create(CompletionCache(items, js))
            items
            |> Array.mapi (fun i item ->
                {
                    caption = item.Name
                    value = item.NameInCode
                    meta = string item.Glyph
                    index = i
                    cache = cache
                })
            |> tcs.SetResult
            cache :> IDisposable)
        tcs.Task