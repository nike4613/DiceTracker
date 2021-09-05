namespace DiceTracker.Website

open System
open System.IO
open System.Threading
open System.Threading.Tasks
open Microsoft.JSInterop
open System.Runtime.CompilerServices
open System.Net.Http
open System.Diagnostics

type StringCallback(f: string -> unit) =
    [<JSInvokable>]
    member _.Invoke(arg) = f arg

module Callback =
    let ofFn f = DotNetObjectReference.Create(StringCallback f)

module Action =
    let ofFn f = Action(f)
    let ofValFn f = Action<_>(f)

module Async =
    let WithYield (a: Async<'T>) : Async<'T> =
        async.Bind(Async.Sleep(10), fun _ -> a)

module Utils =
    let getErrorMessage (exn: exn) =
        sprintf "%s\nil offsets: %A" (exn.ToString()) ((StackTrace exn).GetFrames() |> Array.map (fun e -> e.GetILOffset()) |> Array.toList)

[<Extension>]
type HttpExtensions =

    /// Send a GET request to the specified URI as an asynchronous operation.
    [<Extension>]
    static member AsyncGet(this: HttpClient, uri: string, ?completionOption: HttpCompletionOption, ?cancellationToken: CancellationToken) : Async<HttpResponseMessage> =
        match completionOption, cancellationToken with
        | None, None -> this.GetAsync(uri)
        | Some co, None -> this.GetAsync(uri, co)
        | None, Some ct -> this.GetAsync(uri, ct)
        | Some co, Some ct -> this.GetAsync(uri, co, ct)
        |> Async.AwaitTask

type Delayer(durationInMs: int) =

    let mutable current: option<CancellationTokenSource> = None

    member this.Trigger(task) =
        current |> Option.iter (fun tok -> tok.Cancel())
        let cts = new CancellationTokenSource()
        Async.StartImmediate(async {
            do! Async.Sleep durationInMs
            do! task
            current <- None
        }, cts.Token)
        current <- Some cts