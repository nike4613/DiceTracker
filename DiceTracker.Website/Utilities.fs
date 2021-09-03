namespace DiceTracker.Website

open System
open System.IO
open System.Threading
open System.Threading.Tasks
open Microsoft.JSInterop

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