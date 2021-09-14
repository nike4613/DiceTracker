
module Evaluation

open XPlot.Plotly
open DiceTracker
open System.Reflection
    
let stackedLayout = Layout(barmode = "stack")

let evaluate (expr: MemberInfo) =
    async {
        printfn "switching to thread pool"
        let! _ = Async.SwitchToThreadPool ()

        printfn "evaluating %A" expr
        
        let value =
            match expr with
            | :? FieldInfo as f -> f.GetValue(null)
            | :? PropertyInfo as p -> p.GetGetMethod().Invoke(null, [| |])
            | _ -> raise (exn $"Unknown member type {expr.GetType()}")

        printfn "processing"
        let results =
            match value with
            | :? OutputValue as single -> Processing.processOne single
            | :? seq<OutputValue> as multi -> Processing.processMany multi
            | _ -> raise (exn $"Unknown value type {value.GetType()}")

        printfn "got results"
        let bars = 
            results
            |> Map.toSeq
            |> Seq.map (fun (a, b) -> b |> Map.toSeq |> Seq.map (fun (b, c) -> a, b, c))
            |> Seq.concat
            |> Seq.fold (fun m (n, i, v) -> Map.change i (fun v2 -> (n,v)::(Option.defaultValue [] v2) |> Some) m) Map.empty
            |> Map.map (fun _ v -> List.rev v)
            |> Map.toSeq
            |> Seq.map (fun (i, list) ->
                Bar(
                    x = (list |> List.map (fun (n, _) -> n)),
                    y = (list |> List.map (fun (_, r) -> float r)),
                    name = string i
                ))
            |> Seq.toList

        let chart =
            bars
            |> Chart.Plot
            |> Chart.WithLayout stackedLayout

        return chart
    }

