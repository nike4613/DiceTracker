
module Dice

open DiceTracker
open XPlot.Plotly

let rollsingle skill =
    prob {
        let! roll = d 12
        let! modified = roll + (Arg 0)
        return! condb (roll =. !>12)
            (Literal true)
            (condb (roll =. !>1)
                (Literal false)
                (modified >. !>9))
    } |> funcnb "rollsingle" [skill]

let roll skill count =
    prob {
        return! seq { for _ in 1..count -> rollsingle (Arg 0) }
        |> Seq.map toProb |> Seq.reduce (+)
    } |> funcn "roll" [skill]

let rolldiff count skill diff = 
    prob {
        let pityDice =
            if diff > count then
                seq { for _ in 1..(diff - count) -> d 12 =. !>12 }
                |> Seq.map toProb |> Seq.reduce (+)
            else !>0
        return! ((roll (Arg 0) count) + pityDice >=. !>diff)
    } |> funcnb "rolldiff" [skill]

let result =
    seq {
        for attr in 1..8 do
            for skill in 0..5 do
                yield rolldiff attr !>skill 2 |> toProb 
                |> outputName $"attr {attr} skill {skill}"
    }

let rec printMapFull map = Map.iter (printfn "%s %A") map

let stackedLayout = Layout(barmode = "stack")

[<EntryPoint>]
let main argv =
    let results = result |> Processing.processMany
    printMapFull results

    let bars = 
        results
        |> Map.toSeq
        |> Seq.map (fun (a, b) -> b |> Map.toSeq |> Seq.map (fun (b, c) -> a, b, c))
        |> Seq.concat
        |> Seq.fold (fun m (n, i, v) -> Map.change i (fun v2 -> (n,v)::(Option.defaultValue [] v2) |> Some) m) Map.empty
        |> Map.map (fun _ v -> List.rev v)

    let traces =
        bars
        |> Map.toSeq
        |> Seq.map (fun (i, list) ->
            Bar(
                x = (list |> List.map (fun (n, _) -> n)),
                y = (list |> List.map (fun (_, r) -> float r)),
                name = string i
            ))
        |> Seq.toList

    let chart =
        traces
        |> Chart.Plot
        |> Chart.WithLayout stackedLayout

    chart.Show()

    0 // return an integer exit code