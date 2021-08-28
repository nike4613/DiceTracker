
open System
open Prob

let rollsingle skill =
    prob {
        let! roll = d 12 |> toProb
        let! modified = roll + (Arg 0)
        return! condb (roll =. !>12)
            (Literal true)
            (condb (roll =. !>1)
                (Literal false)
                (modified >. !>7)) |> toProb
    } |> func [skill]

let roll skill count =
    prob {
        return! seq { for _ in 1..count -> Arg 0 }
        |> Seq.map (rollsingle)
        |> Seq.reduce (+)
    } |> func [skill]

let rolldiff count skill diff = 
    prob {
        return! ((roll (Arg 0) count) >. (Arg 1))
    } |> funcb [skill; diff]

let calcProb =
    seq {
        for attr in 1..8 do
            for skill in 0..5 do
                rolldiff attr !>skill !>3 |> toProb 
                |> outputName (sprintf "attr %o skill %o" attr skill)
    }

[<EntryPoint>]
let main argv =
    printfn ""
    let results = calcProb |> Processing.processMany
    results.SaveToString() |> printfn "%s"
    0 // return an integer exit code