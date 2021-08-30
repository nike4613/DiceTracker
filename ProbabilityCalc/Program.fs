
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
                (modified >. !>9))
    } |> funcnb "rollsingle" [skill]

let roll skill count =
    prob {
        return! seq { for _ in 1..count -> Arg 0 }
        |> Seq.map (rollsingle >> toProb)
        |> Seq.reduce (+)
    } |> funcn "roll" [skill]

let rolldiff count skill diff = 
    prob {
        return! ((roll (Arg 0) count) >. (Arg 1))
    } |> funcnb "rolldiff" [skill; diff]

let calcProb =
    seq {
        yield d 12 |> toProb |> outputName "1d12"
        yield (d 12 |> toProb) + !>3 |> outputName "1d12+3"
        yield (d 12 |> toProb) =. !>12 |> toProb |> outputName "1d12=12"
        yield rollsingle !>3 |> toProb |> outputName "rollsingle 3"
        (*for attr in 1..8 do
            for skill in 0..5 do
                yield rolldiff attr !>skill !>2 |> toProb 
                |> outputName (sprintf "attr %o skill %o" attr skill)*)
    }

[<EntryPoint>]
let main argv =
    printfn ""
    let results = calcProb |> Processing.processMany
    results.SaveToString() |> printfn "%s"
    0 // return an integer exit code