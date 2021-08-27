
open System
open Prob

let rollsingle skill = 
    let roll = d 12 |> toProb
    let modified = roll + skill
    condb (roll == 12)
        (Literal true)
        (condb (roll == 1)
            (Literal false)
            (modified >. 7))

let roll skill count =
    seq { for _ in 1..count -> skill }
    |> Seq.map (rollsingle >> toProb)
    |> Seq.reduce (+)

let rolldiff count skill diff = 
    roll skill count >. diff

let calcProb =
    seq {
        for attr in 1..8 do
            for skill in 0..5 do
                rolldiff attr !>skill 3 |> toProb 
                |> outputName (sprintf "attr %o skill %o" attr skill)
    }

[<EntryPoint>]
let main argv =
    !. (pool 12 4) |> ignore
    calcProb |> Seq.iter (printfn "%O")
    0 // return an integer exit code