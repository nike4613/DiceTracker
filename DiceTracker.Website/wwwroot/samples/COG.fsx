
module Dice

open DiceTracker

[<Literal>]
let dicePool = 12
[<Literal>]
let target = 9

let rollsingle skill =
    prob {
        let! roll = d dicePool
        let! modified = roll + (Arg 0)
        return! condb (roll =. !>dicePool)
            (Literal true)
            (condb (roll =. !>1)
                (Literal false)
                (modified >. !>target))
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
                seq { for _ in 1..(diff - count) -> d dicePool =. !>dicePool }
                |> Seq.map toProb |> Seq.reduce (+)
            else !>0
        return! ((roll (Arg 0) count) + pityDice >=. !>diff)
    } |> funcnb "rolldiff" [skill]

let result =
    seq {
        for attr in 1..8 do
            for skill in 0..5 do
                for diff in 1..4 do
                    yield rolldiff attr !>skill diff |> toProb 
                    |> outputName $"diff {diff} attr {attr} skill {skill}"
    }
