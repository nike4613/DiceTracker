
module Prob

type ProbabiltyValue =
    | Die of Die
    | DicePool of DicePool
    | Number of int
    | Sequence of ProbabiltyValue seq
    | Sum of ProbabiltyValue * ProbabiltyValue
    | Difference of ProbabiltyValue * ProbabiltyValue
    | Multiply of ProbabiltyValue * ProbabiltyValue
    | Divide of ProbabiltyValue * ProbabiltyValue
    | Condition of BooleanValue * ProbabiltyValue * ProbabiltyValue
    | ValueMatch of ProbabiltyValue * (int * ProbabiltyValue) list
    static member inline (+) (a, b) = Sum (a, b)
    static member inline (-) (a, b) = Difference (a, b)
    static member inline (*) (a, b) = Multiply (a, b)
    static member inline (/) (a, b) = Divide(a, b)
    
    static member inline (==) (a, b) = Equals(a, b)
    static member inline (!=) (a, b) = NotEquals(a, b)
    static member inline (>) (a, b) = GreaterThan(a, b)
    static member inline (<) (a, b) = LessThan(a, b)
    static member inline (>=) (a, b) = GreaterThanEqual(a, b)
    static member inline (<=) (a, b) = LessThanEqual(a, b)
    
    static member inline (&&) (a, b) = BoolAnd(a, b)
    static member inline (||) (a, b) = BoolOr(a, b)

and BooleanValue =
    | Equals of ProbabiltyValue * int
    | NotEquals of ProbabiltyValue * int
    | GreaterThan of ProbabiltyValue * int
    | LessThan of ProbabiltyValue * int
    | GreaterThanEqual of ProbabiltyValue * int
    | LessThanEqual of ProbabiltyValue * int
    | BoolNot of BooleanValue
    | BoolAnd of BooleanValue * BooleanValue
    | BoolOr of BooleanValue * BooleanValue
    static member inline (!) bval =
        match bval with
        | Equals(v, i) -> NotEquals(v, i)
        | NotEquals(v, i) -> Equals(v, i)
        | _ -> BoolNot bval
    
and Die =
    { size : int }
    static member inline (!.) d = Die d
    static member inline (*) (count, d) = { dice = d ; count = count }

and DicePool =
    { dice : Die ; count : int }
    static member inline (!.) d = DicePool d

let inline (!>) (value: int) = Number value
let inline (!->) seq = Sequence seq
let inline (!.>) seq = !-> (seq |> Seq.map (fun x -> !. x))
let inline (!>>) seq = !-> (seq |> Seq.map (fun x -> !> x))
let inline d size = { size = size }
let inline pool size count = count * (d size)

let inline cond c t f = Condition(c, t, f)
let inline pmatch v cnds = ValueMatch(v, cnds)

let inline map f sv =
    match sv with
    | Sequence s -> Seq.map f s |> (!->)
    | _ -> raise(exn("Cannot apply map to a non-sequence"))

let output (name: string) (value: ProbabiltyValue) = {| name = name ; value = value |}