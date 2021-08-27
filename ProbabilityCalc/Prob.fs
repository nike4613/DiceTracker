
module Prob

type ProbabilityValue =
    | DieValue of Die
    //| DicePool of DicePool
    | Number of int
    //| Sequence of ProbabiltyValue seq
    | Sum of ProbabilityValue * ProbabilityValue
    | Difference of ProbabilityValue * ProbabilityValue
    | Multiply of ProbabilityValue * ProbabilityValue
    | Divide of ProbabilityValue * ProbabilityValue
    | Condition of BooleanValue * ProbabilityValue * ProbabilityValue
    //| ValueMatch of ProbabiltyValue * (int * ProbabiltyValue) list
    static member inline (+) (a, b) = Sum (a, b)
    static member inline (-) (a, b) = Difference (a, b)
    static member inline (*) (a, b) = Multiply (a, b)
    static member inline (/) (a, b) = Divide(a, b)
    
    static member inline (=.) (a, b) = Equals(a, b)
    static member inline (<>.) (a, b) = NotEquals(a, b)
    static member inline (>.) (a, b) = GreaterThan(a, b)
    static member inline (<.) (a, b) = LessThan(a, b)
    static member inline (>=.) (a, b) = GreaterThanEqual(a, b)
    static member inline (<=.) (a, b) = LessThanEqual(a, b)
    
    static member inline (&&.) (a, b) = BoolAnd(a, b)
    static member inline (||.) (a, b) = BoolOr(a, b)

and BooleanValue =
    | Literal of bool
    | BoolCondition of BooleanValue * BooleanValue * BooleanValue
    | Equals of ProbabilityValue * int
    | NotEquals of ProbabilityValue * int
    | GreaterThan of ProbabilityValue * int
    | LessThan of ProbabilityValue * int
    | GreaterThanEqual of ProbabilityValue * int
    | LessThanEqual of ProbabilityValue * int
    | BoolNot of BooleanValue
    | BoolAnd of BooleanValue * BooleanValue
    | BoolOr of BooleanValue * BooleanValue
    static member inline (!) bval =
        match bval with
        | Equals(v, i) -> NotEquals(v, i)
        | NotEquals(v, i) -> Equals(v, i)
        | _ -> BoolNot bval
    static member inline (!.) bval = Condition(bval, Number 1, Number 0)
    
and Die(size: int) =
    member _.Size = size
    static member inline (!.) d = DieValue d
    //static member inline (*) (count, d) = { dice = d ; count = count }

    // All of this crap is so that I can use Die in a Map
    static member private objHash (o: obj) = System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(o)
    member private self.compareTo other = (Die.objHash self).CompareTo(Die.objHash other)
    override self.Equals(o) = obj.ReferenceEquals(self, o)
    override self.GetHashCode() = Die.objHash self

    interface System.IComparable<Die> with
        member self.CompareTo(other) = self.compareTo other
    interface System.IComparable with
        member self.CompareTo(other) =
            match other with
            | :? Die as d -> self.compareTo d
            | _ -> 1

(*and DicePool =
    { dice : Die ; count : int }
    static member inline (!.) d = DicePool d*)

let inline toProb value : ProbabilityValue = !. value

let inline (!>) (value: int) = Number value
//let inline (!->) seq = Sequence seq
//let inline (!.>) seq = !-> (seq |> Seq.map (fun x -> !. x))
//let inline (!>>) seq = !-> (seq |> Seq.map (fun x -> !> x))
let inline d size = new Die(size)
let inline pool size count = count * (d size)

let inline cond c t f = Condition(c, t, f)
let inline condb c t f = BoolCondition(c, t, f)
//let inline pmatch v cnds = ValueMatch(v, cnds)

(*
let inline map f sv =
    match sv with
    | Sequence s -> Seq.map f s |> (!->)
    | _ -> raise(exn("Cannot apply map to a non-sequence"))
    *)

type OutputValue =
    | NamedOutput of string * ProbabilityValue
    | UnnamedOutput of ProbabilityValue

let output value = UnnamedOutput value
let outputName name value = NamedOutput(name, value)