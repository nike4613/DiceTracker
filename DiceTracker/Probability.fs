
namespace DiceTracker

[<AutoOpen>]
module Probability =

    open System.Threading

    type ProbabilityValue =
        | DieValue of Die
        | Number of int
        | Argument of int
        | Sum of ProbabilityValue * ProbabilityValue
        | Difference of ProbabilityValue * ProbabilityValue
        | Multiply of ProbabilityValue * ProbabilityValue
        | Divide of ProbabilityValue * ProbabilityValue
        | Condition of BooleanValue * ProbabilityValue * ProbabilityValue
        | FunctionCall of Function * ProbabilityValue list
        | Binding of int * ProbabilityValue * ProbabilityValue
        | BoundValue of int

        static member inline (!.) v = v
        static member inline (!<.) v = v <>. 0

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
        | BoolBinding of int * ProbabilityValue * BooleanValue
        | BoolCondition of BooleanValue * BooleanValue * BooleanValue
        | Equals of ProbabilityValue * ProbabilityValue
        | NotEquals of ProbabilityValue * ProbabilityValue
        | GreaterThan of ProbabilityValue * ProbabilityValue
        | LessThan of ProbabilityValue * ProbabilityValue
        | GreaterThanEqual of ProbabilityValue * ProbabilityValue
        | LessThanEqual of ProbabilityValue * ProbabilityValue
        | BoolNot of BooleanValue
        | BoolAnd of BooleanValue * BooleanValue
        | BoolOr of BooleanValue * BooleanValue
        | BoolFunctionCall of BoolFunction * ProbabilityValue list
    
        static member inline (!.) bval = Condition(bval, Number 1, Number 0)
        static member inline (!<.) bval = bval

        static member inline (!) bval =
            match bval with
            | Equals(v, i) -> NotEquals(v, i)
            | NotEquals(v, i) -> Equals(v, i)
            | _ -> BoolNot bval
        
        static member inline (&&.) (a, b) = BoolAnd(a, b)
        static member inline (||.) (a, b) = BoolOr(a, b)
    
    and Function =
        { value : ProbabilityValue ; name : string option }
        override this.ToString() =
            match this.name with
            | Some n -> $"{{ name = {n} }}"
            | None -> $"{{ value = {this.value} }}"
    and BoolFunction =
        { value : BooleanValue ; name : string option }
        override this.ToString() =
            match this.name with
            | Some n -> $"{{ name = {n} }}"
            | None -> $"{{ value = {this.value} }}"
    
    and Die =
        { size : int }
        static member inline (!.) d = DieValue d
        override this.ToString() = sprintf "d%o" this.size

    let inline toProb value : ProbabilityValue = !. value
    let inline toBool value : BooleanValue = !<. value
    let Arg = Argument

    let func (args: ProbabilityValue list) result = FunctionCall({ value = result ; name = None }, args)
    let funcb (args: ProbabilityValue list) result = BoolFunctionCall({ value = result ; name = None }, args)

    let funcn name (args: ProbabilityValue list) result = FunctionCall({ value = result ; name = Some name }, args)
    let funcnb name (args: ProbabilityValue list) result = BoolFunctionCall({ value = result ; name = Some name }, args)

    let inline (!>) (value: int) = Number value
    let inline d size = { size = size } |> toProb
    let inline pool size count = count * (d size)

    let inline cond c t f = Condition(c, t, f)
    let inline condb c t f = BoolCondition(c, t, f)

    type private FunctionBinder() =
        let mutable id = 0
        member _.GetNextId() =
            let result = id
            id <- id + 1
            result

    type ProbabilityBuilder() =
        let mutable thisBinder : ThreadLocal<FunctionBinder option> = new ThreadLocal<_>(fun () -> None)

        member private _.BindId() = (Option.get thisBinder.Value).GetNextId()
        member this.Bind(binding, boundFunc: ProbabilityValue -> ProbabilityValue) =
            let id = this.BindId()
            Binding(id, binding, boundFunc (BoundValue id))
        member this.Bind(binding, boundFunc: ProbabilityValue -> BooleanValue) =
            let id = this.BindId()
            BoolBinding(id, binding, boundFunc (BoundValue id))

        member inline _.Return(v) = toProb v
        member inline _.ReturnFrom(v) = v

        member _.Delay(f) : unit -> 'a = f
        member _.Run(f) =
            let prev = thisBinder.Value
            thisBinder.Value <- Some (FunctionBinder())
            let result = f ()
            thisBinder.Value <- prev
            result

    let prob = ProbabilityBuilder()

    type OutputValue =
        | NamedOutput of string * ProbabilityValue
        | UnnamedOutput of ProbabilityValue

    let output value = UnnamedOutput value
    let outputName name value = NamedOutput(name, value)