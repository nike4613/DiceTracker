
module Prob

type BindingIdentity() =
    override a.ToString() = $"{a.GetHashCode()}"
    override a.Equals(b) = obj.ReferenceEquals(a, b)
    override a.GetHashCode() = System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(a)
    interface System.IComparable with
        member a.CompareTo(other) =
            match other with 
            | :? BindingIdentity as b -> (a.GetHashCode()).CompareTo(b.GetHashCode())
            | _ -> -1

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
    | Binding of BindingIdentity * ProbabilityValue * ProbabilityValue
    | BoundValue of BindingIdentity

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

    static member inline (!) bval =
        match bval with
        | Equals(v, i) -> NotEquals(v, i)
        | NotEquals(v, i) -> Equals(v, i)
        | _ -> BoolNot bval
        
    static member inline (&&.) (a, b) = BoolAnd(a, b)
    static member inline (||.) (a, b) = BoolOr(a, b)

    static member inline (!.) bval = Condition(bval, Number 1, Number 0)
    
and Function = { value : ProbabilityValue }
and BoolFunction = { value : BooleanValue }
    
and Die =
    { size : int }
    static member inline (!.) d = DieValue d
    override this.ToString() = sprintf "d%o" this.size

let inline toProb value : ProbabilityValue = !. value
let Arg = Argument

let func (args: ProbabilityValue list) result = FunctionCall({ value = result }, args)
let funcb (args: ProbabilityValue list) result = BoolFunctionCall({ value = result }, args)

let inline (!>) (value: int) = Number value
let inline d size = { size = size }
let inline pool size count = count * (d size)

let inline cond c t f = Condition(c, t, f)
let inline condb c t f = BoolCondition(c, t, f)

// TODO: find a way to make invocations of a function not distinct
type ProbabilityBuilder() =
    member _.Bind(binding, boundFunc) =
        let bind = BindingIdentity()
        Binding(bind, binding, boundFunc (BoundValue bind))
    member inline _.Return(v) = toProb v
    member inline _.ReturnFrom(v) = v

    //member _.Delay(f) = f()
    //member _.Run(v) = v

let prob = ProbabilityBuilder()

type OutputValue =
    | NamedOutput of string * ProbabilityValue
    | UnnamedOutput of ProbabilityValue

let output value = UnnamedOutput value
let outputName name value = NamedOutput(name, value)