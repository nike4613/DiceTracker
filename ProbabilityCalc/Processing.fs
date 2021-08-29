
module Processing

open Prob
open FSharp.Data
open FSharp.Collections

type OutputCsv = CsvProvider<HasHeaders = false, Schema = "Name (string),Value (int),Occurences (int),Probability (float)">

module private Internal =

    type AnyFunction = | BoolFunc of BoolFunction | IntFunc of Function

    let swap f a b = f b a

    let rec getFunctionsInInt value existing =
        let inline binary a b = existing |> getFunctionsInInt a |> getFunctionsInInt b
        match value with
        | Sum(a, b) -> binary a b
        | Difference(a, b) -> binary a b
        | Multiply(a, b) -> binary a b
        | Divide(a, b) -> binary a b
        | Condition(cond, a, b) -> existing |> getFunctionsInBool cond |> getFunctionsInInt a |> getFunctionsInInt b
        | FunctionCall(func, args) -> args |> Seq.fold (swap getFunctionsInInt) (IntFunc func :: existing) |> getFunctionsInInt func.value
        | Binding(_, binding, value) -> existing |> getFunctionsInInt binding |> getFunctionsInInt value
        | Number _ -> existing
        | Argument _ -> existing
        | DieValue _ -> existing
        | BoundValue _ -> existing
    and getFunctionsInBool value existing =
        let inline binaryi a b = existing |> getFunctionsInInt a |> getFunctionsInInt b
        let inline binaryb a b = existing |> getFunctionsInBool a |> getFunctionsInBool b
        match value with
        | Literal _ -> existing
        | Equals(a, b) -> binaryi a b
        | NotEquals(a, b) -> binaryi a b
        | GreaterThan(a, b) -> binaryi a b
        | LessThan(a, b) -> binaryi a b
        | GreaterThanEqual(a, b) -> binaryi a b
        | LessThanEqual(a, b) -> binaryi a b
        | BoolNot v -> getFunctionsInBool v existing
        | BoolAnd(a, b) -> binaryb a b
        | BoolOr(a, b) -> binaryb a b
        | BoolCondition(cond, a, b) -> existing |> getFunctionsInBool cond |> getFunctionsInBool a |> getFunctionsInBool b
        | BoolFunctionCall(func, args) -> args |> Seq.fold (swap getFunctionsInInt) (BoolFunc func :: existing) |> getFunctionsInBool func.value
        | BoolBinding(_, binding, value) -> existing |> getFunctionsInInt binding |> getFunctionsInBool value

    type ProbabilityResult =
        | Probability of float

    type BoolProbResults = { isTrue : ProbabilityResult ; isFalse : ProbabilityResult }
    type IntProbResults = Map<int, ProbabilityResult>

    type AnyProbResults = | BoolResults of BoolProbResults | IntResults of IntProbResults

    type FunctionCache = Map<AnyFunction, AnyProbResults>

    let private doAnalyzeIntFunction cache func =
        IntResults Map.empty, cache

    let private doAnalyzeBoolFunction cache func =
        BoolResults { isTrue = Probability 0. ; isFalse = Probability 1. }, cache

    let private doAnalyzeFunction cache func =
        match func with
        | BoolFunc b -> doAnalyzeBoolFunction cache b
        | IntFunc i -> doAnalyzeIntFunction cache i

    let analyzeAnyFunction (cache: FunctionCache) func =
        match Map.tryFind func cache with
        | Some r -> r, cache
        | None ->
            let (result, cache) = doAnalyzeFunction cache func
            result, Map.add func result cache

    let processWithName (cache: FunctionCache) name prob =
        let functions = getFunctionsInInt prob [] |> List.distinct
        Seq.empty, cache


let private makeCsv data = new OutputCsv(data)

let private processOneImpl cache i data =
    match data with
    | NamedOutput(name, value) -> Internal.processWithName cache name value
    | UnnamedOutput(value) -> Internal.processWithName cache (sprintf "output %o" i) value

let private processManyImpl (data: OutputValue seq) =
    data 
    |> Seq.mapi (fun i v -> (i, v)) 
    |> Seq.fold (fun (sets, cache) (i, v) -> 
                    let (r, cache) = processOneImpl cache i v
                    r::sets, cache) ([], Map.empty)
    |> fst
    |> List.rev
    |> Seq.concat

let processOne = processOneImpl Map.empty 1 >> fst >> makeCsv
let processMany : OutputValue seq -> OutputCsv = processManyImpl >> makeCsv
