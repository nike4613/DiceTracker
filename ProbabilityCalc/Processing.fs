﻿
module Processing

open Prob
open FSharp.Data
open FSharp.Collections

type OutputCsv = CsvProvider<HasHeaders = false, Schema = "Name (string),Value (int),Probability (float)">

module private Internal =

    type AnyFunction = | BoolFunc of BoolFunction | IntFunc of Function

    let swap f a b = f b a
    let tmap f1 f2 (a, b) = (f1 a, f2 b)
    let tmap1 f (a, b) = (f a, b)
    let tmap2 f (a, b) = (a, f b)

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

    type ProbabilityResultResult =
        | IntValue of int
        | BoolValue of bool
        | RSum of ProbabilityResultResult * ProbabilityResultResult
        | ArgumentValue of int

    type ProbabilityResultValue =
        | Probability of float
        | PSum of ProbabilityResultValue * ProbabilityResultValue
        | PProd of ProbabilityResultValue * ProbabilityResultValue

    type ProbResults<'a when 'a : comparison> = Map<'a, ProbabilityResultValue>
    type NormalResults = ProbResults<ProbabilityResultResult>
    type BoolResults = ProbResults<bool>

    type AnyProbResults = | AnyNormalResults of NormalResults | AnyBoolResults of BoolResults

    type FunctionCache = Map<AnyFunction, AnyProbResults>

    let rec reduceResult res = res // TODO: implement

    let rec reduceProb prob = prob // TODO: implement

    let buildMap : seq<_> -> NormalResults = Seq.fold (fun m (k, v) -> Map.change k (fun o -> Some(match o with | Some(o) -> PSum(v, o) | None -> v)) m) Map.empty

    let rec analyze cache value : NormalResults * FunctionCache =
        match value with
        | Number n -> Map.add (IntValue n) (Probability 1.) Map.empty, cache
        | Argument i -> Map.add (ArgumentValue i) (Probability 1.) Map.empty, cache
        | DieValue { size = n } -> seq { for i in 1..n -> IntValue i, Probability (1./(float n)) } |> NormalResults, cache
        | Sum(a, b) -> 
            let (ra, cache) = analyze cache a |> tmap1 Map.toSeq
            let (rb, cache) = analyze cache b |> tmap1 Map.toSeq
            Seq.allPairs ra rb
            |> Seq.map (fun ((k1, v1), (k2, v2)) -> RSum(k1, k2), PProd(v1, v2))
            |> Seq.map (tmap reduceResult reduceProb)
            |> buildMap, cache

        | Condition(cond, t, f) ->
            let (result : BoolResults, cache) = analyzeBool cache cond
            let (result, cache) = 
                result |> Map.toSeq
                |> Seq.mapFold (fun cache (r, v) ->
                    let (res, cache) = analyze cache (if r then t else f)
                    let res = 
                        res |> Map.toSeq 
                        |> Seq.map (fun (k, v2) -> k, PProd(v, v2))
                    res, cache) cache
            result |> Seq.concat |> buildMap, cache


    and analyzeBool cache value =
        Map.empty, cache // TODO:

    and analyzeAnyFunction (cache: FunctionCache) func =
        let doAnalyzeFunction cache func =
            match func with
            | BoolFunc b -> analyzeBool cache b.value |> tmap1 AnyBoolResults
            | IntFunc i -> analyze cache i.value |> tmap1 AnyNormalResults

        match Map.tryFind func cache with
        | Some r -> r, cache
        | None ->
            let (result, cache) = doAnalyzeFunction cache func
            result, Map.add func result cache

    let processWithName (cache: FunctionCache) name prob =
        let functions = getFunctionsInInt prob [] |> List.distinct
        //let cache = functions |> List.mapFold analyzeAnyFunction cache |> snd
        // we calculate and cache the analyzed functions, but ignore their results for the actual final analysis
        let (result, cache) = analyze cache prob
        let result =
            result
            |> Map.toSeq
            |> Seq.map (tmap reduceResult reduceProb)
            |> Seq.map (fun (IntValue v, Probability p) -> OutputCsv.Row(name, v, p))
        result, cache


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
