
module Processing

open Prob
open FSharp.Data
open FSharp.Collections

type OutputCsv = CsvProvider<HasHeaders = false, Schema = "Name (string),Value (int),Probability (float)">

module private Internal =

    type AnyFunction = | BoolFunc of BoolFunction | IntFunc of Function

    let swap f a b = f b a
    let inline tmap f1 f2 (a, b) = (f1 a, f2 b)
    let inline tmap1 f (a, b) = (f a, b)
    let inline tmap2 f (a, b) = (a, f b)

    let rec getFunctionsInInt value existing =
        let binary a b = existing |> getFunctionsInInt a |> getFunctionsInInt b
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
        let binaryi a b = existing |> getFunctionsInInt a |> getFunctionsInInt b
        let binaryb a b = existing |> getFunctionsInBool a |> getFunctionsInBool b
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
        | RSum of ProbabilityResultResult * ProbabilityResultResult
        | ArgumentValue of int

    type BoolResultResult =
        | BoolValue of bool
        | REquals of ProbabilityResultResult * ProbabilityResultResult
        | RNEquals of ProbabilityResultResult * ProbabilityResultResult
        | RGt of ProbabilityResultResult * ProbabilityResultResult
        | RLt of ProbabilityResultResult * ProbabilityResultResult
        | RGte of ProbabilityResultResult * ProbabilityResultResult
        | RLte of ProbabilityResultResult * ProbabilityResultResult
        | RNot of BoolResultResult
        | RAnd of BoolResultResult * BoolResultResult
        | ROr of BoolResultResult * BoolResultResult

    type ProbabilityResultValue =
        | Probability of float
        | PSum of ProbabilityResultValue * ProbabilityResultValue
        | PProd of ProbabilityResultValue * ProbabilityResultValue

    type ProbResults<'a when 'a : comparison> = Map<'a, ProbabilityResultValue>
    type NormalResults = ProbResults<ProbabilityResultResult>
    type BoolResults = ProbResults<BoolResultResult>

    type AnyProbResults = | AnyNormalResults of NormalResults | AnyBoolResults of BoolResults

    type BindingSet = Map<int, NormalResults>

    type FunctionCache = Map<AnyFunction, AnyProbResults>
    
    let rec tryGetValueRInt res =
        match res with
        | IntValue v -> Some v
        | RSum(a, b) -> Option.map2 (+) (tryGetValueRInt a) (tryGetValueRInt b)
        | _ -> None

    let rec reduceResult res =
        match tryGetValueRInt res, res with
        | Some i, _ -> IntValue i
        | _, a -> a

    and tryGetValueRBool res =
        let binop getVal op a b = Option.map2 op (getVal a) (getVal b)
        match res with
        | BoolValue v -> Some v
        | REquals(a, b) -> 
            binop tryGetValueRInt (=) a b
            |> Option.orElseWith (fun () -> if reduceResult a = reduceResult b then Some true else None)
        | RNEquals(a, b) ->
            binop tryGetValueRInt (<>) a b
            |> Option.orElseWith (fun () -> if reduceResult a = reduceResult b then Some false else None)
        | RGt(a, b) -> binop tryGetValueRInt (>) a b
        | RLt(a, b) -> binop tryGetValueRInt (<) a b
        | RGte(a, b) -> binop tryGetValueRInt (>=) a b
        | RLte(a, b) -> binop tryGetValueRInt (<=) a b
        | RAnd(a, b) -> binop tryGetValueRBool (&&) a b
        | ROr(a, b) -> binop tryGetValueRBool (||) a b
        | RNot a -> tryGetValueRBool a |> Option.map not

    and reduceBoolResult res =
        match tryGetValueRBool res, res with
        | Some i, _ -> BoolValue i
        | _, REquals(a, b) -> REquals(reduceResult a, reduceResult b)
        | _, RNEquals(a, b) -> RNEquals(reduceResult a, reduceResult b)
        | _, RGt(a, b) -> RGt(reduceResult a, reduceResult b)
        | _, RLt(a, b) -> RLt(reduceResult a, reduceResult b)
        | _, RGte(a, b) -> RGte(reduceResult a, reduceResult b)
        | _, RLte(a, b) -> RLte(reduceResult a, reduceResult b)
        | _, RAnd(a, b) -> RAnd(reduceBoolResult a, reduceBoolResult b)
        | _, ROr(a, b) -> ROr(reduceBoolResult a, reduceBoolResult b)
        | _, RNot(REquals(a, b)) -> RNEquals(reduceResult a, reduceResult b)
        | _, RNot(RNEquals(a, b)) -> REquals(reduceResult a, reduceResult b)
        | _, RNot(RGt(a, b)) -> RLte(reduceResult a, reduceResult b)
        | _, RNot(RLt(a, b)) -> RGte(reduceResult a, reduceResult b)
        | _, RNot(RGte(a, b)) -> RLt(reduceResult a, reduceResult b)
        | _, RNot(RLte(a, b)) -> RGt(reduceResult a, reduceResult b)
        | _, RNot(RNot(a)) -> a
        | _, RNot a -> RNot(reduceBoolResult a)
        | _, a -> a

    let rec reduceProb prob =
        let rec tryGetValue p =
            let binop op a b = Option.map2 op (tryGetValue a) (tryGetValue b)
            match p with
            | Probability v -> Some v
            | PSum(a, b) -> binop (+) a b
            | PProd(a, b) -> binop (*) a b 

        match tryGetValue prob, prob with
        | Some v, _ -> Probability v
        | _, PSum(a, b) -> PSum(reduceProb a, reduceProb b)
        | _, PProd(a, b) -> PProd(reduceProb a, reduceProb b)
        | _, a -> a

    let inline buildMap seq = Seq.fold (fun m (k, v) -> Map.change k (fun o -> Some(match o with | Some(o) -> PSum(v, o) | None -> v)) m) Map.empty seq

    // TODO: implement all of these
    let rec buildCallImplI args v = v

    and buildCallImplB args v = v

    let buildCallImplP args p = p 

    let rec buildCallInt funcResults (args: NormalResults list) = 
        Map.toSeq funcResults
        |> Seq.map (tmap (buildCallImplI args) (buildCallImplP args))
        |> buildMap
    and buildCallBool funcResults (args: NormalResults list) =
        Map.toSeq funcResults
        |> Seq.map (tmap (buildCallImplB args) (buildCallImplP args))
        |> buildMap

    let findOrAdd key mkFunc map =
        match Map.tryFind key map with
        | Some v -> v, map
        | None ->
            let (v, map) = mkFunc key map
            v, (Map.add key v map)

    let rec analyze bindings cache value : NormalResults * FunctionCache =
        match value with
        | Number n -> Map.add (IntValue n) (Probability 1.) Map.empty, cache
        | Argument i -> Map.add (ArgumentValue i) (Probability 1.) Map.empty, cache
        | DieValue { size = n } -> seq { for i in 1..n -> IntValue i, Probability (1./(float n)) } |> NormalResults, cache
        | Sum(a, b) -> 
            let (ra, cache) = analyze bindings cache a |> tmap1 Map.toSeq
            let (rb, cache) = analyze bindings cache b |> tmap1 Map.toSeq
            Seq.allPairs ra rb
            |> Seq.map (fun ((k1, v1), (k2, v2)) -> RSum(k1, k2), PProd(v1, v2))
            //|> Seq.map (tmap reduceResult reduceProb)
            |> buildMap, cache

        | Condition(cond, t, f) ->
            let (result : BoolResults, cache) = analyzeBool bindings cache cond
            let (result, cache) = 
                result |> Map.toSeq
                |> Seq.map (tmap reduceBoolResult reduceProb)
                |> Seq.mapFold (fun cache (BoolValue r, v) -> // TODO: make this support conditions which aren't known statically
                    let (res, cache) = analyze bindings cache (if r then t else f)
                    let res = 
                        res |> Map.toSeq 
                        |> Seq.map (fun (k, v2) -> k, PProd(v, v2))
                    res, cache) cache
            result |> Seq.concat |> buildMap, cache

        | FunctionCall(func, args) ->
            let (results, cache) = analyzeIntFunc bindings cache func
            let (args, cache) = List.mapFold (analyze bindings) cache args
            buildCallInt results args, cache

        | Binding(i, value, expr) ->
            let (value, cache) = analyze bindings cache value
            analyze (Map.add i value bindings) cache expr

        | BoundValue i ->
            // TODO: how do I make this correctly account for each case?
            Map.tryFind i bindings |> Option.get, cache

    and analyzeBool bindings cache value : BoolResults * FunctionCache =
        let binop analyze op a b =
            let (ra, cache) = analyze bindings cache a |> tmap1 Map.toSeq
            let (rb, cache) = analyze bindings cache b |> tmap1 Map.toSeq
            Seq.allPairs ra rb
            |> Seq.map (fun ((k1, v1), (k2, v2)) -> op (k1, k2), PProd(v1, v2))
            //|> Seq.map (tmap reduceBoolResult reduceProb)
            |> buildMap, cache
        match value with
        | Literal b -> Map.add (BoolValue b) (Probability 1.) Map.empty, cache
        | Equals(a, b) -> binop analyze REquals a b
        | NotEquals(a, b) -> binop analyze RNEquals a b
        | GreaterThan(a, b) -> binop analyze RGt a b
        | LessThan(a, b) -> binop analyze RLt a b
        | GreaterThanEqual(a, b) -> binop analyze RGte a b
        | LessThanEqual(a, b) -> binop analyze RLte a b
        | BoolNot b ->
            analyzeBool bindings cache b 
            |> tmap1 Map.toSeq
            |> tmap1 (Seq.map (fun (k, v) -> (RNot k), v))
            |> tmap1 buildMap
        | BoolAnd(a, b) -> binop analyzeBool RAnd a b
        | BoolOr(a, b) -> binop analyzeBool ROr a b

        | BoolCondition(cond, t, f) ->
            let (result : BoolResults, cache) = analyzeBool bindings cache cond
            let (result, cache) = 
                result |> Map.toSeq
                |> Seq.map (tmap reduceBoolResult reduceProb)
                |> Seq.mapFold (fun cache (BoolValue r, v) -> // TODO: make this support conditions which aren't known statically
                    let (res, cache) = analyzeBool bindings cache (if r then t else f)
                    let res = 
                        res |> Map.toSeq 
                        |> Seq.map (fun (k, v2) -> k, PProd(v, v2))
                    res, cache) cache
            result |> Seq.concat |> buildMap, cache

        | BoolFunctionCall(func, args) ->
            let (results, cache) = analyzeBoolFunc bindings cache func
            let (args, cache) = List.mapFold (analyze bindings) cache args
            buildCallBool results args, cache

        | BoolBinding(i, value, expr) ->
            let (value, cache) = analyze bindings cache value
            analyzeBool (Map.add i value bindings) cache expr
            
    and analyzeBoolFunc bindings cache func =
        findOrAdd (BoolFunc func) (fun _ m -> analyzeBool bindings m func.value |> tmap1 AnyBoolResults) cache
        |> tmap1 (fun v -> match v with | AnyBoolResults r -> r | _ -> raise (exn "Somehow function had the wrong result type"))
    and analyzeIntFunc bindings cache func =
        findOrAdd (IntFunc func) (fun _ m -> analyze bindings m func.value |> tmap1 AnyNormalResults) cache
        |> tmap1 (fun v -> match v with | AnyNormalResults r -> r | _ -> raise (exn "Somehow function had the wrong result type"))

    let processWithName (cache: FunctionCache) name prob =
        //let functions = getFunctionsInInt prob [] |> List.distinct
        //let cache = functions |> List.mapFold analyzeAnyFunction cache |> snd
        // we calculate and cache the analyzed functions, but ignore their results for the actual final analysis
        let (result, cache) = analyze Map.empty cache prob
        let result =
            result
            |> Map.toSeq
            |> Seq.map (tmap reduceResult reduceProb)
            |> Seq.map (tmap1 (fun v -> match v with | IntValue v -> v | _ -> raise (exn $"Found unresolvable value %O{v}")))
            |> Seq.map (tmap2 (fun v -> match v with | Probability p -> p | _ -> raise (exn $"Found unresolvable probability %O{v}")))
            |> Seq.map (fun (v, p) -> OutputCsv.Row(name, v, p))
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
