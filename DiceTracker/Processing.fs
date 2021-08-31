
namespace DiceTracker

module Processing =

    open FSharp.Collections

    module private Internal =

        type AnyFunction = | BoolFunc of BoolFunction | IntFunc of Function

        let swap f a b = f b a
        let inline tmap f1 f2 (a, b) = (f1 a, f2 b)
        let inline tmap1 f (a, b) = (f a, b)
        let inline tmap2 f (a, b) = (a, f b)

        type ProbabilityResultResult =
            | IntValue of int
            | RSum of ProbabilityResultResult * ProbabilityResultResult
            | RDiff of ProbabilityResultResult * ProbabilityResultResult
            | RMul of ProbabilityResultResult * ProbabilityResultResult
            | RDiv of ProbabilityResultResult * ProbabilityResultResult

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
            let binop op a b = Option.map2 op (tryGetValueRInt a) (tryGetValueRInt b)
            match res with
            | IntValue v -> Some v
            | RSum(a, b) -> binop (+) a b
            | RDiff(a, b) -> binop (-) a b
            | RMul(a, b) -> binop (*) a b
            | RDiv(a, b) -> binop (/) a b

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

        let buildMap seq = Seq.fold (fun m (k, v) -> Map.change k (fun o -> Some(match o with | Some(o) -> PSum(v, o) | None -> v)) m) Map.empty seq

        let rec buildCallInt args funcVal = 
            let binop op a b = op (buildCallInt args a, buildCallInt args b)
            match funcVal with
            | Argument i -> List.item i args
            | DieValue _ -> funcVal
            | Number _ -> funcVal
            | Sum(a, b) -> binop Sum a b
            | Difference(a, b) -> binop Difference a b
            | Multiply(a, b) -> binop Multiply a b
            | Divide(a, b) -> binop Divide a b
            | Condition(c, a, b) -> Condition(buildCallBool args c, buildCallInt args a, buildCallInt args b)
            | FunctionCall(func, fargs) -> FunctionCall(func, fargs |> List.map (buildCallInt args))
            | Binding(i, v, r) -> Binding(i, buildCallInt args v, buildCallInt args r)
            | BoundValue _ -> funcVal

        and buildCallBool args funcVal =
            let binop build op a b = op (build args a, build args b)
            match funcVal with
            | Literal _ -> funcVal
            | BoolBinding(i, v, r) -> BoolBinding(i, buildCallInt args v, buildCallBool args r)
            | BoolCondition(c, a, b) -> BoolCondition(buildCallBool args c, buildCallBool args a, buildCallBool args b)
            | Equals(a, b) -> binop buildCallInt Equals a b
            | NotEquals(a, b) -> binop buildCallInt NotEquals a b
            | GreaterThan(a, b) -> binop buildCallInt GreaterThan a b
            | LessThan(a, b) -> binop buildCallInt LessThan a b
            | GreaterThanEqual(a, b) -> binop buildCallInt GreaterThanEqual a b
            | LessThanEqual(a, b) -> binop buildCallInt LessThanEqual a b
            | BoolNot b -> BoolNot(buildCallBool args b)
            | BoolAnd(a, b) -> binop buildCallBool BoolAnd a b
            | BoolOr(a, b) -> binop buildCallBool BoolOr a b
            | BoolFunctionCall(func, fargs) -> BoolFunctionCall(func, fargs |> List.map (buildCallInt args))

        let findOrAdd key mkFunc map =
            match Map.tryFind key map with
            | Some v -> v, map
            | None ->
                let (v, map) = mkFunc key map
                v, (Map.add key v map)

        let rec analyze bindings cache value : NormalResults * FunctionCache =
            let binop op a b =
                let (ra, cache) = analyze bindings cache a |> tmap1 Map.toSeq
                let (rb, cache) = analyze bindings cache b |> tmap1 Map.toSeq
                Seq.allPairs ra rb
                |> Seq.map (fun ((k1, v1), (k2, v2)) -> op (k1, k2), PProd(v1, v2))
                |> buildMap, cache
            match value with
            | Number n -> Map.add (IntValue n) (Probability 1.) Map.empty, cache
            | Argument i -> raise (exn "Unbound argument found in analysis")
            | DieValue { size = n } -> seq { for i in 1..n -> IntValue i, Probability (1./(float n)) } |> NormalResults, cache
            | Sum(a, b) -> binop RSum a b
            | Difference(a, b) -> binop RDiff a b
            | Multiply(a, b) -> binop RMul a b
            | Divide(a, b) -> binop RDiv a b

            | Condition(cond, t, f) ->
                let (result : BoolResults, cache) = analyzeBool bindings cache cond
                let (result, cache) = 
                    result |> Map.toSeq
                    |> Seq.map (tmap reduceBoolResult reduceProb)
                    |> Seq.mapFold (fun cache (BoolValue r, v) ->
                        let (res, cache) = analyze bindings cache (if r then t else f)
                        let res = 
                            res |> Map.toSeq 
                            |> Seq.map (fun (k, v2) -> k, PProd(v, v2))
                        res, cache) cache
                result |> Seq.concat |> buildMap, cache

            | FunctionCall(func, args) ->
                // TODO: make functions get analyzed independent of arguments to facilitate better binding
                let value = buildCallInt args func.value
                analyze bindings cache value

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
                    |> Seq.mapFold (fun cache (BoolValue r, v) ->
                        let (res, cache) = analyzeBool bindings cache (if r then t else f)
                        let res = 
                            res |> Map.toSeq 
                            |> Seq.map (fun (k, v2) -> k, PProd(v, v2))
                        res, cache) cache
                result |> Seq.concat |> buildMap, cache

            | BoolFunctionCall(func, args) ->
                let value = buildCallBool args func.value
                analyzeBool bindings cache value

            | BoolBinding(i, value, expr) ->
                let (value, cache) = analyze bindings cache value
                analyzeBool (Map.add i value bindings) cache expr
            
        let processWithName (cache: FunctionCache) name prob =
            let (result, cache) = analyze Map.empty cache prob
            let result =
                result
                |> Map.toSeq
                |> Seq.map (tmap reduceResult reduceProb)
                |> Seq.map (tmap1 (fun v -> match v with | IntValue v -> v | _ -> raise (exn $"Found unresolvable value %O{v}")))
                |> Seq.map (tmap2 (fun v -> match v with | Probability p -> p | _ -> raise (exn $"Found unresolvable probability %O{v}")))
                |> Map
            name, result, cache


    let private processOneImpl cache i data =
        match data with
        | NamedOutput(name, value) -> Internal.processWithName cache name value
        | UnnamedOutput(value) -> Internal.processWithName cache (sprintf "output %o" i) value

    /// Processes some list of OutputValues into a map of their names to their output probabilities.
    let processMany (data: OutputValue seq) =
        data 
        |> Seq.mapi (fun i v -> (i, v)) 
        |> Seq.fold (fun (sets, cache) (i, v) -> 
                        let (name, r, cache) = processOneImpl cache i v
                        (name, r)::sets, cache) ([], Map.empty)
        |> fst
        |> List.rev
        |> Map

    /// Processes a single OutputValue into its output probabilities
    let processOne = processOneImpl Map.empty 1 >> fun (_, b, _) -> b
