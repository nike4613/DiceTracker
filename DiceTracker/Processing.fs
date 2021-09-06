
namespace DiceTracker

module Processing =

    open FSharp.Collections

    module private Internal =

        let swap f a b = f b a
        let inline tmap f1 f2 (a, b) = (f1 a, f2 b)
        let inline tmap1 f (a, b) = (f a, b)
        let inline tmap2 f (a, b) = (a, f b)

        module Seq =
            let ofTuple (a, b) = seq { yield a; yield b }
            let (|Empty|_|) seq = if Seq.isEmpty seq then Some() else None

        type ProbabilityResultResult =
            | IntValue of int
            | RSum of ProbabilityResultResult * ProbabilityResultResult
            | RDiff of ProbabilityResultResult * ProbabilityResultResult
            | RMul of ProbabilityResultResult * ProbabilityResultResult
            | RDiv of ProbabilityResultResult * ProbabilityResultResult
            | ArgValue of index:int

        module ProbabilityResultResult =
            type Self = ProbabilityResultResult
            [<RequireQualifiedAccess>]
            type UnpackResult = 
                | None of Self
                | Binary of {| ctor: Self*Self -> Self; values: Self*Self |}
            let (|Unpack|) res =
                match res with
                | IntValue _
                | ArgValue _ -> UnpackResult.None res
                | RSum(a, b) -> UnpackResult.Binary {| ctor = RSum ; values = a,b |}
                | RDiff(a, b) -> UnpackResult.Binary {| ctor = RDiff ; values = a,b |}
                | RMul(a, b) -> UnpackResult.Binary {| ctor = RMul ; values = a,b |}
                | RDiv(a, b) -> UnpackResult.Binary {| ctor = RDiv ; values = a,b |}
            let repack map result =
                match result with
                | UnpackResult.None v -> v
                | UnpackResult.Binary r -> r.values |> tmap map map |> r.ctor

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

        module BoolResultResult =
            type Self = BoolResultResult
            type Other = ProbabilityResultResult
            [<RequireQualifiedAccess>]
            type UnpackResult = 
                | None of Self
                | Unary of {| ctor: Self -> Self ; value: Self |}
                | SelfBinary of {| ctor: Self*Self -> Self ; values: Self*Self |}
                | OtherBinary of {| ctor: Other*Other -> Self ; values: Other*Other |}
            let (|Unpack|) res =
                match res with
                | BoolValue _ -> UnpackResult.None res
                | RNot v -> UnpackResult.Unary {| ctor = RNot ; value = v |}
                | RAnd(a, b) -> UnpackResult.SelfBinary {| ctor = RAnd ; values = a,b |}
                | ROr(a, b) -> UnpackResult.SelfBinary {| ctor = ROr ; values = a,b |}
                | REquals(a, b) -> UnpackResult.OtherBinary {| ctor = REquals ; values = a,b |}
                | RNEquals(a, b) -> UnpackResult.OtherBinary {| ctor = RNEquals ; values = a,b |}
                | RGt(a, b) -> UnpackResult.OtherBinary {| ctor = RGt ; values = a,b |}
                | RLt(a, b) -> UnpackResult.OtherBinary {| ctor = RLt ; values = a,b |}
                | RGte(a, b) -> UnpackResult.OtherBinary {| ctor = RGte ; values = a,b |}
                | RLte(a, b) -> UnpackResult.OtherBinary {| ctor = RLte ; values = a,b |}
            let repack imap bmap result = 
                match result with
                | UnpackResult.None v -> v
                | UnpackResult.Unary r -> r.value |> bmap |> r.ctor
                | UnpackResult.SelfBinary r -> r.values |> tmap bmap bmap |> r.ctor
                | UnpackResult.OtherBinary r -> r.values |> tmap imap imap |> r.ctor

        type ProbabilityResultValue =
            | Probability of float
            | PSum of ProbabilityResultValue * ProbabilityResultValue
            | PProd of ProbabilityResultValue * ProbabilityResultValue
            | PCond of BoolResultResult * ProbabilityResultValue * ProbabilityResultValue

        module ProbabilityResultValue = 
            type Self = ProbabilityResultValue
            [<RequireQualifiedAccess>]
            type UnpackResult =
                | None of Self
                | Binary of {| ctor: Self*Self -> Self ; values: Self*Self |}
                | Cond of BoolResultResult * Self * Self
            let (|Unpack|) res =
                match res with
                | Probability _ -> UnpackResult.None res
                | PSum(a, b) -> UnpackResult.Binary {| ctor = PSum ; values = a,b |}
                | PProd(a, b) -> UnpackResult.Binary {| ctor = PProd ; values = a,b |}
                | PCond(c, t, f) -> UnpackResult.Cond(c, t, f)
            let repack bmap rmap result =
                match result with
                | UnpackResult.None v -> v
                | UnpackResult.Binary dc -> dc.values |> tmap rmap rmap |> dc.ctor
                | UnpackResult.Cond(c, t, f) -> PCond(bmap c, rmap t, rmap f)

        type ProbResults<'a when 'a : comparison> = Map<'a, ProbabilityResultValue>
        type NormalResults = ProbResults<ProbabilityResultResult>
        type BoolResults = ProbResults<BoolResultResult>

        type BindingSet = Map<int, NormalResults>
    
        let rec tryGetValueRInt res =
            let binop op a b = Option.map2 op (tryGetValueRInt a) (tryGetValueRInt b)
            match res with
            | IntValue v -> Some v
            | RSum(a, b) -> binop (+) a b
            | RDiff(a, b) -> binop (-) a b
            | RMul(a, b) -> binop (*) a b
            | RDiv(a, b) -> binop (/) a b
            | ArgValue _ -> None

        let rec reduceResult res =
            match tryGetValueRInt res, res with
            | Some i, _ -> IntValue i
            | _, ProbabilityResultResult.Unpack r -> ProbabilityResultResult.repack reduceResult r

        let rec tryGetValueRBool res =
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

        let rec reduceBoolResult res =
            match tryGetValueRBool res, res with
            | Some v, _ -> BoolValue v
            | _, RNot(REquals(a, b)) -> RNEquals(reduceResult a, reduceResult b)
            | _, RNot(RNEquals(a, b)) -> REquals(reduceResult a, reduceResult b)
            | _, RNot(RGt(a, b)) -> RLte(reduceResult a, reduceResult b)
            | _, RNot(RLt(a, b)) -> RGte(reduceResult a, reduceResult b)
            | _, RNot(RGte(a, b)) -> RLt(reduceResult a, reduceResult b)
            | _, RNot(RLte(a, b)) -> RGt(reduceResult a, reduceResult b)
            | _, RNot(RNot(a)) -> a
            | _, BoolResultResult.Unpack r -> BoolResultResult.repack reduceResult reduceBoolResult r

        let rec reduceProb prob =
            let rec tryGetValue p =
                let binop op a b = Option.map2 op (tryGetValue a) (tryGetValue b)
                match p with
                | Probability v -> Some v
                | PSum(a, b) -> binop (+) a b
                | PProd(a, b) -> binop (*) a b
                | PCond(c, t, f) ->
                    match tryGetValueRBool c with
                    | None -> None
                    | Some b -> if b then tryGetValue t else tryGetValue f

            match tryGetValue prob, prob with
            | Some v, _ -> Probability v
            | _, ProbabilityResultValue.Unpack r -> ProbabilityResultValue.repack reduceBoolResult reduceProb r

        let buildMap seq = Seq.fold (fun m (k, v) -> Map.change k (fun o -> Some(match o with | Some(o) -> PSum(v, o) | None -> v)) m) Map.empty seq

        let filterImpossible seq =
            seq |> Seq.filter (function
                | _, Probability 0. -> false
                | _, _ -> true)

        let reduceSeq seq = seq |> Seq.map (tmap reduceResult reduceProb) |> filterImpossible
        let reduceBoolSeq seq = seq |> Seq.map (tmap reduceBoolResult reduceProb) |> filterImpossible

        let reduceResultMap = Map.toSeq >> reduceSeq >> Map
        let reduceBoolResultMap = Map.toSeq >> reduceBoolSeq >> Map

        let cartProd (args: 'a seq seq) = //: 'a seq seq =
            args
            |> Seq.fold 
                (function
                | Seq.Empty -> Seq.singleton
                | l -> fun v ->
                    Seq.allPairs l v
                    |> Seq.map (fun (a, b) -> Seq.append a (Seq.singleton b))) 
                Seq.empty

        let rec buildCallFixInt (args: _ list) value =
            match value with
            | ArgValue i -> List.item i args
            | ProbabilityResultResult.Unpack r -> ProbabilityResultResult.repack (buildCallFixInt args) r

        and buildCallFixBool (args: _ list) (BoolResultResult.Unpack r) =
            BoolResultResult.repack (buildCallFixInt args) (buildCallFixBool args) r

        and buildCallFixProb (args: _ list) (ProbabilityResultValue.Unpack r) =
            ProbabilityResultValue.repack (buildCallFixBool args) (buildCallFixProb args) r

        let buildCallImpl fixImpl (args: NormalResults list) funcVal =
            let args = args |> Seq.map Map.toSeq |> cartProd |> Seq.map Seq.toList |> Seq.cache
            funcVal
            |> Map.toSeq
            |> Seq.allPairs args
            |> Seq.map (fun (args, (value, prob)) -> args |> List.map fst, (value, prob), List.fold (fun a b -> PProd(a, snd b)) prob args)
            |> Seq.map (fun (args, (value, _), prob) -> fixImpl args value, buildCallFixProb args prob)
            |> buildMap

        let buildCallInt (args: NormalResults list) (funcVal: NormalResults) : NormalResults =
            buildCallImpl buildCallFixInt args funcVal

        let buildCallBool (args: NormalResults list) (funcVal: BoolResults) : BoolResults =
            buildCallImpl buildCallFixBool args funcVal
            
        type FunctionCache = Map<Function, NormalResults> * Map<BoolFunction, BoolResults>
        module FunctionCache =
            let empty : FunctionCache = Map.empty, Map.empty

        let findOrAdd1 key mkFunc (cache : FunctionCache) =
            match Map.tryFind key (fst cache) with
            | Some v -> v, cache
            | None ->
                let (v, cache) = mkFunc key cache
                v, (Map.add key v (fst cache), snd cache)
        let findOrAdd2 key mkFunc (cache : FunctionCache) =
            match Map.tryFind key (snd cache) with
            | Some v -> v, cache
            | None ->
                let (v, cache) = mkFunc key cache
                v, (fst cache, Map.add key v (snd cache))

        let rec analyze bindings cache value : NormalResults * FunctionCache =
            let binop op a b =
                let (ra, cache) = analyze bindings cache a |> tmap1 Map.toSeq
                let (rb, cache) = analyze bindings cache b |> tmap1 Map.toSeq
                Seq.allPairs ra rb
                |> Seq.map (fun ((k1, v1), (k2, v2)) -> op (k1, k2), PProd(v1, v2))
                |> reduceSeq // for some fuckin reason not having these early reductions causes it to give the wrong answer
                |> buildMap, cache
            match value with
            | Number n -> Map.add (IntValue n) (Probability 1.) Map.empty, cache
            | Argument i -> Map.add (ArgValue i) (Probability 1.) Map.empty, cache
            | DieValue { size = n } -> seq { for i in 1..n -> IntValue i, Probability (1./(float n)) } |> NormalResults, cache
            | Sum(a, b) -> binop RSum a b
            | Difference(a, b) -> binop RDiff a b
            | Multiply(a, b) -> binop RMul a b
            | Divide(a, b) -> binop RDiv a b
            | Condition(cond, t, f) -> analyzeCond analyze bindings cache cond t f

            | FunctionCall(func, args) ->
                let func, cache = maybeAnalyzeFuncInt cache func
                let args, cache = args |> List.mapFold (analyze bindings) cache
                buildCallInt args func, cache

            | Binding(i, value, expr) ->
                let (value, cache) = analyze bindings cache value
                analyze (Map.add i value bindings) cache expr

            | BoundValue i ->
                // TODO: how do I make this correctly account for each case?
                // ex: if a bound value is somehow tested in a condition, theneach branch
                // should consider only the cases where that bound value is a matching value
                Map.tryFind i bindings |> Option.get, cache

        and analyzeBool bindings cache value : BoolResults * FunctionCache =
            let binop analyze op a b =
                let (ra, cache) = analyze bindings cache a |> tmap1 Map.toSeq
                let (rb, cache) = analyze bindings cache b |> tmap1 Map.toSeq
                Seq.allPairs ra rb
                |> Seq.map (fun ((k1, v1), (k2, v2)) -> op (k1, k2), PProd(v1, v2))
                |> reduceBoolSeq
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
                |> tmap1 reduceBoolSeq
                |> tmap1 buildMap
            | BoolAnd(a, b) -> binop analyzeBool RAnd a b
            | BoolOr(a, b) -> binop analyzeBool ROr a b
            | BoolCondition(cond, t, f) ->
                let result, cache = analyzeBool bindings cache cond
                let result, cache = 
                    result |> Map.toSeq
                    |> reduceBoolSeq
                    |> Seq.mapFold (fun cache (r, v) ->
                            let rt, cache = analyzeBool bindings cache t |> tmap1 Map.toSeq
                            let rf, cache = analyzeBool bindings cache f |> tmap1 Map.toSeq
                            let rt = 
                                rt
                                |> Seq.map (tmap2 (fun v -> PCond(r, v, Probability 0.)))
                                |> Seq.map (tmap2 (fun v2 -> PProd(v, v2)))
                            let rf = 
                                rf
                                |> Seq.map (tmap2 (fun v -> PCond(r, Probability 0., v)))
                                |> Seq.map (tmap2 (fun v2 -> PProd(v, v2)))
                            Seq.append rt rf, cache
                    ) cache
                result |> Seq.concat |> buildMap, cache

            | BoolFunctionCall(func, args) ->
                let func, cache = maybeAnalyzeFuncBool cache func
                let args, cache = args |> List.mapFold (analyze bindings) cache
                buildCallBool args func, cache

            | BoolBinding(i, value, expr) ->
                let (value, cache) = analyze bindings cache value
                analyzeBool (Map.add i value bindings) cache expr

        // TODO: why does this not work in BoolCondition?
        and analyzeCond (analyze:BindingSet->FunctionCache->'a->ProbResults<'b>*FunctionCache) (bindings:BindingSet) (cache:FunctionCache) (cond:BooleanValue) (t:'a) (f:'a) : ProbResults<'b>*FunctionCache =
            let result, cache = analyzeBool bindings cache cond
            let result, cache = 
                result |> Map.toSeq
                |> reduceBoolSeq
                |> Seq.mapFold (fun cache (r, v) ->
                        let rt, cache = analyze bindings cache t |> tmap1 Map.toSeq
                        let rf, cache = analyze bindings cache f |> tmap1 Map.toSeq
                        let rt = 
                            rt
                            |> Seq.map (tmap2 (fun v -> PCond(r, v, Probability 0.)))
                            |> Seq.map (tmap2 (fun v2 -> PProd(v, v2)))
                        let rf = 
                            rf
                            |> Seq.map (tmap2 (fun v -> PCond(r, Probability 0., v)))
                            |> Seq.map (tmap2 (fun v2 -> PProd(v, v2)))
                        Seq.append rt rf, cache
                ) cache
            result |> Seq.concat |> buildMap, cache

        and maybeAnalyzeFuncInt cache (func: Function) =
            findOrAdd1 func (fun func cache ->
                analyze Map.empty cache func.value |> tmap1 reduceResultMap) cache
        and maybeAnalyzeFuncBool cache (func: BoolFunction) =
            findOrAdd2 func (fun func cache ->
                analyzeBool Map.empty cache func.value |> tmap1 reduceBoolResultMap) cache
            
        let processWithName (cache: FunctionCache) name prob =
            let (result, cache) = analyze Map.empty cache prob
            let result =
                result
                |> Map.toSeq
                |> reduceSeq
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
                        (name, r)::sets, cache) ([], Internal.FunctionCache.empty)
        |> fst
        |> Map

    /// Processes a single OutputValue into its output probabilities
    let processOne = processOneImpl Internal.FunctionCache.empty 1 >> fun (name, b, _) -> Map.add name b Map.empty
