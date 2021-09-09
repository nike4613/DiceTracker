
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
            | RBinding of index:int

        module ProbabilityResultResult =
            type Self = ProbabilityResultResult
            [<RequireQualifiedAccess>]
            type UnpackResult = 
                | None of Self
                | Binary of {| ctor: Self*Self -> Self; values: Self*Self |}
            let (|Unpack|) res =
                match res with
                | IntValue _
                | RBinding _
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
            | Probability of decimal
            | PSum of ProbabilityResultValue * ProbabilityResultValue
            | PProd of ProbabilityResultValue * ProbabilityResultValue
            | PRebase of value:ProbabilityResultValue * numerator:int * denominator:int
            | PCond of BoolResultResult * ProbabilityResultValue * ProbabilityResultValue

        module ProbabilityResultValue = 
            type Self = ProbabilityResultValue
            [<RequireQualifiedAccess>]
            type UnpackResult =
                | None of Self
                | Binary of {| ctor: Self*Self -> Self ; values: Self*Self |}
                | Rebase of Self*int*int
                | Cond of BoolResultResult * Self * Self
            let (|Unpack|) res =
                match res with
                | Probability _ -> UnpackResult.None res
                | PSum(a, b) -> UnpackResult.Binary {| ctor = PSum ; values = a,b |}
                | PProd(a, b) -> UnpackResult.Binary {| ctor = PProd ; values = a,b |}
                | PRebase(v, n, d) -> UnpackResult.Rebase(v, n, d)
                | PCond(c, t, f) -> UnpackResult.Cond(c, t, f)
            let repack bmap rmap result =
                match result with
                | UnpackResult.None v -> v
                | UnpackResult.Binary dc -> dc.values |> tmap rmap rmap |> dc.ctor
                | UnpackResult.Rebase(v, n, d) -> PRebase(rmap v, n, d)
                | UnpackResult.Cond(c, t, f) -> PCond(bmap c, rmap t, rmap f)

        module Prob =
            let always = Probability 1.m
            let never = Probability 0.m

        type BindingSet = Map<int, ProbabilityResultResult>
        type ProbResults<'a when 'a : comparison> = Map<'a * BindingSet, ProbabilityResultValue>
        type CompleteProbResults<'a when 'a : comparison> = Map<'a, ProbabilityResultValue>
        type UnboundNormalResults = ProbResults<ProbabilityResultResult>
        type BoundNormalResults = CompleteProbResults<ProbabilityResultResult>
        type UnboundBoolResults = ProbResults<BoolResultResult>
        type BoundBoolResults = CompleteProbResults<BoolResultResult>
    
        let rec tryGetValueRInt res =
            let binop op a b = Option.map2 op (tryGetValueRInt a) (tryGetValueRInt b)
            match res with
            | IntValue v -> Some v
            | RSum(a, b) -> binop (+) a b
            | RDiff(a, b) -> binop (-) a b
            | RMul(a, b) -> binop (*) a b
            | RDiv(a, b) -> binop (/) a b
            | ArgValue _ -> None
            | RBinding _ -> None // we specifically need to do late binding here

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
            
        let reduceBindings binds =
            binds |> Map.map (fun _ v -> reduceResult v)
            
        let reduceUnboundResult (res, binds) =
            reduceResult res,
            reduceBindings binds

        let reduceUnboundBoolResult (res, binds) =
            reduceBoolResult res,
            reduceBindings binds

        let rec reduceProb prob =
            let rec tryGetValue p =
                let binop op a b = Option.map2 op (tryGetValue a) (tryGetValue b)
                match p with
                | Probability v -> Some v
                | PSum(a, b) -> binop (+) a b
                | PProd(a, b) -> binop (*) a b
                | PRebase(v, n, d) -> tryGetValue v |> Option.map (fun v -> v * (decimal n) / (decimal d))
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
                | _, Probability 0.m -> false
                | _, _ -> true)

        let reduceBoundSeq seq = seq |> Seq.map (tmap reduceResult reduceProb) |> filterImpossible
        let reduceBoundBoolSeq seq = seq |> Seq.map (tmap reduceBoolResult reduceProb) |> filterImpossible

        let reduceUnboundSeq seq = seq |> Seq.map (tmap reduceUnboundResult reduceProb) |> filterImpossible
        let reduceUnboundBoolSeq seq = seq |> Seq.map (tmap reduceUnboundBoolResult reduceProb) |> filterImpossible

        let rec bindResultImpl binds res =
            match res with
            | RBinding i -> Map.find i binds
            | ProbabilityResultResult.Unpack r -> ProbabilityResultResult.repack (bindResultImpl binds) r

        let rec bindBoolResultImpl binds (BoolResultResult.Unpack r) =
            BoolResultResult.repack (bindResultImpl binds) (bindBoolResultImpl binds) r

        let rec bindProbImpl binds (ProbabilityResultValue.Unpack r) =
            ProbabilityResultValue.repack (bindBoolResultImpl binds) (bindProbImpl binds) r

        let bindBindings (binds: BindingSet) : BindingSet =
            binds
            |> Map.toSeq
            |> Seq.fold (fun m (i, v) -> Map.add i (bindResultImpl m v) m) Map.empty

        let bindResult ((r, binds), v) =
            let binds = bindBindings binds
            bindResultImpl binds r, bindProbImpl binds v

        let bindBoolResult ((r, binds), v) =
            let binds = bindBindings binds
            bindBoolResultImpl binds r, bindProbImpl binds v

        let bindSeq seq = seq |> Seq.map bindResult
        let bindBoolSeq seq = seq |> Seq.map bindBoolResult

        let rebuildMap f = Map.toSeq >> f >> buildMap

        let reduceBoundResultMap = rebuildMap reduceBoundSeq
        let reduceBoundBoolResultMap = rebuildMap reduceBoundBoolSeq

        let bindResultMap : UnboundNormalResults -> BoundNormalResults = rebuildMap bindSeq
        let bindBoolResultMap : UnboundBoolResults -> BoundBoolResults = rebuildMap bindBoolSeq

        let bindReduceMap = rebuildMap (bindSeq >> reduceBoundSeq)
        let bindReduceBoolMap = rebuildMap (bindBoolSeq >> reduceBoundBoolSeq)

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

        let rec buildCallFixBool (args: _ list) (BoolResultResult.Unpack r) =
            BoolResultResult.repack (buildCallFixInt args) (buildCallFixBool args) r

        let rec buildCallFixProb (args: _ list) (ProbabilityResultValue.Unpack r) =
            ProbabilityResultValue.repack (buildCallFixBool args) (buildCallFixProb args) r
            
        // Returns true if all elements in each of the binding lists are equivalent.
        let checkBindingListMapMatch map =
            Map.forall (fun _ ->
                List.fold
                    (fun (b, p) v ->
                        b && (p |> Option.map (fun p -> reduceResult p = reduceResult v) |> Option.defaultValue true), Some v)
                    (true, None) >> fst) map

        let foldSeqToMapDuplicated seq =
            Seq.fold (fun m (i, v) ->
                Map.change i (fun vs -> v::(Option.defaultValue [] vs) |> Some) m) Map.empty seq

        // Selects only the first element in a binding list.
        let selectRealBindings map =
            Map.map (fun _ -> List.head >> reduceResult) map

        let mergeBindingsDuplicated b1 b2 =
            Seq.append (Map.toSeq b1) (Map.toSeq b2)
            |> foldSeqToMapDuplicated

        let bindingsMatch b1 b2 =
            mergeBindingsDuplicated b1 b2
            |> checkBindingListMapMatch
            
        let probCombine a b = PProd(a, b)

        let buildCallImpl fixImpl (args: UnboundNormalResults list) funcVal =
            // Collects the bindings for each of the arguments into a map of int->(binding list)
            let collectArgBindings args =
                let joinedMap =
                    args
                    |> Seq.collect (fun ((_, b), _) -> b |> Map.toSeq)
                    |> foldSeqToMapDuplicated
                args |> Seq.map (fun ((k, _), v) -> k,v), joinedMap

            let args = 
                args 
                |> Seq.map Map.toSeq 
                // Get the cartesian product of the arguments; so one element for each possible combination
                |> cartProd 
                // Collect the arguments' bindings
                |> Seq.map collectArgBindings
                // Filter to only arguments with matching bindings
                |> Seq.filter (fun (_, m) -> checkBindingListMapMatch m)
                // Materialize the actual value list and select a single binding value
                |> Seq.map (tmap Seq.toList selectRealBindings)
                |> Seq.cache
            funcVal
            |> Map.toSeq
            // Cartesian product with the possible argument values
            |> Seq.allPairs args
            // Remap stuff to be more easily used by fixImpl and buildCallFixProb
            |> Seq.map (fun ((args, binds), (value, prob)) -> args |> List.map fst, value, List.fold (fun a b -> probCombine a (snd b)) prob args, binds)
            // Perform the actual fixups, returning an actual set of possibilities
            |> Seq.map (fun (args, value, prob, binds) -> (fixImpl args value, binds), buildCallFixProb args prob)
            |> buildMap

        let buildCallInt (args: UnboundNormalResults list) (funcVal: BoundNormalResults) : UnboundNormalResults =
            buildCallImpl buildCallFixInt args funcVal

        let buildCallBool (args: UnboundNormalResults list) (funcVal: BoundBoolResults) : UnboundBoolResults =
            buildCallImpl buildCallFixBool args funcVal

        let analyzeBinop analyze reducer cache op a b =
            let (ra, cache) = analyze cache a |> tmap1 Map.toSeq
            let (rb, cache) = analyze cache b |> tmap1 Map.toSeq
            Seq.allPairs ra rb
            |> Seq.map (fun (((k1, b1), v1), ((k2, b2), v2)) -> (k1, v1), (k2, v2), b1, b2)
            |> Seq.filter (fun (_, _, a, b) -> bindingsMatch a b)
            |> Seq.map (fun ((k1, v1), (k2, v2), b, _) -> (op (k1, k2), b), probCombine v1 v2)
            |> reducer
            |> buildMap, cache

        (*
                For correct condition analysis, we need to be able to notice restrictions to possible values of bindings.
                This ends up effectively requiring the same kind of algorithm that eg. the C# compiler uses to determine that
            a value is not null in a certain piece of code because of a surrounding check. As far as I know though, all such
            algorithms are falliable, and requires, in some cases, the programmer to insert explicit assertions that such a
            thing is the case. For us, such failures lead to an incorrect result, and as such are not permissible.
                Therefore, I think, we need to keep track of all restrictions on all conditionals that surround the current
            block (that contain binding references) so that we can determine binding value restrictions when resolving them.
            This information *also* needs to propagate to bindings referencing other bindings, and so bindings need to actually
            be resolved *as late as possible*. This means, for functions, the end of the core function evaluation, and for top-
            level expressions, the end of evaluation of that expression.
        *)

        let analyzeCond analyze analyzeBool cache cond t f =
            let onlyIfTrue cond v = PCond(cond, v, Prob.never)
            let onlyIfFalse cond v = PCond(cond, Prob.never, v)
            let result, cache = analyzeBool cache cond
            let result, cache = 
                result |> Map.toSeq
                |> reduceUnboundBoolSeq
                |> Seq.mapFold (fun cache ((r, b), v) ->
                    let rt, cache = analyze cache t |> tmap1 Map.toSeq
                    let rf, cache = analyze cache f |> tmap1 Map.toSeq
                    let rt = 
                        rt
                        |> Seq.map (tmap2 (onlyIfTrue r))
                        |> Seq.map (tmap2 (probCombine v))
                    let rf = 
                        rf
                        |> Seq.map (tmap2 (onlyIfFalse r))
                        |> Seq.map (tmap2 (probCombine v))
                    Seq.append rt rf, cache
                ) cache
            result |> Seq.concat |> buildMap, cache

        let analyzeBinding analyze analyzeBody cache i value expr =
            let value, cache = analyze cache value
            let result, cache = analyzeBody cache expr

            Seq.allPairs (Map.toSeq value) (Map.toSeq result)
            // first is binding, then is result
            |> Seq.map (fun (((rb, bb), pb), ((rr, br), pr)) -> (rb, pb), (rr, pr), mergeBindingsDuplicated bb br)
            |> Seq.filter (fun (_, _, m) -> checkBindingListMapMatch m)
            |> Seq.map (fun (a, b, m) -> a, b, selectRealBindings m)
            // at this point the 3rd item is a merged binding list, now we just need to add 1st to it
            |> Seq.map (fun ((rb, pb), (rr, pr), bind) -> rr, probCombine pb pr, Map.add i rb bind)
            // rearrange into the correct shape, then we're good
            |> Seq.map (fun (rr, p, bind) -> (rr, bind), p)
            |> buildMap, cache

        type FunctionCache = Map<Function, BoundNormalResults> * Map<BoolFunction, BoundBoolResults>
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

        // TODO: do we even need the bindings argument now?
        let rec analyze cache value : UnboundNormalResults * FunctionCache =
            let binop = analyzeBinop analyze reduceUnboundSeq cache
            match value with
            | Number n -> Map.add (IntValue n, Map.empty) Prob.always Map.empty, cache
            | Argument i -> Map.add (ArgValue i, Map.empty) Prob.always Map.empty, cache
            | DieValue { size = n } -> seq { for i in 1..n -> (IntValue i, Map.empty), Probability (1.m/(decimal n)) } |> UnboundNormalResults, cache
            | Sum(a, b) -> binop RSum a b
            | Difference(a, b) -> binop RDiff a b
            | Multiply(a, b) -> binop RMul a b
            | Divide(a, b) -> binop RDiv a b
            | Condition(cond, t, f) -> analyzeCond analyze analyzeBool cache cond t f

            | FunctionCall(func, args) ->
                let func, cache = maybeAnalyzeFuncInt cache func
                let args, cache = args |> List.mapFold analyze cache
                buildCallInt args func, cache

            | Binding(i, value, expr) -> analyzeBinding analyze analyze cache i value expr

            | BoundValue i -> Map.add (RBinding i, Map.empty) Prob.always Map.empty, cache

        and analyzeBool cache value : UnboundBoolResults * FunctionCache =
            let binop analyze = analyzeBinop analyze reduceUnboundBoolSeq cache
            match value with
            | Literal b -> Map.add (BoolValue b, Map.empty) Prob.always Map.empty, cache
            | Equals(a, b) -> binop analyze REquals a b
            | NotEquals(a, b) -> binop analyze RNEquals a b
            | GreaterThan(a, b) -> binop analyze RGt a b
            | LessThan(a, b) -> binop analyze RLt a b
            | GreaterThanEqual(a, b) -> binop analyze RGte a b
            | LessThanEqual(a, b) -> binop analyze RLte a b
            | BoolNot b ->
                analyzeBool cache b 
                |> tmap1 Map.toSeq
                |> tmap1 (Seq.map (fun ((k, b), v) -> (RNot k, b), v))
                |> tmap1 reduceUnboundBoolSeq
                |> tmap1 buildMap
            | BoolAnd(a, b) -> binop analyzeBool RAnd a b
            | BoolOr(a, b) -> binop analyzeBool ROr a b
            | BoolCondition(cond, t, f) -> analyzeCond analyzeBool analyzeBool cache cond t f

            | BoolFunctionCall(func, args) ->
                let func, cache = maybeAnalyzeFuncBool cache func
                let args, cache = args |> List.mapFold analyze cache
                buildCallBool args func, cache

            | BoolBinding(i, value, expr) -> analyzeBinding analyze analyzeBool cache i value expr

        and maybeAnalyzeFuncInt cache (func: Function) =
            findOrAdd1 func (fun func cache ->
                analyze cache func.value |> tmap1 bindReduceMap) cache
        and maybeAnalyzeFuncBool cache (func: BoolFunction) =
            findOrAdd2 func (fun func cache ->
                analyzeBool cache func.value |> tmap1 bindReduceBoolMap) cache
            
        let processWithName (cache: FunctionCache) name prob =
            let (result, cache) = analyze cache prob
            let result =
                result
                |> Map.toSeq
                |> bindSeq
                |> reduceBoundSeq
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
