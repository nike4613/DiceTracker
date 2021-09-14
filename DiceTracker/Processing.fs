
namespace DiceTracker

module Processing =

    open FSharp.Collections
    open MathNet.Numerics

    module private Internal =

        // A couple of definitions so that we can allocate less
        type RefOption<'a> = Option<'a>
        type Option<'a> = ValueOption<'a>
        type 'a roption = RefOption<'a>
        type 'a option = Option<'a>

        module RefOption = Option
        module Option = ValueOption

        [<Struct>]
        type ValOrRef<'a, 'b> =
            | Val of value:'a
            | Ref of reference:'b
        module ValOrRef =
            let inline unify vtor valOrRef =
                match valOrRef with
                | Val a -> vtor a
                | Ref a -> a

        let inline Some x = ValueSome x
        let None = ValueNone

        let inline ctor2 c a b = c(a, b)

        let inline tmap f1 f2 (a, b) = (f1 a, f2 b)
        let inline tmap1 f (a, b) = (f a, b)
        let inline tmap2 f (a, b) = (a, f b)

        module Seq =
            let ofTuple (a, b) = seq { yield a; yield b }
            let inline (|Empty|_|) seq = if Seq.isEmpty seq then RefOption.Some() else RefOption.None

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
            [<Struct>]
            type UnpackResult = 
                | None of Self
                | Binary of ctor:(Self->Self->Self) * val1:Self * val2:Self
            let (|Unpack|) res : UnpackResult =
                match res with
                | IntValue _
                | RBinding _
                | ArgValue _ -> UnpackResult.None res
                | RSum(a, b) -> UnpackResult.Binary(ctor2 RSum, a, b)
                | RDiff(a, b) -> UnpackResult.Binary(ctor2 RDiff, a, b)
                | RMul(a, b) -> UnpackResult.Binary(ctor2 RMul, a, b)
                | RDiv(a, b) -> UnpackResult.Binary(ctor2 RDiv, a, b)
            let repack map (result: UnpackResult) =
                match result with
                | UnpackResult.None v -> v
                | UnpackResult.Binary(ctor, v1, v2) -> ctor (map v1) (map v2)

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
            type Unpacked =
                | Unary of ctor:(Self -> Self) * value:Self
                | SelfBinary of ctor:(Self->Self->Self) * val1:Self * val2:Self
                | OtherBinary of ctor:(Other->Other->Self) * val1:Other * val2:Other
            type UnpackResult = ValOrRef<Self, Unpacked>
            let (|Unpack|) res : UnpackResult =
                match res with
                | BoolValue _ -> Val res
                | RNot v -> Unpacked.Unary(RNot, v) |> Ref
                | RAnd(a, b) -> Unpacked.SelfBinary(ctor2 RAnd, a, b) |> Ref
                | ROr(a, b) -> Unpacked.SelfBinary(ctor2 ROr, a, b) |> Ref
                | REquals(a, b) -> Unpacked.OtherBinary(ctor2 REquals, a, b) |> Ref
                | RNEquals(a, b) -> Unpacked.OtherBinary(ctor2 RNEquals, a, b) |> Ref
                | RGt(a, b) -> Unpacked.OtherBinary(ctor2 RGt, a, b) |> Ref
                | RLt(a, b) -> Unpacked.OtherBinary(ctor2 RLt, a, b) |> Ref
                | RGte(a, b) -> Unpacked.OtherBinary(ctor2 RGte, a, b) |> Ref
                | RLte(a, b) -> Unpacked.OtherBinary(ctor2 RLte, a, b) |> Ref
            let repack imap bmap (result: UnpackResult) = 
                match result with
                | Val v -> v
                | Ref(Unpacked.Unary(c, v)) -> bmap v |> c
                | Ref(Unpacked.SelfBinary(c, a, b)) -> c (bmap a) (bmap b)
                | Ref(Unpacked.OtherBinary(c, a, b)) -> c (imap a) (imap b)

        type ProbabilityResultValue =
            | Probability of BigRational
            | PSum of ProbabilityResultValue * ProbabilityResultValue
            | PProd of ProbabilityResultValue * ProbabilityResultValue
            | PCond of BoolResultResult * ProbabilityResultValue * ProbabilityResultValue

        module ProbabilityResultValue = 
            type Self = ProbabilityResultValue
            [<RequireQualifiedAccess>]
            type Unpacked =
                | Binary of ctor:(Self->Self->Self) * val1:Self * val2:Self
                | Cond of BoolResultResult * Self * Self
            type UnpackResult = ValOrRef<Self, Unpacked>
            let (|Unpack|) res : UnpackResult =
                match res with
                | Probability _ -> Val res
                | PSum(a, b) -> Unpacked.Binary(ctor2 PSum, a, b) |> Ref
                | PProd(a, b) -> Unpacked.Binary(ctor2 PProd, a, b) |> Ref
                | PCond(c, t, f) -> Unpacked.Cond(c, t, f) |> Ref
            let repack bmap rmap (result: UnpackResult) =
                match result with
                | Val v -> v
                | Ref(Unpacked.Binary(c, a, b)) -> c (rmap a) (rmap b)
                | Ref(Unpacked.Cond(c, t, f)) -> PCond(bmap c, rmap t, rmap f)

        module Prob =
            let always = Probability BigRational.One
            let never = Probability BigRational.Zero

        type BindingSet = Map<int, ProbabilityResultResult>
        type ProbResults<'a when 'a : comparison> = Map<'a * BindingSet, ProbabilityResultValue>
        type CompleteProbResults<'a when 'a : comparison> = Map<'a, ProbabilityResultValue>
        type UnboundNormalResults = ProbResults<ProbabilityResultResult>
        type BoundNormalResults = CompleteProbResults<ProbabilityResultResult>
        type UnboundBoolResults = ProbResults<BoolResultResult>
        type BoundBoolResults = CompleteProbResults<BoolResultResult>
    
        let rec reduceResultImpl res =
            let inline binop ctor op a b =
                match reduceResultImpl a, reduceResultImpl b with
                | Val a, Val b -> Val (op a b)
                | a, b -> ctor (ValOrRef.unify IntValue a) (ValOrRef.unify IntValue b) |> Ref
            match res with
            | IntValue i -> Val i
            | ArgValue _
            | RBinding _ -> Ref res
            | RSum(a, b) -> binop (ctor2 RSum) (+) a b
            | RDiff(a, b) -> binop (ctor2 RDiff) (-) a b
            | RMul(a, b) -> binop (ctor2 RMul) (*) a b
            | RDiv(a, b) -> binop (ctor2 RDiv) (/) a b

        let reduceResult res =
            reduceResultImpl res |> ValOrRef.unify IntValue

        let rec reduceBoolResultImpl res =
            let inline mapToCtor vtor ctor a b =
                ctor (ValOrRef.unify vtor a) (ValOrRef.unify vtor b) |> Ref
            let inline binopi ctor op a b =
                match reduceResultImpl a, reduceResultImpl b with
                | Val a, Val b -> op a b |> Val
                | a, b -> mapToCtor IntValue ctor a b
            let inline binopb ctor op a b =
                match reduceBoolResultImpl a, reduceBoolResultImpl b with
                | Val a, Val b -> op a b |> Val
                | a, b -> mapToCtor BoolValue ctor a b
            match res with
            | BoolValue b -> Val b
            | REquals(a, b) -> binopi (ctor2 REquals) (=) a b
            | RNEquals(a, b) -> binopi (ctor2 RNEquals) (<>) a b
            | RGt(a, b) -> binopi (ctor2 RGt) (>) a b
            | RLt(a, b) -> binopi (ctor2 RLt) (<) a b
            | RGte(a, b) -> binopi (ctor2 RGte) (>=) a b
            | RLte(a, b) -> binopi (ctor2 RLte) (<=) a b
            | RAnd(a, b) -> binopb (ctor2 RAnd) (&&) a b
            | ROr(a, b) -> binopb (ctor2 ROr) (||) a b
            | RNot(REquals(a, b)) -> RNEquals(a, b) |> reduceBoolResultImpl
            | RNot(RNEquals(a, b)) -> REquals(a, b) |> reduceBoolResultImpl
            | RNot(RGt(a, b)) -> RLte(a, b) |> reduceBoolResultImpl
            | RNot(RLt(a, b)) -> RGte(a, b) |> reduceBoolResultImpl
            | RNot(RGte(a, b)) -> RLt(a, b) |> reduceBoolResultImpl
            | RNot(RLte(a, b)) -> RGt(a, b) |> reduceBoolResultImpl
            | RNot(RNot(a)) -> a |> reduceBoolResultImpl
            | RNot v -> 
                match reduceBoolResultImpl v with
                | Val b -> not b |> Val
                | Ref a -> RNot a |> Ref

        let reduceBoolResult res =
            reduceBoolResultImpl res |> ValOrRef.unify BoolValue

        let reduceBindings binds =
            binds |> Map.map (fun _ v -> reduceResult v)
            
        let reduceUnboundResult (res, binds) =
            reduceResult res,
            reduceBindings binds

        let reduceUnboundBoolResult (res, binds) =
            reduceBoolResult res,
            reduceBindings binds

        let rec reduceProbImpl prob =
            let inline binop ctor op a b =
                match reduceProbImpl a, reduceProbImpl b with
                | Val a, Val b -> Val (op a b)
                | a, b -> ctor (ValOrRef.unify Probability a) (ValOrRef.unify Probability b) |> Ref
            match prob with
            | Probability p -> Val p
            | PSum(a, b) -> binop (ctor2 PSum) (+) a b
            | PProd(a, b) -> binop (ctor2 PProd) (*) a b
            | PCond(c, t, f) ->
                match reduceBoolResultImpl c with
                | Val b -> if b then reduceProbImpl t else reduceProbImpl f
                | Ref c -> PCond(c, reduceProbImpl t |> ValOrRef.unify Probability, reduceProbImpl f |> ValOrRef.unify Probability) |> Ref

        let reduceProb prob =
            reduceProbImpl prob |> ValOrRef.unify Probability

        let buildMap seq = Seq.fold (fun m (k, v) -> Map.change k (fun o -> RefOption.Some(match o with | RefOption.Some o -> PSum(v, o) | RefOption.None -> v)) m) Map.empty seq

        let filterImpossible seq =
            seq |> Seq.filter (function
                | _, Probability r when r.IsZero -> false
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
        let inline checkBindingListMapMatch map =
            Map.forall (fun _ ->
                List.fold
                    (fun (b, p) v ->
                        b && (p |> Option.map (fun p -> reduceResult p = reduceResult v) |> Option.defaultValue true), Some v)
                    (true, None) >> fst) map

        let inline foldSeqToMapDuplicated seq =
            Seq.fold (fun m (i, v) ->
                Map.change i (fun vs -> v::(RefOption.defaultValue [] vs) |> RefOption.Some) m) Map.empty seq

        // Selects only the first element in a binding list.
        let inline selectRealBindings map =
            Map.map (fun _ -> List.head >> reduceResult) map

        let inline mergeBindingsDuplicated b1 b2 =
            Seq.append (Map.toSeq b1) (Map.toSeq b2)
            |> foldSeqToMapDuplicated

        let inline bindingsMatch b1 b2 =
            mergeBindingsDuplicated b1 b2
            |> checkBindingListMapMatch
            
        let inline probCombine a b = PProd(a, b)

        let inline buildCallImpl fixImpl (args: UnboundNormalResults list) funcVal =
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

        let inline analyzeBinop analyze reducer cache op a b =
            let (ra, cache) = analyze cache a |> tmap1 Map.toSeq
            let (rb, cache) = analyze cache b |> tmap1 Map.toSeq
            Seq.allPairs ra rb
            |> Seq.map (fun (((k1, b1), v1), ((k2, b2), v2)) -> (k1, v1), (k2, v2), mergeBindingsDuplicated b1 b2)
            |> Seq.filter (fun (_, _, m) -> checkBindingListMapMatch m)
            |> Seq.map (fun ((k1, v1), (k2, v2), m) -> (op (k1, k2), selectRealBindings m), probCombine v1 v2)
            |> reducer
            |> buildMap, cache

        let inline analyzeCond analyze analyzeBool cache cond t f =
            let onlyIfTrue cond v = PCond(cond, v, Prob.never)
            let onlyIfFalse cond v = PCond(cond, Prob.never, v)
            let getCondRes onlyFun r b v seq =
                seq
                |> Seq.map (fun ((k, b1), v) -> k, onlyFun r v, mergeBindingsDuplicated b b1)
                |> Seq.filter (fun (_, _, m) -> checkBindingListMapMatch m)
                |> Seq.map (fun (k, v2, m) -> (k, selectRealBindings m), probCombine v v2)
            let result, cache = analyzeBool cache cond
            let result, cache = 
                result |> Map.toSeq
                |> reduceUnboundBoolSeq
                |> Seq.mapFold (fun cache ((r, b), v) ->
                    let rt, cache = analyze cache t |> tmap1 Map.toSeq
                    let rf, cache = analyze cache f |> tmap1 Map.toSeq
                    let rt = getCondRes onlyIfTrue r b v rt
                    let rf = getCondRes onlyIfFalse r b v rf
                    Seq.append rt rf, cache
                ) cache
            result |> Seq.concat |> buildMap, cache

        let inline analyzeBinding analyze analyzeBody cache i value expr =
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
            let inline binop op a b = analyzeBinop analyze reduceUnboundSeq cache op a b
            match value with
            | Number n -> Map.add (IntValue n, Map.empty) Prob.always Map.empty, cache
            | Argument i -> Map.add (ArgValue i, Map.empty) Prob.always Map.empty, cache
            | DieValue { size = n } -> seq { for i in 1..n -> (IntValue i, Map.empty), Probability (BigRational.FromIntFraction(1, n)) } |> UnboundNormalResults, cache
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
            let inline binop analyze = analyzeBinop analyze reduceUnboundBoolSeq cache
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
    let processOne = processOneImpl Internal.FunctionCache.empty 0 >> fun (name, b, _) -> Map.add name b Map.empty
