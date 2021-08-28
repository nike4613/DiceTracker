
module Processing

open Prob
open FSharp.Data
open FSharp.Collections

type OutputCsv = CsvProvider<HasHeaders = false, Schema = "Name (string),Value (int),Occurences (int),Probability (float)">

module private Internal =
    type EvaluatedState<'v> = 
        { dieValues: Map<Die, int>
        ; realValue: 'v }

    let stateWithValue value = { dieValues = Map.empty; realValue = value }
    let copyWithValue state value = { dieValues = state.dieValues ; realValue = value }
    
    let modify op s = copyWithValue s (op s.realValue)

    let matchingDieValueFilter (a, b) = 
        a.dieValues
        |> Map.toSeq
        |> Seq.forall (fun (k, v) -> 
            match Map.tryFind k b.dieValues with
            | Some(bv) -> v = bv
            | None -> true)

    let mergeStates mergeVal (a, b) =
        { dieValues = Seq.concat [a.dieValues |> Map.toSeq ; b.dieValues |> Map.toSeq]
                        |> Seq.distinctBy fst
                        |> Map
        ; realValue = mergeVal a.realValue b.realValue }
        
    let binop eval op a b = 
        Seq.allPairs (eval a) (eval b) 
        |> Seq.filter matchingDieValueFilter
        |> Seq.map (mergeStates op)

    let mapModify op i = Seq.map (modify (fun v -> op v i))

    let evalCondition evalv a b conds =
        conds
        |> Seq.map (fun s -> s, if s.realValue then evalv a else evalv b)
        |> Seq.map (fun (s1, b) -> b |> Seq.map (fun s2 -> s1, s2))
        |> Seq.concat
        |> Seq.filter matchingDieValueFilter
        |> Seq.map (mergeStates (fun _ b -> b))

    type CachingEvaluator() =
        let isCache = new System.Collections.Concurrent.ConcurrentDictionary<ProbabilityValue, EvaluatedState<int> seq>()
        let bsCache = new System.Collections.Concurrent.ConcurrentDictionary<BooleanValue, EvaluatedState<bool> seq>()
        member _.GetOrAddProb value func = isCache.GetOrAdd(value, new System.Func<ProbabilityValue, EvaluatedState<int> seq>(func))
        member _.GetOrAddBool value func = bsCache.GetOrAdd(value, new System.Func<BooleanValue, EvaluatedState<bool> seq>(func))

        member this.evaluateStates value =
            match value with
            | Number n -> stateWithValue n |> Seq.singleton
            | DieValue d -> seq { for i in 1..d.Size -> { dieValues = Map.add d i Map.empty; realValue = i } }
            | Sum(a, b) -> binop this.evaluateStatesC (+) a b
            | Difference(a, b) -> binop this.evaluateStatesC (-) a b
            | Multiply(a, b) -> binop this.evaluateStatesC (*) a b
            | Divide(a, b) -> binop this.evaluateStatesC (/) a b
            | Condition(cond, a, b) -> this.evaluateBoolsC cond |> evalCondition this.evaluateStatesC a b
        member this.evaluateBools value =
            match value with
            | Literal b -> stateWithValue b |> Seq.singleton
            | Equals(v, i) -> this.evaluateStatesC v |> mapModify (=) i
            | NotEquals(v, i) -> this.evaluateStatesC v |> mapModify (<>) i
            | GreaterThan(v, i) -> this.evaluateStatesC v |> mapModify (>) i
            | LessThan(v, i) -> this.evaluateStatesC v |> mapModify (<) i
            | GreaterThanEqual(v, i) -> this.evaluateStatesC v |> mapModify (>=) i
            | LessThanEqual(v, i) -> this.evaluateStatesC v |> mapModify (<=) i
            | BoolNot v -> this.evaluateBoolsC v |> Seq.map (modify not)
            | BoolAnd(a, b) -> binop this.evaluateBoolsC (&&) a b
            | BoolOr(a, b) -> binop this.evaluateBoolsC (||) a b
            | BoolCondition(cond, a, b) -> this.evaluateBoolsC cond |> evalCondition this.evaluateBoolsC a b

        member this.evaluateStatesC value = this.GetOrAddProb value (this.evaluateStates >> Seq.cache)
        member this.evaluateBoolsC value = this.GetOrAddBool value (this.evaluateBools >> Seq.cache)

    let processWithName (cache: CachingEvaluator) name prob : OutputCsv.Row seq =
        let values = 
            cache.evaluateStatesC prob
            |> Seq.map (fun s -> s.realValue) 
            |> Seq.fold (fun m v -> Map.change v (Option.orElse (Some 0) >> Option.map ((+) 1)) m) Map.empty
        let total = Map.toSeq values |> Seq.sumBy snd
        Map.toSeq values
        |> Seq.map (fun (v, c) -> OutputCsv.Row(name, v, c, (float c) / (float total)))


let private makeCsv data = new OutputCsv(data)

let private processOneImpl cache i (data: OutputValue) =
    match data with
    | NamedOutput(name, value) -> Internal.processWithName cache name value
    | UnnamedOutput(value) -> Internal.processWithName cache (sprintf "output %o" i) value

let private processManyImpl (data: OutputValue seq) =
    data |> Seq.mapi (processOneImpl (Internal.CachingEvaluator ())) |> Seq.concat

let processOne = processOneImpl (Internal.CachingEvaluator ()) 1 >> makeCsv
let processMany : OutputValue seq -> OutputCsv = processManyImpl >> makeCsv
