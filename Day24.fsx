#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = Helpers.Web.getInput 24

let vals, ops =
    let idx = data |> Array.findIndex ((=)"")
    data[..idx-1], data[idx+1..]

type Op =
    | AND of string*string
    | OR  of string*string
    | XOR of string*string

let opsMap =
    ops
    |> Array.map (fun o ->
        match o with
        | Helpers.Regex "(.+) AND (.+) -> (.+)" [op1;op2;res]
            -> res, AND (op1,op2)
        | Helpers.Regex "(.+) XOR (.+) -> (.+)" [op1;op2;res]
            -> res, XOR (op1,op2)
        | Helpers.Regex "(.+) OR (.+) -> (.+)" [op1;op2;res]
            -> res, OR (op1,op2)
        )
    |> Map.ofArray

let zVars =
    opsMap
    |> Map.toArray
    |> Array.map fst
    |> Array.filter (fun s -> s.StartsWith("z"))

let valMap =
    vals
    |> Array.map (function | Helpers.Regex "(.+): (\d+)" [s;v] -> s,int v)
    |> Map.ofArray

let containsBothSides (valMap: Map<string,int>) op =
    match op with
    | AND (s1,s2) -> valMap.ContainsKey(s1) && valMap.ContainsKey(s2)
    | OR (s1,s2) -> valMap.ContainsKey(s1) && valMap.ContainsKey(s2)
    | XOR (s1,s2) -> valMap.ContainsKey(s1) && valMap.ContainsKey(s2)

let calc (opsMap: Map<string,Op>) (valMap: Map<string,int>) =
    let rec f (m: Map<string,int>) =
        let opOpt =
            opsMap
            |> Map.tryFindKey (fun k v -> containsBothSides m v && (m.ContainsKey(k) |> not))
        match opOpt with
        | Some op ->
            match opsMap[op] with
            | AND (s1,s2) ->
                m
                |> Map.add op (m[s1] &&& m[s2])
                |> f
            | OR (s1,s2) ->
                m
                |> Map.add op (m[s1] ||| m[s2])
                |> f
            | XOR (s1,s2) ->
                m
                |> Map.add op (m[s1] ^^^ m[s2])
                |> f
        | None -> m

    f valMap

let varsToBits (resMap: Map<string,int>) vars =
    vars
    |> Array.map (fun x -> if resMap[x] = 1 then true else false)
    
let createFromBits (bits: bool array) =
    bits
    |> Array.mapi (fun i bit -> if bit then 1L <<< i else 0L)
    |> Array.reduce (|||)

let resMap = calc opsMap valMap

let ans1 =
    zVars |> varsToBits resMap |> createFromBits 

ans1

/// Part 2

open System.Collections.Generic

let xVars = seq { for i in 0..44 do yield sprintf "x%02i" i } |> Array.ofSeq
let yVars = seq { for i in 0..44 do yield sprintf "y%02i" i } |> Array.ofSeq

let numToBits (n: int64) =
    [| for i in 0 .. 45 -> (n >>> i) &&& 1L = 1L |]

let firstBadIdx valMap opsMap =
    let resMap = calc opsMap valMap
    let expected =
        let x = varsToBits resMap xVars |> createFromBits
        let y = varsToBits resMap yVars |> createFromBits
        x + y
        |> numToBits

    let actual = varsToBits resMap zVars

    [|0..45|]
    |> Array.tryFind (fun i -> expected[i] <> actual[i])

let opDeps = 
    let opDeps' (op: Op) =
        match op with
        | AND (v1,v2) -> [|v1;v2|]
        | OR (v1,v2)  -> [|v1;v2|]
        | XOR (v1,v2) -> [|v1;v2|]

    Helpers.memoize opDeps'

let findDependencies var (opsMap: Map<string,Op>) =
    let rec f var =
        match Map.tryFind var opsMap with
        | Some op ->
            let deps = opDeps op
            Array.append deps (Array.append (f deps[0]) (f deps[1]))
            |> Array.filter (fun s -> (s.StartsWith("x") || s.StartsWith("y")) |> not)
        | None -> [||]

    f var
    |> Array.distinct

let solvePriority valMap startingOpsMap =
    let pq = PriorityQueue<Map<string,Op>*(string*string) array*int,int>()
    pq.Enqueue((startingOpsMap,[||],17), 0)
    while pq.Count > 0 do
        let opsMap, switches, idx = pq.Dequeue()
        if switches.Length = 4 then
            ()
        else
        let deps = findDependencies (sprintf "z%i" idx) opsMap
        printfn "%A - Deps: %i - Ops: %i - Idx: %i" switches deps.Length opsMap.Count idx
        deps
        |> Array.Parallel.iter (fun kv1key ->
            for kv2 in opsMap do
                //printfn "Switching %A <-> %A" kv1key kv2.Key
                let newOpsMap =
                    opsMap
                    |> Map.add kv1key kv2.Value
                    |> Map.add kv2.Key opsMap[kv1key]

                try
                    let nextBadIdx = firstBadIdx valMap newOpsMap
                    match nextBadIdx with
                    | Some x when x > idx ->
                        let newSwitches = Array.append switches [|(kv1key, kv2.Key)|]
                        //printfn "Enqueuing: %A -> %i" newSwitches x
                        pq.Enqueue((newOpsMap, newSwitches, x),-x)
                    | None ->
                        let newSwitches = Array.append switches [|(kv1key, kv2.Key)|]
                        printfn "Solved: %A" newSwitches
                    | _ -> ()
                with
                | _ -> ()
            )

//solvePriority valMap opsMap

findDependencies "z04" opsMap


type Op2 =
    | Const of string
    | AND of Op2 * Op2
    | OR of Op2 * Op2
    | XOR of Op2 * Op2

let ops2 =
    let idx = data |> Array.findIndex ((=)"")
    data[idx+1..]
    |> Array.map (fun o ->
        match o with
        | Helpers.Regex "(.+) AND (.+) -> (.+)" [op1;op2;res]
            -> res, AND (Const op1,Const op2)
        | Helpers.Regex "(.+) XOR (.+) -> (.+)" [op1;op2;res]
            -> res, XOR (Const op1,Const op2)
        | Helpers.Regex "(.+) OR (.+) -> (.+)" [op1;op2;res]
            -> res, OR (Const op1,Const op2)
        )
    |> Map.ofArray

let startingAliases ops2 =
    ops2
    |> Map.toArray
    |> Array.choose (fun (k,v) ->
        match v with
        | XOR(Const x, Const y)
        | XOR(Const y, Const x) when x.StartsWith("x") && y.StartsWith("y") ->
            Some <| (k, Const ("xor" + x.Substring(1)))
        | AND(Const x, Const y)
        | AND(Const y, Const x) when x.StartsWith("x") && y.StartsWith("y") ->
            Some <| (k, Const ("and" + x.Substring(1)))

        | _ -> None
        )
    |> Array.filter (fun (k,v) -> v = Const "xor00" |> not)
    |> Map.ofArray

let (|Xorand|_|) =
    function
    | AND (Const a, Const b) when a.StartsWith("xor") && b.StartsWith("c") ->
        Some (Xorand (a.Substring(3)))
    | AND (Const b, Const a) when a.StartsWith("xor") && b.StartsWith("c") ->
        Some (Xorand (a.Substring(3)))
    | _ -> None

let (|Carry|_|) =
    function
    | OR (Const a, Const b) when a.StartsWith("and") && b.StartsWith("xorand") ->
        Some (Carry (a.Substring(3)))
    | OR (Const b, Const a) when a.StartsWith("and") && b.StartsWith("xorand") ->
        Some (Carry (a.Substring(3)))
    | _ -> None
        
let switch k1 k2 (m : Map<_,_>) =
    let v1 = m[k1]
    let v2 = m[k2]
    m |> Map.add k1 v2 |> Map.add k2 v1

let replace startMap =
    let rec f (map: Map<string,Op2>) (aliases: Map<string,Op2>) =
        let hasAlias x = aliases |> Map.containsKey x
        let aliasify x =
            if hasAlias x then
                aliases[x]
            else
                Const x
        let newMap =
            map
            |> Map.map (fun k v ->
                match v with
                | XOR(Const x, Const y) -> XOR(aliasify x, aliasify y)
                | AND(Const x, Const y) -> AND(aliasify x, aliasify y)
                | OR(Const x, Const y)  ->  OR(aliasify x, aliasify y)
                )

        let newAliases =
            let candidates =
                newMap
                |> Map.toArray
                |> Array.choose (fun (k,v) ->
                    match v with
                    | Xorand num -> Some (k, Const ("xorand" + num))
                    | Carry num -> Some (k, Const ("c" + num))
                    | _ -> None
                )
            printfn "%A" candidates
            candidates |> Array.fold (fun s (k,v) -> s |> Map.add k v) aliases

        if newMap = map && newAliases = aliases then
            newMap
        else
            f newMap newAliases

    let startingMap = startMap
                      |> switch "cmv" "z17"
                      |> switch "rmj" "z23"
                      |> switch "rdg" "z30"
                      |> switch "mwp" "btb"
    
    (startingAliases startingMap)
    |> Map.add "and00" (Const "c00") // manually add the carry for the half adder
    |> f startingMap

let printV =
    function
    | XOR (Const a, Const b) when a.StartsWith("xor") ->
        sprintf "XOR (%s, %s)" b a
    | XOR (Const b, Const a) when a.StartsWith("xor") ->
        sprintf "XOR (%s, %s)" b a
    | v -> sprintf "%A" v

replace ops2
|> Map.iter (fun k v -> printfn "%s -> %s" (printV v) k)


let ans2 =
    Array.sort [|"cmv"; "z17"; "rmj"; "z23"; "rdg"; "z30"; "mwp"; "btb"|]
    |> (fun arr -> String.Join(',', arr))

ans2