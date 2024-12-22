#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = Helpers.Web.getInput 21

(*
+---+---+---+
| 7 | 8 | 9 |
+---+---+---+
| 4 | 5 | 6 |
+---+---+---+
| 1 | 2 | 3 |
+---+---+---+
    | 0 | A |
    +---+---+

    +---+---+
    | ^ | A |
+---+---+---+
| < | v | > |
+---+---+---+
*)

let diff (x,y) (x',y') = (x'-x), (y'-y)

let numLoc =
    function
    | 'A' -> (0 ,0)  | '0' -> (-1,0)
    | '3' -> (0 ,-1) | '2' -> (-1,-1) | '1' -> (-2,-1)
    | '6' -> (0 ,-2) | '5' -> (-1,-2) | '4' -> (-2,-2)
    | '9' -> (0 ,-3) | '8' -> (-1,-3) | '7' -> (-2,-3)

let arrowLoc =
    function
    | 'A' -> (0,0) | '^' -> (-1,0)
    | '>' -> (0,1) | 'v' -> (-1,1) | '<' -> (-2,1)

let horizontal x =
    if Math.Sign(float x) > 0 then
        Array.replicate x '>'
    else
        Array.replicate (abs x) '<'

let vertical y =
    if Math.Sign(float y) > 0 then
        Array.replicate y 'v'
    else
        Array.replicate (abs y) '^'

let solveNumericPath (fromKey,toKey) =
    let fromLoc, toLoc = numLoc fromKey, numLoc toKey
    let (x,y) = diff fromLoc toLoc
    match fromKey, toKey with
    | '0', '1' | '0', '4' | '0', '7'
    | 'A', '1' | 'A', '4' | 'A', '7' -> 
        [| Array.append (vertical y) (horizontal x) |]
    | '1', '0' | '4', '0' | '7', '0' 
    | '1', 'A' | '4', 'A' | '7', 'A' -> 
        [| Array.append (horizontal x) (vertical y) |]
    | _ -> 
        [|
            Array.append (horizontal x) (vertical y)
            Array.append (vertical y) (horizontal x)
        |]
    |> Array.distinct
    |> (fun arrs -> arrs |> Array.map (fun arr -> Array.append arr [|'A'|]))

let solveArrowPath (fromKey,toKey) =
    let fromLoc, toLoc = arrowLoc fromKey, arrowLoc toKey
    let (x,y) = diff fromLoc toLoc
    match fromKey, toKey with
    | _, '<' ->
        [| Array.append (vertical y) (horizontal x) |]
    | '<', _ ->
        [| Array.append (horizontal x) (vertical y) |]
    | _ ->
        [|
            Array.append (horizontal x) (vertical y)
            Array.append (vertical y) (horizontal x)
        |]
    |> Array.distinct
    |> (fun arrs -> arrs |> Array.map (fun arr -> Array.append arr [|'A'|]))

let collectPaths (arrs : char array array array) =
    let rec f i (acc: char array array) =
        if i > arrs.Length - 1 then
            acc
        else
            let newAcc =
                acc
                |> Array.collect
                    (fun a -> arrs[i]
                              |> Array.map (fun a' -> Array.append a a'))                        
            f (i+1) newAcc
                
    f 0 [|[||]|]

let keypresses solveF pattern =
    Array.append [|'A'|] pattern
    |> Array.pairwise
    |> Array.map solveF
    |> collectPaths

let complexity (s : string) =
    let minLength =
        keypresses solveNumericPath (s.ToCharArray())
        |> Array.collect (keypresses solveArrowPath)
        |> Array.collect (keypresses solveArrowPath)
        |> Array.map Array.length
        |> Array.min
    let numericPart = s.Replace("A", "") |> int
    minLength * numericPart

let ans1 = data |> Array.sumBy complexity

ans1

/// Part 2

let pairwiseArr =
    Helpers.memoize (fun (s: string) -> ("A" + s + "A").ToCharArray() |> Array.pairwise)

type TransitionConfig =
    { Av: string; vA: string; UpRight: string; RightUp: string }

let transConfigs =
    seq {
        for av in [|"<v"; "v<"|] do
            for va in [|"^>"; ">^"|] do
                for ur in [|">v";"v>"|] do
                 for ru in [|"^<"; "<^"|] do
                    yield {Av = av; vA = va; UpRight = ur; RightUp = ru}
    } |> Array.ofSeq

let getTransitions transConfig ((fromPos,toPos),c) =
    match fromPos,toPos with
    | x, y when x = y -> pairwiseArr "" 
    | 'A', '<' -> pairwiseArr "v<<"
    | 'A', '^' -> pairwiseArr "<"  
    | 'A', 'v' -> pairwiseArr transConfig.Av
    | 'A', '>' -> pairwiseArr "v"  
    | '<', 'A' -> pairwiseArr ">>^"
    | '<', '^' -> pairwiseArr ">^" 
    | '<', 'v' -> pairwiseArr ">"  
    | '<', '>' -> pairwiseArr ">>" 
    | '^', 'A' -> pairwiseArr ">"  
    | '^', '<' -> pairwiseArr "v<" 
    | '^', 'v' -> pairwiseArr "v"  
    | '^', '>' -> pairwiseArr transConfig.UpRight
    | '>', 'A' -> pairwiseArr "^"  
    | '>', '<' -> pairwiseArr "<<" 
    | '>', 'v' -> pairwiseArr "<"  
    | '>', '^' -> pairwiseArr transConfig.RightUp
    | 'v', 'A' -> pairwiseArr transConfig.vA
    | 'v', '<' -> pairwiseArr "<"  
    | 'v', '>' -> pairwiseArr ">"  
    | 'v', '^' -> pairwiseArr "^"  
    |> Array.map (fun p -> p,c)

let doRound config (m: Map<char*char,int64>) =
    m
    |> Map.toArray
    |> Array.collect (getTransitions config)
    |> Array.groupBy fst
    |> Array.map (fun (k,g) -> k, g |> Array.sumBy snd)
    |> Map.ofArray

let mapLength (m: Map<_,int64>) =
    m
    |> Map.toSeq
    |> Seq.sumBy snd

let complexity2 config rounds (s: string) =
    let numericPart = s.Replace("A", "") |> int64
    let minLength =
        keypresses solveNumericPath (s.ToCharArray())
        |> Array.map (fun arr -> 
            arr
            |> Array.append [|'A'|]
            |> Array.pairwise
            |> Array.countBy id
            |> Array.map (fun (k,c) -> k,int64 c)
            |> Map.ofArray
            |> (fun m -> Array.fold (fun s _ -> (doRound config) s) m [|1..rounds|])
            |> mapLength
        )
        |> Array.min
    minLength*numericPart

let ans2 = data
           |> Array.map (fun d -> transConfigs
                                  |> Array.map (fun t -> complexity2 t 25 d)
                                  |> Array.min)
           |> Array.sum

ans2
