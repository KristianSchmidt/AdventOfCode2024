#load "Helpers.fsx"

open System
open System.Text.RegularExpressions

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let patterns, designs =
    let data = Helpers.Web.getInput 19
    let patterns = data[0] |> Helpers.split ", "
    let designs = data[2..]
    patterns, designs

let idxsOf str subStr =
    Regex.Matches(str, sprintf "(?=(%s))" subStr)
    |> Seq.cast<Match>
    |> Seq.map (fun m -> m.Index)
    |> Array.ofSeq

let patternGraph design =
    patterns
    |> Array.collect (fun s -> idxsOf design s
                               |> Array.map (fun i -> i,i+s.Length))
    |> Array.groupBy fst
    |> Array.map (fun (g,arr) -> g,arr |> Array.map snd)
    |> Map.ofArray

let isPossible design =
    let m = patternGraph design

    Helpers.BFS.bfs (fun a -> Map.tryFind a m |> Option.defaultValue [||]) 0
    |> Map.containsKey design.Length

let ans1 = designs |> Array.filter isPossible |> Array.length

ans1

/// Part 2

open System.Collections.Generic

let memoize f =
    let d = new Dictionary<_,_>()
    let rec g x =
        match d.TryGetValue x with
        | true, res -> res
        | _ -> let res = f g x in d.Add(x, res); res
    g    

let findNumPaths design =
    let m = patternGraph design

    let rec numPaths f idx =
        if idx = design.Length then
            1L
        else
            match m |> Map.tryFind idx with
            | Some arr -> arr |> Array.sumBy f
            | None -> 0L

    memoize numPaths 0

let ans2 =
    designs
    |> Array.sumBy findNumPaths
    
ans2