#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = Helpers.Web.getInput 5

let rules,sets =
    let idx = data |> Array.findIndex ((=)"")
    data[0..idx-1] |> Array.map (Helpers.split "|" >> fun [|x;y|] -> (int x,int y)), data[idx+1 ..]

let isSetValid (set: string) =
    let positions = Helpers.split "," set |> Array.map int |> Array.mapi (fun i v -> (v,i)) |> Map.ofArray
    rules
    |> Array.forall (
        fun (before,after) ->
            match Map.tryFind before positions, Map.tryFind after positions with
            | Some b, Some a -> b < a
            | _ -> true
        )
    
let ans1 =
    sets
    |> Array.filter isSetValid
    |> Array.map (Helpers.split "," >> Array.map int)
    |> Array.sumBy (fun arr -> arr[arr.Length/2])

ans1

/// Part 2

let incorrect =
    sets
    |> Array.filter (isSetValid >> not)
    |> Array.map (Helpers.split "," >> Array.map int)

let findBrokenRule (arr : int array) =
    let positions = arr |> Array.mapi (fun i v -> (v,i)) |> Map.ofArray
    rules
    |> Array.tryFind (
        fun (before,after) ->
            match Map.tryFind before positions, Map.tryFind after positions with
            | Some b, Some a -> b > a
            | _ -> false
        )

let rec fix (arr: int array) =
    let positions = arr |> Array.mapi (fun i v -> (v,i)) |> Map.ofArray
    match findBrokenRule arr with
    | Some (before, after) ->
        let idxBefore = positions[before]
        let idxAfter = positions[after]
        arr
        |> Array.removeAt idxBefore
        |> Array.insertAt idxBefore after
        |> Array.removeAt idxAfter
        |> Array.insertAt idxAfter before
        |> fix
    | None -> arr

let ans2 =
    incorrect
    |> Array.map fix
    |> Array.sumBy (fun arr -> arr[arr.Length/2])

ans2