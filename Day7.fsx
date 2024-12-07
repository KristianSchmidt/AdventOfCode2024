#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data =
    Helpers.Web.getInput 7
    |> Array.map (fun s -> Helpers.split ": " s)
    |> Array.map (fun [|res; lst|] -> int64 res, Helpers.split " " lst |> Array.map int64 |> List.ofArray)

let findCombinations (res: int64, lst: int64 list) =
    let rec f curr lst =
        match lst with
        | [] when curr = res -> 1
        | [] when curr <> res -> 0
        | _ when curr > res -> 0
        | x :: xs ->
            let solsWhenAdding = f (curr + x) xs
            let solsWhenMult = f (curr * x) xs
            solsWhenAdding + solsWhenMult

    f (List.head lst) (List.tail lst)

// 3267: 81 40 27
findCombinations (3267L, [81L;40L;27L])

let ans1 =
    data
    |> Array.filter (fun x -> findCombinations x > 0)
    |> Array.sumBy fst

ans1

/// Part 2

let findCombinations2 (res: int64, lst: int64 list) =
    let rec f curr lst =
        match lst with
        | [] when curr = res -> 1
        | [] when curr <> res -> 0
        | _ when curr > res -> 0
        | x :: xs ->
            let solsWhenAdding = f (curr + x) xs
            let solsWhenMult = f (curr * x) xs
            let solsWhenConcat =
                let multiplier = Math.Floor(Math.Log10(float x)) + 1.
                f (curr * int64 (Math.Pow(10,multiplier)) + x) xs
            solsWhenAdding + solsWhenMult + solsWhenConcat

    f (List.head lst) (List.tail lst)

let ans2 =
    data
    |> Array.filter (fun x -> findCombinations2 x > 0)
    |> Array.sumBy fst

ans2