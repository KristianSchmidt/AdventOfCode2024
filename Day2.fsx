#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = Helpers.Web.getInput 2

let isValid (arr: int array) =
    let diffs = arr |> Array.pairwise |> Array.map (fun (x1,x2) -> x1 - x2)
    let allSame = (diffs |> Array.map (fun x -> Math.Sign(x)) |> Set.ofArray |> Set.count) = 1
    let maxDiff3 = (diffs |> Array.map abs |> Array.max) <= 3
    allSame && maxDiff3

let ans1 = 
    data
    |> Array.map (Helpers.split " " >> Array.map int)
    |> Array.filter isValid
    |> Array.length

ans1

/// Part 2

let isValid2 (arr: int array) =
    let idxs = [|0..arr.Length-1|]
    idxs
    |> Array.exists (fun idx -> arr |> Array.removeAt idx |> isValid)

let ans2 = 
    data
    |> Array.map (Helpers.split " " >> Array.map int)
    |> Array.filter isValid2
    |> Array.length

ans2
