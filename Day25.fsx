#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data =
    let i = Helpers.Web.getInput 25
    i
    |> Array.chunkBySize 8
    |> Array.map (fun arr -> arr[0..6])

let toNumSeq (arr : string array) =
    [|0..4|]
    |> Array.map (fun i -> 
        arr[1..]
        |> Array.map (fun s -> s[i])
        |> Array.filter ((=)'#')
        |> Array.length)

let locks,keys =
    let (l,k) =
        data
        |> Array.partition (fun arr -> arr[0][0] = '#')
    l |> Array.map toNumSeq, k |> Array.map (Array.rev >> toNumSeq)

let fits (a1: int array,a2: int array) =
    Array.zip a1 a2
    |> Array.forall (fun (x1,x2) -> x1 + x2 <= 5)

let ans1 =
    Array.allPairs locks keys
    |> Array.filter fits
    |> Array.length

ans1

/// Part 2

let ans2 = data

ans2