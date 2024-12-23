#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data =
    Helpers.Web.getInput 23
    |> Array.map (Helpers.split "-")
    |> Array.collect (fun [|a;b|] -> [|(a,b);(b,a)|])
    |> Array.groupBy fst
    |> Array.map (fun (k,arr) -> k,arr |> Array.map snd |> Set.ofArray)
    |> Map.ofArray

let allPairs (s: Set<string>) =
    let arr = s |> Set.toArray
    seq {
        for i in 0..arr.Length-2 do
            for j in i+1..arr.Length-1 do
                yield (arr[i],arr[j])
    }
    |> Array.ofSeq

let startsWithT =
    data
    |> Map.filter (fun k v -> k.StartsWith("t"))
    |> Map.toArray

let ans1 =
    seq {
        for (t,tConn) in startsWithT do
            for (p1,p2) in allPairs tConn do
                if Set.contains p2 data[p1] then
                    yield (t, p1, p2)

    }
    |> Seq.map (fun (a,b,c) -> Set.ofArray [|a;b;c;|])
    |> Seq.distinct
    |> Seq.length

ans1

/// Part 2

let ans2 = data

ans2