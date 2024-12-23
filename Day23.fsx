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

// Add a connection to self to each node since
// that will make set intersection easier
let data2 =
    data
    |> Map.map (fun k v -> v |> Set.add k)

let tryCandidate (k1,k2) =
    let overlap = Set.intersect data2[k1] data2[k2]
    seq {
        yield data2[k1]
        yield data2[k2]
        for k in overlap do
            yield data2[k]
    }
    |> Set.intersectMany

let ans2 =
    seq {
        for k1 in Map.keys data2 do
            for k2 in Map.keys data2 do
                if k1 <> k2 then
                    let overlap = Set.intersect data2[k1] data2[k2]
                    if overlap.Count > 2 then
                        let clique = tryCandidate (k1,k2)
                        if clique.Count = 13 then
                            yield clique
    }
    |> Seq.distinct
    |> Seq.head
    |> Set.toArray
    |> Array.sort
    |> fun a -> String.Join(",",a)

ans2