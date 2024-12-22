#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = Helpers.Web.getInput 22 |> Array.map int64

let mix (a: int64) b = a ^^^ b

let prune a = a % 16777216L

let evolve (num: int64) =
    let res1 = mix num (num*64L) |> prune
    let res2 = mix res1 (res1/32L) |> prune
    mix res2 (res2*2048L) |> prune

let calc200th num =
    Array.scan (fun s _ -> evolve s) num [|1..2000|]
    |> Array.last

let ans1 = data |> Array.sumBy calc200th

ans1

/// Part 2

let groupAndPrice (arr: (int64*int64) array) =
    arr |> Array.map fst, arr |> Array.last |> snd

let groupsAndPrices num =
    Array.scan (fun s _ -> evolve s) num [|1..2000|]
    |> Array.pairwise
    |> Array.map (fun (a,b) -> ((b%10L)-(a%10L)), b % 10L)
    |> Array.windowed 4
    |> Array.map groupAndPrice

let grps = data |> Array.map groupsAndPrices

let allSequences =
    grps
    |> Array.collect id
    |> Array.map fst
    |> Array.distinct

let grpMap grp =
    grp
    |> Array.map fst
    |> Array.distinct
    |> Array.map (fun a -> Array.find (fst >> (=)a) grp)
    |> Map.ofArray

let allGrpMaps =
    grps
    |> Array.Parallel.map grpMap

let bananas sequence =
    allGrpMaps
    |> Array.choose (Map.tryFind sequence)
    |> Array.sum

let findMaxSeq seqs =
    let mutable max = 0L
    let rec f i seqs =
        if i > Array.length seqs then
            max
        else
            if i % 100 = 0 then
                printfn "Iter: %i" i
            let b = bananas seqs[i]
            if b > max then
                max <- b
                printfn "New max: %i" max
            f (i+1) seqs
    
    seqs
    |> Array.sortByDescending Array.sum
    |> f 0

findMaxSeq allSequences

let ans2 = data

ans2