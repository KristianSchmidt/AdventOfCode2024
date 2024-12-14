#load "Helpers.fsx"

open System
open System.Collections.Generic

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = (Helpers.Web.getInput 11)[0] |> Helpers.split " " |> Array.map int64

let digits (i: int64) =
    Math.Floor(Math.Log10(float i)) + 1.
    |> int

let transformElement =
    function
    | 0L -> [|1L|]
    | i when (digits i) % 2 = 0 ->
        //printfn "here1 %i" i
        let s = string i
        let d1 = s.Substring(0,s.Length / 2) |> int64
        let d2 = s.Substring(s.Length / 2) |> int64
        [|d1;d2|]
    | i ->
        //printfn "here2 %i" i
        [|i * 2024L|]

let transform (arr: int64 array) =
    arr
    |> Array.map transformElement
    |> Array.collect id

let ans1 = Array.fold (fun s i -> transform s) data [|1..25|] |> Array.length

ans1

/// Part 2

let stones f (i,itersLeft) =
    match itersLeft with
    | 0 -> 1L // If there are no iterations left, a stone is a stone
    | _ -> transformElement i
           |> Array.sumBy (fun ele -> f (ele,(itersLeft - 1)))

let memoize f =
    let d = new Dictionary<_,_>()
    let rec g x =
        match d.TryGetValue x with
        | true, res -> res
        | _ -> let res = f g x in d.Add(x, res); res
    g

let ans2 =
    data
    |> Array.sumBy (fun i -> memoize stones (i,75))

ans2