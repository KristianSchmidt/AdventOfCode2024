#load "Helpers.fsx"

open System
open System.Text.RegularExpressions

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__


let data = Helpers.Web.getInput 3

let getLineSum s =
    let ms = Regex.Matches(s, "mul\((\d+),(\d+)\)")

    ms
    |> Seq.sumBy
        (fun m -> 
            printfn "%s" m.Groups[0].Value;
            (int64 m.Groups[1].Value)*(int64 m.Groups[2].Value))

let ans1 = data |> Seq.sumBy getLineSum

ans1

/// Part 2

let getLineSum2 s =
    let dos =
        Regex.Matches(s, "do\(\)") |> Seq.map (fun m -> m.Index) |> Set.ofSeq
    let donts =
        Regex.Matches(s, "don't\(\)") |> Seq.map (fun m -> m.Index) |> Set.ofSeq

    let isMostRecentDo i =
        let mostRecentDo = Array.tryFindBack (fun i -> Set.contains i dos) [|0..i|]
        let mostRecentDont = Array.tryFindBack (fun i -> Set.contains i donts) [|0..i|]
        match mostRecentDo, mostRecentDont with
        | Some _, None -> true
        | None, Some _ -> false
        | Some x, Some y -> x > y
        | _ -> true

    let mults =
        Regex.Matches(s, "mul\((\d+),(\d+)\)")
        |> Seq.filter (fun m -> isMostRecentDo m.Index)

    mults |> Seq.sumBy (fun m -> (int64 m.Groups[1].Value)*(int64 m.Groups[2].Value))

// In this part it needs to be one line, so it remembers if the last part of a line is a don't() instruction
let ans2 = getLineSum2 (String.Join('\n', data))

ans2