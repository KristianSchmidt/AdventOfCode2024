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

data[0]

let dos =
    Regex.Matches(data[0], "do\(\)") |> Seq.map (fun m -> m.Index)
let donts =
    Regex.Matches(data[0], "don't\(\)") |> Seq.map (fun m -> m.Index)
let mults =
    Regex.Matches(data[0], "mul\((\d+),(\d+)\)") |> Seq.map (fun m -> m.Index)

let ans2 = data

ans2