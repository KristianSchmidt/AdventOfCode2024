#load "Helpers.fsx"

open System
open System.Text.RegularExpressions

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__


let data = Helpers.Web.getInput 3

let getLineSum s =
    let ms = System.Text.RegularExpressions.Regex.Matches(s, "mul\((\d+),(\d+)\)")

    ms
    |> Seq.sumBy
        (fun m -> 
            printfn "%s" m.Groups[0].Value;
            (int64 m.Groups[1].Value)*(int64 m.Groups[2].Value))

let ans1 = data |> Seq.sumBy getLineSum

data[0]

ans1

/// Part 2

let ans2 = data

ans2