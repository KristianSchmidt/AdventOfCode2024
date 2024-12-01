#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = Helpers.Web.getInput 1

let l1, l2 =
    data
    |> Array.map (Helpers.split "   ")
    |> Array.map (Array.map int)
    |> Array.map (fun [|x1;x2|] -> (x1,x2))
    |> Array.unzip
    |> (fun (l1,l2) -> Array.sort l1, Array.sort l2)

let ans1 =
    Array.zip l1 l2
    |> Array.map (fun (x1,x2) -> Math.Abs(x1-x2))
    |> Array.sum

ans1

/// Part 2

let l2counts =
    l2
    |> Array.countBy id
    |> Map

let ans2 =
    l1
    |> Array.sumBy (
        fun x -> 
            x * (Map.tryFind x l2counts |> Option.defaultValue 0)
       )

ans2