#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data =
    Helpers.Web.getInput 14
    |> Array.map (function | Helpers.Regex "p=([\-\d]+),([\-\d]+) v=([\-\d]+),([\-\d]+)" [px;py;vx;vy] -> (int px, int py, int vx, int vy))

let xDim = 101
let yDim = 103

let position (px,py,vx,vy) iters =
    let endX =
        let x = (px + vx*iters) % xDim
        if x < 0 then
            xDim + x
        else
            x
    let endY =
        let y = (py + vy*iters) % yDim
        if y < 0 then
            yDim + y
        else
            y
    (endX,endY)
    
let midX = Math.Floor(float xDim / 2.) |> int
let midY = Math.Floor(float yDim / 2.) |> int

let quardrant (px,py) =
    match px < midX, py < midY with
    | true, true -> 1
    | false, true -> 2
    | true, false -> 3
    | false, talse -> 4

let ans1 =
    data
    |> Array.map (fun d -> position d 100)
    |> Array.filter (fun (x,y) -> x <> midX && y <> midY)
    |> Array.countBy quardrant
    |> Array.map snd
    |> Array.reduce (*)

ans1

/// Part 2

let printIt (pos: (int*int) array) =
    let isRobot = Set.ofArray pos
    for y in 0..(yDim-1) do
        [|0..xDim-1|]
        |> Array.map (fun x -> if Set.contains (x,y) isRobot then 'X' else ' ')
        |> (fun arr -> String.Join("",arr))
        |> printfn "%s"

let p iters =
    printfn "%i" iters
    data
    |> Array.map (fun d -> position d iters)
    |> printIt

[|0..10000|]
|> Array.iter p

let ans2 = 8179 // found by visual inspection

ans2