#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data =
    Helpers.Web.getInput 20
    |> Array.map (fun s -> s.ToCharArray())
    |> Helpers.toGridMap

let startPos = data |> Map.findKey (fun k v -> v = 'S')
let endPos   = data |> Map.findKey (fun k v -> v = 'E')

let minX,maxX,minY,maxY = Helpers.getMinMax (data |> Map.toArray)

let isInside (x,y) = minX <= x && x <= maxX && minY <= y && y <= maxY

let adj (x,y) =
    [|(x-1,y); (x+1,y); (x,y-1); (x,y+1)|]
    |> Array.filter (fun (x,y) -> isInside (x,y) && data[(x,y)] <> '#')

let parents = Helpers.BFS.bfsWithPath adj startPos |> snd

let baseCost = Helpers.BFS.bfsWithPath adj startPos |> fst |> Map.find endPos

let getPath startPos endPos =
    let rec f acc p =
        if p = startPos then
            acc
        else
            f (parents[p] :: acc) parents[p]

    f [endPos] endPos

let origPath = getPath startPos endPos |> List.toArray

let origPathIdx = origPath |> Array.mapi (fun i x -> x,i) |> Map.ofArray

let squaresWithin dist (sx,sy) =
    seq {
        for x in -dist .. dist do
            for y in -dist .. dist do
                let x' = sx + x
                let y' = sy + y
                if abs x + abs y <= dist && isInside (x',y') && Map.containsKey (x',y') origPathIdx && origPathIdx[(x',y')] > origPathIdx[(sx,sy)] then
                    yield (sx,sy),(x',y')
    }
    |> Seq.toArray

let manhattan ((x,y),(x',y')) = abs (x-x') + abs (y-y')

let findCheatCost cheat = baseCost - (origPathIdx[snd cheat] - origPathIdx[fst cheat]) + (manhattan cheat)

let ans1 =
    origPath
    |> Array.collect (squaresWithin 2)
    |> Array.map findCheatCost
    |> Array.filter (fun x -> baseCost - x >= 100)
    |> Array.length

ans1

/// Part 2

let ans2 =
    origPath
    |> Array.collect (squaresWithin 20)
    |> Array.map findCheatCost
    |> Array.filter (fun x -> baseCost - x >= 100)
    |> Array.length

ans2