#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data =
    Helpers.Web.getInput 10
    |> Array.map (fun s -> s.ToCharArray() |> Array.map (string >> int))
    |> Array.mapi (fun x a -> a |> Array.mapi (fun y e -> ((x,y),e)))

let dataMap = data |> Array.collect id |> Map.ofArray

let isInside (x,y) = 0 <= x && x <= data.Length - 1 && 0 <= y && y <= data[0].Length - 1

let neighbors (x,y) =
    [|(x-1,y);(x+1,y);(x,y-1);(x,y+1)|]
    |> Array.filter (fun (x',y') -> isInside (x',y') && dataMap[(x',y')] - dataMap[(x,y)] = 1)

let trailheadScore (x,y) =
    Helpers.BFS.bfs neighbors (x,y)
    |> Map.filter (fun k v -> v = 9)
    |> Map.count

let ans1 = dataMap
           |> Map.filter (fun k v -> v = 0)
           |> Map.toArray
           |> Array.sumBy (fst >> trailheadScore)

ans1

/// Part 2

let findAllPaths pos =
    let rec f curr =
        let ns = neighbors curr
        match ns with
        | [||] when dataMap[curr] = 9 -> 1
        | [||] -> 0
        | xs -> xs |> Array.sumBy f

    f pos

let ans2 = dataMap
           |> Map.filter (fun k v -> v = 0)
           |> Map.toArray
           |> Array.sumBy (fst >> findAllPaths)

ans2