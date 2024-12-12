#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = Helpers.Web.getInput 12
           |> Array.map (fun s -> s.ToCharArray())

let isInside (x,y) = 0 <= x && x <= data.Length - 1 && 0 <= y && y <= data[0].Length - 1

let neighbors (x,y) =
    [|(x-1,y); (x+1, y); (x, y-1); (x, y+1);|]
    |> Array.filter (fun (x',y') -> isInside (x',y') && data[x][y] = data[x'][y'])

let getAreas () =
    let points = seq { for x in 0..data.Length-1 do for y in 0..data[0].Length-1 do yield (x,y) } |> Set.ofSeq
    let rec f pointsLeft areas =
        if Set.isEmpty pointsLeft then
            areas
        else
            let p = Set.minElement pointsLeft
            let newArea = Helpers.BFS.bfs neighbors p |> Map.keys |> Set.ofSeq
            let newAreas = Set.add newArea areas
            let newPointsLeft = Set.difference pointsLeft newArea
            f newPointsLeft newAreas

    f points Set.empty

let perimiter (area: Set<int*int>) =
    area
    |> Set.toSeq
    |> Seq.sumBy (fun p -> 4 - Array.length (neighbors p))

let price (area: Set<int*int>) =
    Set.count area * (perimiter area)

let ans1 =
    getAreas ()
    |> Set.toSeq
    |> Seq.sumBy price

ans1

/// Part 2

let nonNeighbors (x,y) =
    [|(x-1,y); (x+1, y); (x, y-1); (x, y+1);|]
    |> Array.filter (fun (x',y') -> (isInside (x',y') && data[x][y] <> data[x'][y']) || ((x' = -1 || x' = data.Length) || (y' = -1 || y' = data[0].Length)))
    |> Array.map (fun (x',y') -> (x',y',(x-x',y-y')))

let perimiterNeighbors (area: Set<int*int*(int*int)>) (x,y,dir) =
    [|(x-1,y,dir); (x+1, y, dir); (x, y-1, dir); (x, y+1, dir);|]
    |> Array.filter (fun (x',y',dir') -> Set.contains (x',y',dir') area)// || ((x' = -1 || x' = data.Length) && (y' = -1 || y' = data[0].Length)))

let getSides area =    
    let rec f pointsLeft perimeters =
        if Set.isEmpty pointsLeft then
            perimeters
        else
            let p = Set.minElement pointsLeft
            let newPerimeter = Helpers.BFS.bfs (perimiterNeighbors pointsLeft) p |> Map.keys |> Set.ofSeq
            let newPerimeters = Set.add newPerimeter perimeters
            let newPointsLeft = Set.difference pointsLeft newPerimeter
            f newPointsLeft newPerimeters

    f area Set.empty

let priceBulk (area: Set<int*int>) =
    let perimeter = area |> Set.map nonNeighbors |> Seq.toArray |> Array.collect id |> Set.ofArray
    let sides = getSides perimeter |> Set.count
    sides * (Set.count area)

let ans2 = getAreas ()
           |> Set.toArray
           |> Array.sumBy priceBulk


ans2