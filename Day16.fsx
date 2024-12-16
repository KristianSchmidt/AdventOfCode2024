#load "Helpers.fsx"

open System
open System.Collections.Generic

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data =
    Helpers.Web.getInput 16
    |> Array.map (fun s -> s.ToCharArray())
    |> Helpers.toGridMap

//let data = """###############
//#.......#....E#
//#.#.###.#.###.#
//#.....#.#...#.#
//#.###.#####.#.#
//#.#.#.......#.#
//#.#.#####.###.#
//#...........#.#
//###.#.#####.#.#
//#...#.....#.#.#
//#.#.#.###.#.#.#
//#.....#...#.#.#
//#.###.#.#.#.#.#
//#S..#.....#...#
//###############""" |> Helpers.split "\n"
//                   |> Array.map (fun s -> s.ToCharArray())
//                   |> Helpers.toGridMap

type Direction = | E | W | N | S
type Pos = int*int*Direction

type CostFunc = Pos -> Pos -> int

let isGoal ((cx,cy,_): Pos) ((gx,gy,_): Pos) =
    cx = gx && cy = gy

let astar start goal (adj : Pos -> Pos array) (cost : CostFunc) (heuristic : CostFunc) =
    let frontier = PriorityQueue<Pos,int>()
    frontier.Enqueue(start, 0)
    let cameFrom = Dictionary<Pos,Pos>()
    let costSoFar = Dictionary<Pos,int>()
    cameFrom[start] <- (-1,-1,E)
    costSoFar[start] <- 0
    let mutable isDone = false
    let mutable processed = 0

    while (frontier.Count > 0 && isDone = false) do
        let current = frontier.Dequeue()
        processed <- processed + 1

        if (isGoal current goal) then
            isDone <- true
        else
            for next in adj current do
                let newCost = costSoFar[current] + (cost current next)
                if (not (costSoFar.ContainsKey(next)) || newCost < costSoFar[next]) then
                    costSoFar[next] <- newCost
                    let priority = newCost + (heuristic next goal)
                    frontier.Enqueue(next, priority)
                    cameFrom[next] <- current

    printfn "Processed %i nodes" processed
    cameFrom, costSoFar

let (sx,sy) = data |> Map.findKey (fun k v -> v = 'S')
let (ex,ey) = data |> Map.findKey (fun k v -> v = 'E')

let manhattan (x,y,_) (x',y',_) = abs(x-x') + abs(y-y')

let cost (x,y,dir) (x',y',dir') =
    if dir <> dir' then 1000 else 1

let turnClockwise =
    function
    | E -> S | S -> W | W -> N | N -> E

let turnCounterClockwise =
    function
    | E -> N | S -> E | W -> S | N -> W

let adj ((x,y,dir): Pos) =
    let turn1 = (x,y,turnClockwise dir)
    let turn2 = (x,y,turnCounterClockwise dir)
    
    match dir with
    | E -> [|turn1; turn2; (x+1,y,dir)|] | W -> [|turn1; turn2; (x-1,y,dir)|]
    | N -> [|turn1; turn2; (x,y-1,dir)|] | S -> [|turn1; turn2; (x,y+1,dir)|]
    |> Array.filter (fun (x,y,_) -> data[(x,y)] <> '#')

let getPath (endX,endY) (cameFrom: Map<Pos,Pos>) =
    let rec f acc pos =
        match Map.tryFind pos cameFrom with
        | Some x -> f (x :: acc) x
        | None -> acc

    let endPos = cameFrom |> Map.keys |> Seq.find (fun (x,y,_) -> x = endX && y = endY)
    f [(endX,endY,E)] endPos |> List.tail

let toMap d =
    d
    |> Seq.map (fun (k: KeyValuePair<_,_>) -> k.Key, k.Value)
    |> Map.ofSeq

astar (sx,sy,E) (ex,ey,E) adj cost manhattan
|> fst
|> toMap 
|> getPath (ex,ey)

astar (sx,sy,E) (ex,ey,E) adj cost manhattan
|> snd
|> toMap 
|> Map.filter (fun (x,y,_) _ -> x = ex && y = ey)

let ans1 = data

ans1

/// Part 2

let ans2 = data

ans2