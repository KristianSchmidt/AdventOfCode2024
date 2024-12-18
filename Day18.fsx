#load "Helpers.fsx"

open System
open System.Collections.Generic

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data =
    Helpers.Web.getInput 18
    |> Array.map (Helpers.split ",")
    |> Array.map (fun [|x;y|] -> (int x, int y))

type Pos = int*int

type CostFunc = Pos -> Pos -> int

let isGoal ((cx,cy): Pos) ((gx,gy): Pos) =
    cx = gx && cy = gy

let astar start goal (adj : Pos -> Pos array) (cost : CostFunc) (heuristic : CostFunc) =
    let frontier = PriorityQueue<Pos,int>()
    frontier.Enqueue(start, 0)
    let cameFrom = Dictionary<Pos,Pos>()
    let costSoFar = Dictionary<Pos,int>()
    cameFrom[start] <- (-1,-1)
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

    //printfn "Processed %i nodes" processed
    cameFrom, costSoFar

let isInside (x,y) =
    0 <= x && x <= 70 && 0 <= y && y <= 70

let p1corrupt = data[0..1023] |> Set.ofArray

let adj1 (x,y) =
    [|(x+1,y); (x-1,y); (x,y+1); (x,y-1)|]
    |> Array.filter isInside
    |> Array.filter (fun (x',y') -> Set.contains (x',y') p1corrupt |> not)

let ans1 =
    astar (0,0) (70,70) adj1 (fun _ _ -> 1) (fun _ _ -> 0)
    |> snd
    |> fun d -> d[70,70]

ans1

/// Part 2

let p2corrupt =
    [|0..data.Length-1|]
    |> Array.map (fun i -> i, data[0..i] |> Set.ofArray)
    |> Map.ofArray

let adj2 i (x,y) =
    [|(x+1,y); (x-1,y); (x,y+1); (x,y-1)|]
    |> Array.filter isInside
    |> Array.filter (fun (x',y') -> Set.contains (x',y') p2corrupt[i] |> not)

let noSolution i =
    astar (0,0) (70,70) (adj2 i) (fun _ _ -> 1) (fun _ _ -> 0)
    |> snd
    |> fun d -> d.ContainsKey((70,70)) |> not

let ans2 =
    [|0..data.Length-1|]
    |> Array.find noSolution
    |> fun i -> data[i]

ans2
