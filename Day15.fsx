#load "Helpers.fsx"

open System


Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let map,moves =
    let data = Helpers.Web.getInput 15
    let splitIdx = data |> Array.findIndex ((=)"")
    let map = data[0..splitIdx-1] |> Array.map (fun s -> s.ToCharArray()) |> Helpers.toGridMap
    let moves = data[splitIdx+1..data.Length-1] |> Array.collect (fun s -> s.ToCharArray())
    map, moves

let startPos = map |> Map.findKey (fun _ v -> v = '@')

let direction = function
                  | '^' -> (0,-1) | '>' -> ( 1,0)
                  | 'v' -> (0, 1) | '<' -> (-1,0)

type Move =
    | NoMove
    | MoveBot of (int*int)
    | MoveBotAndBox of (int*int)*(int*int)

let findMove (map: Map<int*int,char>) pos (dX,dY) =
    let rec f (cX,cY) (boxes: (int*int) list) =
        let nextPos = (cX+dX, cY+dY)
        match map[nextPos] with
        | '#' -> NoMove
        | '.' ->
            match boxes with
            | [] -> MoveBot nextPos
            | xs ->
                // Move bot to the first box
                // Move first box to first empty
                MoveBotAndBox ((List.last xs), nextPos)
        | 'O' -> f nextPos (nextPos :: boxes)

    f pos []

let runMoves startingPos =
    let movesLst = List.ofArray moves
    let rec f mv pos (map: Map<int*int,char>) =
        match mv with
        | x :: xs ->
            //printfn "Move from %A in direction %A. %A" pos x (map[startPos])
            match findMove map pos (direction x) with
            | NoMove -> f xs pos map
            | MoveBot botPos ->
                let newMap = map |> Map.add pos '.' |> Map.add botPos '@'
                f xs botPos newMap
            | MoveBotAndBox (botPos, boxPos) ->
                let newMap = map |> Map.add pos '.'
                                 |> Map.add botPos '@'
                                 |> Map.add boxPos 'O'
                f xs botPos newMap
        | [] -> map

    f movesLst startingPos map

let ans1 =
    runMoves startPos
    |> Map.filter (fun k v -> v = 'O')
    |> Map.toArray
    |> Array.map fst
    |> Array.sumBy (fun (x,y) -> x + 100*y)

ans1

/// Part 2

let map2 =
    let data = Helpers.Web.getInput 15
    let splitIdx = data |> Array.findIndex ((=)"")
    let map =
        data[0..splitIdx-1]
        |> Array.map (fun s -> s.ToCharArray()
                               |> Array.collect (function
                                                 | '#' -> [|'#';'#'|]
                                                 | '.' -> [|'.';'.'|]
                                                 | '@' -> [|'@';'.'|]
                                                 | 'O' -> [|'[';']'|]
                               ))
    map
    |> Helpers.toGridMap

let neighbors dy (map: Map<int*int,char>) (x,y) =
    // in direction dy they're neighbors for any side of the box
    // in direction x they're neighbors if it's the other side of the box
    let dyN =
        match map[(x,y+dy)] with
        | '[' | ']' -> Some (x,y+dy)
        | _ -> None

    let xN =
        match map[(x,y)] with
        | '[' when map[(x+1,y)] = ']' -> Some (x+1,y)
        | ']' when map[(x-1,y)] = '[' -> Some (x-1,y)
        | _ -> None

    [|dyN; xN|] |> Array.choose id

let neighborsSideways dx (map: Map<int*int,char>) (x,y) =
    if map[(x+dx,y)] = '[' || map[(x+dx,y)] = ']' then
        [|(x+dx,y)|]
    else Array.empty

let printMap (map: Map<int*int,char>) =
    let maxX = map |> Map.keys |> Seq.maxBy fst |> fst
    let maxY = map |> Map.keys |> Seq.maxBy snd |> snd
    for y in 0..maxY do
        for x in 0..maxX do
            printf "%c" map[(x,y)]
        printfn ""

let runMoves2 map =
    let startPos = map |> Map.findKey (fun _ v -> v = '@')

    let movesLst = List.ofArray moves
    let rec f mv (cx,cy) (map: Map<int*int,char>) =
        match mv with
        | x :: xs ->
            //printMap map
            //printfn "Move from %A in direction %A. %A" (cx,cy) x (map[(cx,cy)])
            let neighborFunc, dir, dirF =
                match x with
                | '^' -> neighbors, -1, fun (x,y) -> (x,y-1)
                | 'v' -> neighbors,  1, fun (x,y) -> (x,y+1)
                | '>' -> neighborsSideways,  1, fun (x,y) -> (x+1,y)
                | '<' -> neighborsSideways, -1, fun (x,y) -> (x-1,y)
            let boxes = Helpers.BFS.bfs (neighborFunc dir map) (cx,cy)
                        |> Map.keys |> Seq.toArray
            let canMove =
                boxes
                |> Array.exists (fun (x,y) -> map[dirF (x,y)] = '#')
                |> not
            if canMove then
                //printfn "%A" <| (boxes |> Array.map (fun (x,y) -> map[(x,y)]))
                let newMap =
                    let newPositons = boxes |> Array.map (fun (x,y) -> (dirF (x,y),map[(x,y)]))
                    let mapCleared = boxes |> Array.fold (fun s pos -> s |> Map.add pos '.') map
                    newPositons |> Array.fold (fun s (k,v) -> s |> Map.add k v) mapCleared
                f xs (dirF (cx,cy)) newMap
            else
                f xs (cx,cy) map
        | [] -> map

    f movesLst startPos map

let ans2 =
    runMoves2 map2
    |> Map.filter (fun k v -> v = '[')
    |> Map.keys
    |> Seq.sumBy (fun (x,y) -> x + 100*y)

ans2