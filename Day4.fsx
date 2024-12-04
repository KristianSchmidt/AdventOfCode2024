#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = 
    Helpers.Web.getInput 4
    |> Array.map (fun s -> s.ToCharArray())

let forward = [|'X'; 'M'; 'A'; 'S'|]
let backward = [|'S'; 'A'; 'M'; 'X'|]

let isXmas arr = arr = forward || arr = backward

let transpose data =
    data
    |> Array.mapi (fun x arr -> arr |> Array.mapi (fun y _ -> data[y][x]))

let horizontal data =
    data
    |> Array.map (Array.windowed 4)
    |> Array.reduce Array.append
    |> Array.filter isXmas
    |> Array.length

let vertical data =
    data
    |> transpose
    |> horizontal
    
let isInside (i,j) = i >= 0 && i <= data.Length - 1 &&
                     j >= 0 && j <= data.Length - 1

let diagonals (data: char array array) =
    seq {
        for x in 0 .. data.Length - 1 do
            for y in 0 .. data.Length - 1 do
                yield [|(x,y); (x-1, y-1); (x-2, y-2); (x-3, y-3);|]
                yield [|(x,y); (x+1, y-1); (x+2, y-2); (x+3, y-3);|]
                yield [|(x,y); (x+1, y+1); (x+2, y+2); (x+3, y+3);|]
                yield [|(x,y); (x-1, y+1); (x-2, y+2); (x-3, y+3);|]
    }
    |> Seq.filter (Array.forall isInside)
    |> Seq.toArray
    |> Array.map (Array.map (fun (x,y) -> data[x][y]))
    |> Array.filter isXmas
    |> Array.length
    |> (fun i -> i / 2)

let ans1 =
    diagonals data +
    horizontal data +
    vertical data

ans1

/// Part 2

let checkIfValid (data: char array array) (x,y) =
    let isA = data[x][y] = 'A'
    let topLeftDownRight =
        let arr = [|data[x-1][y-1];data[x+1][y+1]|]
        arr = [|'M';'S'|] || arr = [|'S';'M'|]
    let downLeftTopRight =
        let arr = [|data[x-1][y+1];data[x+1][y-1]|]
        arr = [|'M';'S'|] || arr = [|'S';'M'|]
    isA && topLeftDownRight && downLeftTopRight

let ans2 =
    seq {
        for x in 1 .. data.Length - 2 do
            for y in 1 .. data.Length - 2 do
                if checkIfValid data (x,y) then
                    yield (x,y)
    }
    |> Array.ofSeq
    |> Array.length

ans2