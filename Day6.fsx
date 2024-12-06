#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data =
    Helpers.Web.getInput 6
    |> Array.map (fun s -> s.ToCharArray())

let startPos =
    seq {
        for x in 0 .. data.Length - 1 do
            for y in 0 .. data[0].Length - 1 do
                if data[x][y] = '^' then
                    yield (x,y)
    } |> Seq.head

let isOutside (x,y) =
    (0 <= x && x <= data.Length - 1 && 0 <= y && y <= data[0].Length - 1)
    |> not

let turn (x,y) = (y,-x)

let run startPos =
    let mutable distinctPos = Set.empty
    let rec f (x,y) (dirX,dirY) steps =
        distinctPos <- Set.add (x,y) distinctPos
        //printfn "(%i,%i) (%i,%i) %i" x y dirX dirY steps
        let (nx,ny) = (x+dirX, y+dirY)
        //printfn "(%i,%i)" nx ny
        if isOutside (nx,ny) then
            distinctPos |> Set.count
        else
            match data[nx][ny] with
            | '.' -> f (nx,ny) (dirX,dirY) (steps+1)
            | '#' -> f (x,y) (turn (dirX, dirY)) steps

    f startPos (-1,0) 0

let ans1 = run startPos

ans1

/// Part 2

let getsStuckInLoop startPos (obsX,obsY) =
    let dataCpy = Array.init data.Length (fun i -> Array.copy data[i])
    dataCpy[obsX][obsY] <- '#'

    let mutable distinctPos = Set.empty
    let rec f (x,y) (dirX,dirY) =
        let (nx,ny) = (x+dirX, y+dirY)
        if Set.contains (x,y,dirX,dirY) distinctPos then
            printfn "(%i, %i) %i" obsX obsY (Set.count distinctPos)
            true
        else if isOutside (nx,ny) then
            printfn "(%i,%i) false" obsX obsY
            false
        else
            distinctPos <- Set.add (x,y,dirX,dirY) distinctPos    
            match dataCpy[nx][ny] with
            | '^' | '.' -> f (nx,ny) (dirX,dirY)
            | '#' -> f (x,y) (turn (dirX, dirY))

    if startPos = (obsX, obsY) then
        false
    else
        f startPos (-1,0)

let ans2 =
    seq {
        for x in 0 .. data.Length - 1 do
            for y in 0 .. data[0].Length - 1 do
                yield getsStuckInLoop startPos (x,y)
    }
    |> Array.ofSeq
    |> Array.filter id
    |> Array.length

ans2