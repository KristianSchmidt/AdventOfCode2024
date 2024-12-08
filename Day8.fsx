#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data =
    Helpers.Web.getInput 8
    |> Array.map (fun s -> s.ToCharArray())

let coords =
    seq {
        for x in 0 .. data.Length - 1 do
            for y in 0 .. data.Length - 1 do
                yield (x,y)
    }

let isInside (x,y) = 0 <= x && x <= data.Length - 1 && 0 <= y && y <= data.Length - 1

let ans1 =
    seq {
        for (x,y) in coords do
            for (x', y') in coords do
                if ((x,y) <> (x',y') &&
                    data[x][y] = data[x'][y']
                    && data[x][y] <> '.') then
                    yield x'+(x'-x), y'+(y'-y)
    }
    |> Seq.filter isInside
    |> Set.ofSeq
    |> Set.count

ans1

/// Part 2

let ans2 =
    seq {
        for (x,y) in coords do
            for (x', y') in coords do
                if ((x,y) <> (x',y') &&
                    data[x][y] = data[x'][y']
                    && data[x][y] <> '.') then
                    yield (x,y)
                    yield (x',y')
                    let (vx,vy) = (x'-x), (y'-y)
                    let mutable i = 1
                    let mutable candidate = ((x'+i*vx),(y'+i*vy))
                    while (isInside candidate) do
                        yield candidate
                        i <- i + 1
                        candidate <- ((x'+i*vx),(y'+i*vy))
    }
    |> Seq.filter isInside
    |> Set.ofSeq
    |> Set.count

ans2