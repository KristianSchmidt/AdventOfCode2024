#load "Helpers.fsx"
#r "nuget: Microsoft.Z3"

open System
open Helpers

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data =
    Helpers.Web.getInput 13
    |> Array.chunkBySize 4
    |> Array.map (fun arr -> arr[0] + " " + arr[1] + " " + arr[2])
    |> Array.map (function | Regex "Button A: X\+(\d+), Y\+(\d+) Button B: X\+(\d+), Y\+(\d+) Prize: X=(\d+), Y=(\d+)" xs -> xs |> List.map int)

let solve1 xs =
    let [aX;aY;bX;bY;x;y] = xs
    seq {
        for aPresses in 0 .. 100 do
            for bPresses in 0 .. 100 do
                if aPresses * aX + bPresses * bX = x && aPresses * aY + bPresses * bY = y then
                    yield aPresses * 3 + bPresses
    }
    |> Array.ofSeq

let ans1 =
    data
    |> Array.map solve1
    |> Array.map Array.tryHead
    |> Array.sumBy (function | Some i -> i | None -> 0)

ans1

/// Part 2

open Microsoft.Z3

let solve2 (lst: int list) =
    let ctx = new Context()
    let solver = ctx.MkOptimize()

    let [aX;aY;bX;bY;x;y] = lst

    let aPresses = ctx.MkIntConst("aPresses")
    let bPresses = ctx.MkIntConst("bPresses")

    solver.Add(ctx.MkEq(aPresses * aX + bPresses * bX, ctx.MkInt(int64 x + 10000000000000L)))
    solver.Add(ctx.MkEq(aPresses * aY + bPresses * bY, ctx.MkInt(int64 y + 10000000000000L)))
    solver.MkMinimize(aPresses * 3 + bPresses) |> ignore

    match string <| solver.Check() with
    | "UNSATISFIABLE" -> 0L
    | "SATISFIABLE" ->
        let aResult = int64 <| solver.Model.Evaluate(aPresses).SExpr()
        let bResult = int64 <| solver.Model.Evaluate(bPresses).SExpr()
        aResult * 3L + bResult

let ans2 = data |> Array.sumBy solve2

ans2
