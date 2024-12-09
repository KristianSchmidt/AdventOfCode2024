#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = (Helpers.Web.getInput 9)[0]

type Node = | Free | Disk of int
let isDisk = function | Disk _ -> true | _ -> false
let isFree = function | Free -> true | _ -> false


let arr (s : string) =
    let x = s.ToCharArray() |> Array.map (string >> int)
    let pairs = Array.chunkBySize 2 x
    let mutable id' = 0
    seq {
        for p in pairs do
            match p with
            | [|files;free|] ->
                for i in 0 .. files - 1 do
                    yield Disk id'
                id' <- id' + 1
                if free > 0 then
                    for j in 0 .. free - 1 do
                        yield Free
            | [|files|] ->
                for i in 0 .. files - 1 do
                    yield Disk id'
    } |> Array.ofSeq


let defrag (nodes: Node array) =
    let mutable frontPtr = 0
    let mutable endPtr = nodes.Length
    let rec f () =
        endPtr <- Array.findIndexBack isDisk nodes[.. endPtr-1]
        frontPtr <- Array.findIndex isFree nodes
        printfn "frontPtr: %A (%i) endPtr: %A (%i)" nodes[frontPtr] frontPtr nodes[endPtr] endPtr
        if frontPtr > endPtr then
            nodes
        else
            nodes[frontPtr] <- nodes[endPtr]
            nodes[endPtr] <- Free
            f ()
    
    f ()

let ans1 =
    data
    |> arr 
    |> defrag
    |> Array.map (function | Disk i -> i | Free -> 0)
    |> Array.mapi (fun i _id -> int64 (i * _id))
    |> Array.sum

ans1

/// Part 2

type Node2 = | Free2 of size: int | Disk2 of int*int
let getsize =
    function
    | Free2 size -> size
    | Disk2 (_,size) -> size

let arr2 (s : string) =
    let x = s.ToCharArray() |> Array.map (string >> int)
    let pairs = Array.chunkBySize 2 x
    let mutable id' = 0
    seq {
        for p in pairs do
            match p with
            | [|files;free|] ->
                yield Disk2 (id',files)
                id' <- id' + 1
                if free > 0 then
                    yield Free2 free
            | [|files|] ->
                yield Disk2 (id',files)
    } |> Array.ofSeq

let defrag2 (nodes2: Node2 array) =
    let mutable nodes = Array.copy nodes2
    let maxId = nodes |> Array.map (function | Free2 _ -> -1 | Disk2 (id',_) -> id') |> Array.max
    
    let rec f movId =
        //printfn "%A" nodes
        if movId <= 0 then
            nodes
        else
            let movIdIdx = nodes |> Array.findIndex (function | Disk2 (id',_) when id' = movId -> true | _ -> false)
            let size = getsize nodes[movIdIdx]
            match nodes |> Array.tryFindIndex (function | Free2 i when i >= size -> true | _ -> false) with
            | Some i when i < movIdIdx ->
                printfn "Found space for %i (size %i) at idx %i" movId size i
                let spaceLeft = getsize nodes[i] - size
                nodes[movIdIdx] <- Free2 size
                nodes <-  nodes |> Array.removeAt i |> Array.insertManyAt i [|Disk2 (movId, size); Free2 spaceLeft|]
            | _ ->
                printfn "Couldn't find space for %i" movId
            f (movId - 1)
    f maxId
    |> Array.filter (function | Free2 0 -> false | _ -> true)

let checksum2 (nodes2: Node2 array) =
     seq {
        for n in nodes2 do
            match n with
            | Disk2 (id,size) ->
                for i in 1 .. size do
                    yield Disk id
            | Free2 size ->
                for i in 1 .. size do
                    yield Free
     }
     |> Array.ofSeq
     |> Array.map (function | Disk i -> i | Free -> 0)
     |> Array.mapi (fun i _id -> int64 (i * _id))
     |> Array.sum
     
let ans2 = data |> arr2 |> defrag2 |> checksum2

ans2