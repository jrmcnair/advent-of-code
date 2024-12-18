
// https://adventofcode.com/2024/day/18
module Day18

open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions
open AdventOfCode.Common


(*
let startLoc = (0,0)
let endLoc = (6,6)

- coords are x,y
- grid is [0..70, 0..70]
- start is upper left corner at 0,0
- exit is bottom right corner at 70,70

- "bytes" fall into memory space making it "corrupted"
- . = safe # = corrupted
- one byte per tick
- map safe path through maze
*)
let parse (input: string seq) =
    input
    |> Seq.map (fun s ->
        let matches = Regex.Matches(s, "(\d+)") |> Seq.map (fun n -> int n.Value) |> Array.ofSeq
        matches[0], matches[1]
    )

let memoryState (coords: (int * int) seq) (size: int) (ticks: int)=
    let grid = Array2D.create (size+1) (size+1) true

    coords
    |> Seq.take ticks
    |> Seq.iter (fun (x, y) -> grid[x,y] <- false)
    
    grid

let findShortestPath (memory: bool[,]) (size: int) =
    let pq = PriorityQueue<int * int, int>() // key: x,y; value: steps
    let visited = Dictionary<int * int, int>() // x,y -> steps

    let neighborIsValid (x: int, y:int) = x >= 0 && x <= size && y >=0 && y <= size && memory[x,y]
    
    let neighbors (x:int, y:int) =
        [(-1,0); (1,0); (0,-1); (0,1)]
        |> Seq.map (fun (dx,dy) -> x + dx, y + dy)
        |> Seq.filter neighborIsValid

    let rec dijkstra () =
        match pq.TryDequeue() with
        | true, (x, y), current ->
            if not (visited.ContainsKey(x,y)) || current < visited[(x,y)] then
                if visited.ContainsKey(x,y)
                then visited[(x,y)] <- current
                else visited.Add((x,y), current)

                if (x,y) = (size,size) then ()
                else
                    neighbors (x,y) |> Seq.iter (fun loc -> pq.Enqueue(loc, current + 1))

            dijkstra()
        | _ -> ()

    pq.Enqueue((0,0), 0)
    dijkstra()

    visited[size,size]

let run () =
    let input = File.ReadAllLines "./Input/day18.txt"
    let size = 70
    let ticks = 1024

    let coords = parse input
    let grid = memoryState coords size ticks

    findShortestPath grid size |> printfn "[Day 16] Part 1: %d"
    // ?? |> printfn "[Day 16] Part 2: %d"
    ()