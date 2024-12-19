
// https://adventofcode.com/2024/day/18
module Day18

open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions

let parse (input: string seq) =
    input
    |> Seq.map (fun s ->
        let matches = Regex.Matches(s, "(\d+)") |> Seq.map (fun n -> int n.Value) |> Array.ofSeq
        matches[0], matches[1])
    |> List.ofSeq

let memoryState (coords: (int * int) list) (size: int) (ticks: int)=
    let grid = Array2D.create (size+1) (size+1) true

    let x = coords |> List.take ticks // last item is coords[ticks - 1]
    coords
    |> List.take ticks
    |> List.iter (fun (x, y) -> grid[x,y] <- false)
    
    grid

let shortestPath (size: int) (memory: bool[,]) =
    let pq = PriorityQueue<int * int, int>() // key: x,y; value: steps
    let visited = HashSet<int * int>()

    let neighborIsValid (x: int, y:int) = x >= 0 && x <= size && y >=0 && y <= size && memory[x,y]
    
    let neighbors (x:int, y:int) =
        [(x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1)]
        |> Seq.filter neighborIsValid

    let rec dijkstra () =
        match pq.TryDequeue() with
        | false, _, _ -> None
        | true, (x, y), current when (x, y) = (size, size) -> Some current
        | true, (x, y), _ when visited.Contains(x,y) -> dijkstra ()
        | true, (x, y), current ->
            visited.Add(x, y) |> ignore
            
            neighbors (x, y)
            |> Seq.iter(fun (nx, ny) -> pq.Enqueue((nx, ny), current + 1))
            dijkstra ()

    pq.Enqueue((0,0), 0)
    dijkstra()

let findBlockingCoord (coords: (int * int) list) (size: int) =
    let start = 0
    let max = coords.Length - 1

    let rec search low high =
        if high - low <= 1 then high
        else
            let ticks = (low + high) / 2
            let isBlocked = memoryState coords size ticks |> shortestPath size |> Option.isNone
            if isBlocked then search low ticks
            else search ticks high

    let ticks = search start max
    coords[ticks - 1]

let run () =
    let input = File.ReadAllLines "./Input/day18.txt"
    let size = 70
    let ticks = 1024

    let coords = parse input
    let grid = memoryState coords size ticks

    shortestPath size grid |> Option.get |> printfn "[Day 18] Part 1: %d"
    findBlockingCoord coords size |> printfn "[Day 18] Part 2: %A"
