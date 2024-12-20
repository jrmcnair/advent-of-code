
// https://adventofcode.com/2024/day/20
module Day20

open System.Collections.Generic
open System.IO
open AdventOfCode.Common

let parse (input: string seq) : char[,] = Array2D.ofChars id input

let walkPath (track: char[,]) (start: int * int) =
    let pq = PriorityQueue<int * int, int>()
    let visited = Dictionary<int * int, int>()
    let finish = track |> Array2D.findIndex (fun x -> x = 'E')

    let rec dijkstra () =
        match pq.TryDequeue() with
        | false, _, _ -> ()
        | true, pos, time when pos = finish -> visited.Add(pos, time)
        | true, pos, _ when visited.Keys.Contains(pos) -> dijkstra ()
        | true, (row, col), time ->
            visited.Add((row, col), time)

            Array2D.cardinalNeighbors (row, col) track
            |> Seq.choose(fun (loc, v) -> if v <> '#' then Some loc else None)
            |> Seq.iter(fun next -> pq.Enqueue(next, time + 1))
            
            dijkstra ()

    pq.Enqueue(start, 0)
    dijkstra()

    visited

let possibleCheats (track: char[,]) (maxCheats: int) (stepsTo: Dictionary<int * int, int>) =
    let fromPoint (r: int, c:int) =
        let deltas = [| 0..maxCheats |]

        Array.allPairs deltas deltas
        |> Seq.filter (fun (x, y) -> x + y <= maxCheats && x + y > 0)
        |> Seq.collect (fun (x, y) -> [| (r - x, c - y); (r - x, c + y); (r + x, c - y); (r + x, c + y) |])
        |> Seq.filter (fun (x, y) -> Array2D.isValid track (x, y) && (track[x, y] = '.' || track[x, y] = 'E'))
        |> Seq.distinct
        |> Set.ofSeq

    stepsTo.Keys |> Seq.map(fun pos -> pos, fromPoint pos) |> dict

let cheatCount (track: char[,]) (maxCheats: int) (timeSaved: int) (stepsTo: Dictionary<int * int, int>) =
    let distance (r1: int, c1: int) (r2: int, c2: int) =
        abs(r2 - r1) + abs(c2 - c1)        

    let toSavings start finish =
        let cheat = distance start finish
        let savings = stepsTo[finish] - stepsTo[start] - cheat

        if stepsTo[finish] - stepsTo[start] > cheat && savings >= timeSaved
        then Some savings else None

    possibleCheats track maxCheats stepsTo
    |> List.ofSeq
    |> List.map(fun kvp -> kvp.Value |> Seq.choose (toSavings kvp.Key))
    |> Seq.concat
    |> Seq.length

let race (input: string seq) (maxCheats: int) (timeSaved: int) =
    let track = parse input
    let start = track |> Array2D.findIndex(fun x -> x = 'S')

    walkPath track start
    |> cheatCount track maxCheats timeSaved

let run () =
    let input = File.ReadAllLines "./Input/day20.txt"
    let timeSaved = 100

    race input 2 timeSaved |> printfn "[Day 20] Part 1: %d" // 1387
    race input 20 timeSaved |> printfn "[Day 20] Part 1: %d" // 1015092
