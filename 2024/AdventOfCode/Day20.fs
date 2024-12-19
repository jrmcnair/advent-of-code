
// https://adventofcode.com/2024/day/20
module Day20

open System.Collections.Generic
open System.IO
open AdventOfCode.Common

let parse (input: string seq) : char[,] = Array2D.ofChars id input

let race (track: char[,]) =
    let pq = PriorityQueue<int * int, int>() // key: row,col; value: picoseconds
    let visited = HashSet<int * int>() // row,col

    let startPos = track |> Array2D.findIndex 'S'
    let endPos = track |> Array2D.findIndex 'E'
    
    let neighbors (row:int, col:int) =
        [(row - 1, col); (row + 1, col); (row, col - 1); (row, col + 1)]
        |> Seq.choose (fun x ->
            Array2D.tryGet x track
            |> Option.bind(fun y -> if y <> '#' then Some x else None))

    let rec dijkstra () =
        match pq.TryDequeue() with
        | false, _, _ -> None
        | true, pos, time when pos = endPos -> Some time
        | true, pos, _ when visited.Contains(pos) -> dijkstra ()
        | true, pos, time ->
            visited.Add(pos) |> ignore
            
            neighbors pos
            |> Seq.iter(fun next -> pq.Enqueue(next, time + 1))

            dijkstra ()

    pq.Enqueue(startPos, 0)
    dijkstra()

let interiorWalls (track: char[,]) =
    let maxRow = Array2D.length1 track - 1
    let maxCol = Array2D.length2 track - 1

    let isValid (r: int) (c:int) (v:char) = v = '#' && r > 0 && r < maxRow && c > 0 && c < maxCol
    let canCross (r:int) (c:int) = (track[r-1, c] <> '#' && track[r+1, c] <> '#') || (track[r, c-1] <> '#' && track[r, c+1] <> '#')
    
    track
    |> Array2D.mapi (fun r c v -> if isValid r c v && canCross r c then Some (r, c) else None)
    |> Seq.cast<(int * int) option>
    |> Seq.choose id

let part1 (track: char[,]) (timeSaved: int) : int =
    let fairTime = race track |> Option.get

    // going to be a trick where a cheat square works in multiple directions?
    // do I need to only pick walls along the path?  i.e. are there sections of the grid unused?
    // can I alter the maze as I go through in a single recursion?

    interiorWalls track
    |> Seq.choose(fun (wr, wc) ->
         let cheatTrack = Array2D.copy track
         cheatTrack[wr, wc] <- '.'
         race cheatTrack)
    |> Seq.filter (fun time -> time <= (fairTime - timeSaved))
    |> Seq.length

let part2 () = ()

let run () =
    let track = File.ReadAllLines "./Input/day20.txt" |> parse
    let picoseconds = 100

    part1 track picoseconds |> printfn "[Day 20] Part 1: %d"
    part2 () |> printfn "[Day 20] Part 2: %A"
