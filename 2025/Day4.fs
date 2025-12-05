module AdventOfCode2025.Day4

open System.IO

// https://adventofcode.com/2025/day/4

module Grid =
    let coords grid =
        [ for r in [0 .. Array2D.length1 grid - 1] do
          for c in [0 .. Array2D.length2 grid - 1] do
            yield r, c ]

    let cells grid =
        coords grid |> List.map (fun (r, c) -> (r, c, grid[r,c]))

    let neighbors (grid: 'T[,]) (r: int, c: int) : 'T seq =
        seq {
          for dr in [-1..1] do
          for dc in [-1..1] do
            if not (dr = 0 && dc = 0) then
                let nr, nc = r + dr, c + dc
                if nr >= 0 && nr < Array2D.length1 grid &&
                   nc >= 0 && nc < Array2D.length2 grid then
                    yield grid[nr, nc]
        }

let parseInput (input: string seq) : bool[,] =
    input
    |> Seq.map (Seq.map ((=) '@'))
    |> array2D

let data =
    "./input/day4.txt"
    |> File.ReadLines
    |> parseInput

let getNeighborPaperCount (grid: bool[,]) (r: int, c: int) : int =
    Grid.neighbors grid (r,c)
    |> Seq.filter id
    |> Seq.length

let accessibleRolls (grid: bool[,]) : (int * int * bool) list =
    grid
    |> Grid.cells
    |> List.filter(fun (r, c, isPaper) ->
        isPaper && getNeighborPaperCount grid (r,c) < 4)

module Part1 =
            
    let run () =
        data
        |> accessibleRolls
        |> List.length
        |> printfn "Part1: Accessible Rolls of Paper = %d"

module Part2 =
    let removeAccessibleRolls (grid: bool[,]) (accessibleRolls: (int * int * bool) list) : bool[,] =
        accessibleRolls
        |> List.iter (fun (r, c, _) -> grid[r,c] <- false)
        
        grid

    let rec loop (counter: int) (grid: bool[,]) : int =
        let accessibleRolls = accessibleRolls grid
        if List.isEmpty accessibleRolls then
            counter
        else
            accessibleRolls
            |> removeAccessibleRolls grid
            |> loop (counter + List.length accessibleRolls)
    
    let run () =
        loop 0 data
        |> printfn "Part2: Total Rolls Removed = %d"
