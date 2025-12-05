module AdventOfCode2025.Day4

open System.IO

// https://adventofcode.com/2025/day/4

let parseInput (input: string seq) : bool[,] =
    input
    |> Seq.map (Seq.map ((=) '@'))
    |> array2D

let data =
    "./input/day4.txt"
    |> File.ReadLines
    |> parseInput

let getNeighborCoords (grid: 'T[,]) (r: int, c: int) : (int * int) seq =
    // seq {
    //     for dr in -1 .. 1 do
    //         for dc in -1 .. 1 do
    //             if dr <> 0 || dc <> 0 then
    //                 yield (r + dr, c + dc)
    // }

    [ for dr in [-1..1] do
      for dc in [-1..1] do
        if not (dr = 0 && dc = 0) then
            let nr, nc = r + dr, c + dc
            if nr >= 0 && nr < Array2D.length1 grid &&
               nc >= 0 && nc < Array2D.length2 grid then
                yield nr, nc ]



module Part1 =
    let run () =
        1
        |> printfn "Part1: TBD = %d"

module Part2 =
    let run () =
        2
        |> printfn "Part2: TBD = %d"
