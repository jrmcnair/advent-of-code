module AdventOfCode2025.Day7

open System.IO

// type Location =
//     | Start
//     | Space
//     | Splitter

module Parse =
    // let toGrid (input: string[]) : Location[,] =
    //     input
    //     |> Array.map (fun line ->
    //         line.ToCharArray()
    //         |> Array.map (function
    //             | 'S' -> Start
    //             | '^' -> Splitter
    //             | '.' -> Space
    //             | _ -> failwith "Unknown character"))
    //     |> array2D
    let start (input: string[]) : int =
        input.[0] |> Seq.findIndex (fun c -> c = 'S')
    
    let splitters (input: string[]) : (int * int) list =
        input
        |> Array.mapi (fun row line ->
            line.ToCharArray()
            |> Array.mapi (fun col c ->
                if c = '^' then
                    Some (row, col)
                else
                    None))
        |> Array.collect id
        |> Array.choose id
        |> Array.toList
        
let input =
    "./input/day7.txt"
    |> File.ReadAllLines

module Part1 =
    let countSplits (maxRow: int) (maxCol: int) (startCol: int) (splitters: (int * int) list) : int =
        let splitBeam (col: int) : int seq =
            seq {
                if col > 0 then yield col - 1
                if col < maxCol then yield col + 1
            }

        let rec loop (count: int) (beams: Set<int>) (row: int) : int =
            if row > maxRow then
                count
            else
                let rowSplitters = splitters |> List.filter (fun (r, _) -> r = row)
                let mutable updatedCount = count
                let updatedBeams =
                    beams
                    |> Seq.collect (fun col ->
                        if rowSplitters |> List.exists (fun (_, c) -> c = col) then
                            updatedCount <- updatedCount + 1
                            splitBeam col
                        else
                            seq { col })
                    |> set
                
                loop updatedCount updatedBeams (row + 1)
 
        loop 0 (set [startCol]) 0 
        
    let run () =
        let start = input |> Parse.start
        let splitters = input |> Parse.splitters
        let maxRow = input.Length - 1
        let maxCol = input[0].Length - 1

        countSplits maxRow maxCol start splitters
        |> printfn "Part1: %d"

module Part2 =
    let run () =
        0
        |> printfn "Part2: %d"
