module AdventOfCode2025.Day3

open System
open System.IO

let data =
    "./input/day3.txt"
    |> File.ReadLines

module Part1 =
    let getMaxJoltage (bank: string) : int =
        let chars = bank |> Seq.toList
        
        let first = chars |> List.truncate (chars.Length - 1) |> List.max
        let index = chars |> List.findIndex ((=) first)
        let second = chars |> List.skip (index + 1) |> List.max

        $"{first}{second}" |> int
        
    let run () =
        data
        |> Seq.map getMaxJoltage
        |> Seq.sum
        |> printfn "Part1: Max Joltage = %d"

module Part2 =
    let rec getMaxJoltage (batteryCount: int) (bank: string) : int64 =
        let rec worker (acc: char list) (input: string) (remaining: int) =
            if remaining = 0 then
                acc |> List.rev |> String.Concat |> int64
            else
                let digit, index =
                    input
                    |> Seq.take (input.Length - remaining + 1)
                    |> Seq.mapi (fun idx digit -> digit, idx + 1)
                    |> Seq.maxBy fst

                worker (digit :: acc) (input.Substring(index)) (remaining - 1)
        
        worker [] bank batteryCount

    let run () =
        data
        |> Seq.map (getMaxJoltage 12)
        |> Seq.sum
        |> printfn "Part2: Max Joltage = %d"
