module AdventOfCode2025.Day3

open System.IO

let data =
    "./input/day3.txt"
    |> File.ReadLines

module Part1 =
    let getMaxJoltage (bank: string) : int =
        let first = bank.Substring(0, bank.Length - 1) |> Seq.max
        let index = bank.IndexOf(first) + 1
        let second = bank.Substring(index, bank.Length - index) |> Seq.max

        $"{first}{second}" |> int
        
    let run () =
        data
        |> Seq.map getMaxJoltage
        |> Seq.sum
        |> printfn "Part1: Max Joltage = %d"

module Part2 =
    let run () =
        2
        |> printfn "Part2: TBD = %d"
