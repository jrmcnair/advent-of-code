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
    let findMaxDigit (input:string) : char * int =
        let maxDigit = input |> Seq.max
        let index = input.IndexOf(maxDigit) + 1
        
        maxDigit, index

    let rec getMaxJoltage (batteryCount: int) (bank: string) : int64 =
        let rec worker (joltage: string) (input: string) (batteryCount: int) : string =
            if batteryCount = 0 then
                joltage
            else
                let digit, index = input.Substring(0, input.Length - batteryCount + 1) |> findMaxDigit
                worker $"{joltage}{digit}" (input.Substring(index, input.Length - index)) (batteryCount - 1)
        
        worker "" bank batteryCount |> int64

    let run () =
        data
        |> Seq.map (getMaxJoltage 12)
        |> Seq.sum
        |> printfn "Part2: Max Joltage = %d"
