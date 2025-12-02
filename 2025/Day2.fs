module AdventOfCode2025.Day2

open System.IO

type IdRange = {
    Start: int64
    End: int64
}

// TODO: when to convert them to int?
let parseData (input: string) : IdRange[] =
    input.Split(",", System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun input ->
        let parts = input.Split("-")
        { Start = int64 parts.[0]; End = int64 parts.[1] })

let data : IdRange[] =
    "./input/day2.txt"
    |> File.ReadAllText
    |> parseData

module Part1 =
    let isInvalidProductId (productId: int64): int64 option =
        let stringDigits = productId.ToString()
        let numDigits = stringDigits.Length
        
        if numDigits % 2 = 1 then
            None
        else
            let left = stringDigits.Substring(0, numDigits / 2)
            let right = stringDigits.Substring(numDigits / 2, numDigits / 2)
            if left = right then
                Some productId
            else
                None
    
    let processRange (range: IdRange) =
        seq {range.Start..range.End}
        |> Seq.choose isInvalidProductId
        |> Seq.fold (+) 0L

    let run () =
        data
        |> Array.map processRange
        |> Array.sum
        |> printfn "Part1: Sum of Invalid Ids = %d"
