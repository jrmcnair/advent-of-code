module AdventOfCode2025.Day2

open System.IO

type IdRange = {
    Start: int
    End: int
}

// TODO: when to convert them to int?
let parseData (input: string) : IdRange[] =
    input.Split(",", System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun input ->
        let parts = input.Split("-")
        { Start = int parts.[0]; End = int parts.[1] })

let data : IdRange[] =
    "./input/day2.txt"
    |> File.ReadAllText
    |> parseData

module Part1 =
    let isInvalidProductId (productId: int): bool =
        let stringDigits = productId.ToString()
        let numDigits = stringDigits.Length
        
        if numDigits % 2 = 1 then
            false
        else
            let left = stringDigits.Substring(0, numDigits / 2)
            let right = stringDigits.Substring(numDigits / 2, numDigits / 2)
            left = right
    
    let processRange (range: IdRange) : Set<int> =

        //if number of digits is odd, can't be invalid
        set []

    let getInvalidIds (ranges: IdRange[]): Set<int> =
        set []

    let run () =
        data
        |> getInvalidIds
        |> Set.fold(+) 0
        |> printfn "Part1: Sum of Invalid Ids = %d"
