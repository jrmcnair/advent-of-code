module Day3

open System.IO
open System.Text.RegularExpressions

let loadData dataFile lineParser =
    dataFile
    |> File.ReadLines
    |> Seq.map lineParser
    |> Seq.concat

// -- part 1 ------------------------------------------------------------------- 

let parseLineV1 (line:string) =
    Regex.Matches(line, "mul\((\d+),(\d+)\)")
    |> Seq.map (fun m -> m.Groups.[1].Value |> int, m.Groups.[2].Value |> int)

let part1 filename =
    loadData filename parseLineV1
    |> Seq.fold (fun current (x, y) ->
        current + (x * y)
    ) 0

// -- part 2 ------------------------------------------------------------------- 

type MatchResult =
| Do
| Dont
| Multiply of int * int

let mapMatch (m: Match) : MatchResult =
    match m.Value with
    | "do()" -> MatchResult.Do
    | "don't()" -> MatchResult.Dont
    | _ -> MatchResult.Multiply (m.Groups.[1].Value |> int, m.Groups.[2].Value |> int)

let parseLineV2 (line:string) =
    Regex.Matches(line, "mul\((\d+),(\d+)\)|do\(\)|don't\(\)")
    |> Seq.map (mapMatch)

let part2 filename =
    let (_, total) =
        loadData filename parseLineV2
        |> Seq.fold (fun (current, total) next ->
            match current, next with
            | _, Do -> next, total
            | Dont, _
            | _, Dont -> MatchResult.Dont, total
            | _, Multiply (x, y) -> next, total + (x * y)
        ) (MatchResult.Do, 0)
    total

let run () =
    let filename = "./Input/day3.txt"

    part1 filename
    |> printfn "[Day 3] Part 1: %d"

    part2 filename
    |> printfn "[Day 3] Part 2: %d"
