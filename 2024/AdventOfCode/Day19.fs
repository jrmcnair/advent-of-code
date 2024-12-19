// https://adventofcode.com/2024/day/19
module Day19

open System.Collections.Generic
open System.IO

let parse (input: string seq) : string list * string seq =
    let patterns = (Seq.head input).Split(',') |> Seq.map _.Trim() |> List.ofSeq
    let designs = input |> Seq.tail |> Seq.tail

    patterns, designs

let part1 (patterns: string list) (designs: string seq) : int =
    let rec isPossible (design: string) : bool =
        if design.Length = 0 then true
        else
            patterns
            |> Seq.tryFind (fun p -> design.StartsWith p && design.Substring(p.Length) |> isPossible)
            |> Option.isSome

    designs
    |> Seq.filter isPossible
    |> Seq.length

let part2 (patterns: string list) (designs: string seq) : int64 =
    let seen = Dictionary<string, int64>()

    let rec isPossible (design: string) : int64 =
        if seen.ContainsKey(design) then seen[design]
        else
            let result =
                if design.Length = 0 then 1L
                else
                    patterns
                    |> Seq.map (fun p ->
                        if design.StartsWith p
                        then design.Substring(p.Length) |> isPossible
                        else 0L)
                    |> Seq.sum
            seen.Add(design, result)
            result

    designs
    |> Seq.map isPossible
    |> Seq.sum

let run () =
    let input = File.ReadAllLines "./Input/day19.txt"
    let patterns, designs = parse input

    part1 patterns designs
    |> printfn "[Day 19] Part 1: %d"
    
    part2 patterns designs
    |> printfn "[Day 19] Part 2: %d"
