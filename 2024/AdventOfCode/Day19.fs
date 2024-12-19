// https://adventofcode.com/2024/day/19
module Day19

open System.IO

let parse (input: string seq) : string list * string seq =
    let patterns = (Seq.head input).Split(',') |> Seq.map _.Trim() |> List.ofSeq
    let designs = input |> Seq.tail |> Seq.tail

    patterns, designs

let rec isPossible (patterns: string list) (design: string) : bool =
    if design.Length = 0 then true
    else
        patterns
        |> Seq.tryFind (fun p -> design.StartsWith p && design.Substring(p.Length) |> isPossible patterns)
        |> Option.isSome

let part1 (patterns: string list) (designs: string seq) : int =
    designs
    |> Seq.filter (isPossible patterns)
    |> Seq.length

let part2 () = ()

let run () =
    let input = File.ReadAllLines "./Input/day19.txt"
    let patterns, designs = parse input

    part1 patterns designs
    |> printfn "[Day 19] Part 1: %d"
    
    part2 ()
    |> printfn "[Day 19] Part 2: %A"
