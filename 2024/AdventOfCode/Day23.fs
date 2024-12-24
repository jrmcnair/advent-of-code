// https://adventofcode.com/2024/day/23
module Day23

open System.Collections.Generic
open System.IO

let parse (input: string seq) =
    input
    |> Seq.map (fun x ->
        let computers = x.Split('-')
        computers[0], computers[1])

let connections (pairs: (string * string) seq) =
    let x = Dictionary<string, HashSet<string>>()

    pairs 
    |> Seq.iter (fun (c1, c2) ->
        if x.ContainsKey(c1) then x[c1].Add(c2) |> ignore else x.Add(c1, HashSet([ c2 ]))
        if x.ContainsKey(c2) then x[c2].Add(c1) |> ignore else x.Add(c2, HashSet([ c1])))

    x

let triangles (connections: Dictionary<string, HashSet<string>>) =
    let possibles = connections |> Seq.filter (fun p -> p.Key.StartsWith "t")

    seq {
        for p in possibles do
            for c in p.Value do
                for gc in connections[c] do
                    if connections[gc].Contains(p.Key) then
                        yield Set([p.Key; c; gc])
    } |> Seq.distinct

let input = File.ReadAllLines "./Input/day23.txt"

let part1 () =
    parse input
    |> connections
    |> triangles
    |> Seq.length
    |> printfn "[Day 23] Part 1: %d"// 1110

let part2 () =
    0
    |> printfn "[Day 23] Part 2: %d"
