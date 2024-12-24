// https://adventofcode.com/2024/day/23
module Day23

open System.Collections.Generic
open System.IO

let parse (input: string seq) =
    input
    |> Seq.map (fun x ->
        let computers = x.Split('-')
        computers[0], computers[1])

let toGraph (pairs: (string * string) seq) =
    let x = Dictionary<string, Set<string>>()

    pairs 
    |> Seq.iter (fun (c1, c2) ->
        if x.ContainsKey(c1) then x[c1] <- x[c1].Add(c2) else x.Add(c1, set [ c2 ])
        if x.ContainsKey(c2) then x[c2] <- x[c2].Add(c1) else x.Add(c2, set [ c1 ]))

    x

let triangles (graph: Dictionary<string, Set<string>>) =
    let possibles = graph |> Seq.filter (fun p -> p.Key.StartsWith "t")

    seq {
        for p in possibles do
            for c in p.Value do
                for gc in graph[c] do
                    if graph[gc].Contains(p.Key) then
                        yield Set([p.Key; c; gc])
    } |> Seq.distinct

// https://en.wikipedia.org/wiki/Bron%E2%80%93Kerbosch_algorithm
// adapted from https://fssnip.net/jg/title/BronKerbosch-maximal-cliques-algorithm
let rec bronKerbosch R P X (graph: Dictionary<string, Set<string>>) =
    seq {
        if (Set.isEmpty P) && (Set.isEmpty X) then
          yield (Set.toSeq R)
        let vPX =
            Seq.unfold
                (function
                | (v::tailP as P, currX) ->
                    let newX = Set.add v currX
                    Some((v, set <| P, currX), (tailP, newX))
                | ([], _) -> None)
                (P |> Set.toList, X)
        for (v, P, X) in vPX do
            let n = graph[v]
            yield! bronKerbosch (Set.add v R) (Set.intersect P n) (Set.intersect X n) graph
    }

let input = File.ReadAllLines "./Input/day23.txt"
let graph = input |> parse |> toGraph

let part1 () =
    graph
    |> triangles
    |> Seq.length
    |> printfn "[Day 23] Part 1: %d"// 1110

let part2 () =
    let keys = graph.Keys |> Set.ofSeq
    bronKerbosch Set.empty keys Set.empty graph
    |> Seq.sortByDescending Seq.length
    |> Seq.head
    |> Seq.sort
    |> String.concat ","
    |> printfn "[Day 23] Part 2: %s"
