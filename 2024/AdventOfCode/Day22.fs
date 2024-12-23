// https://adventofcode.com/2024/day/22
module Day22

open System.Collections.Generic
open System.IO

let parse (input: string seq) = input |> Seq.map int64

let mix (current: int64) (value: int64) = current ^^^ value

let prune (value: int64) = value % 16_777_216L

let price (secret: int64) = secret % 10L |> int

let next (current: int64) =
    let step1 = mix current (current * 64L) |> prune
    let step2 = mix step1 (step1 / 32L) |> prune
    mix step2 (step2 * 2048L) |> prune

let rec getSecret (count: int) (current: int64) =
    let secret = next current

    if count = 1 then secret
    else getSecret (count - 1) secret

let getPrices (iterations: int) (secret: int64) =
    let prices = Array.create iterations 0
    let mutable n = secret

    for i in 0 .. (iterations - 1) do
        prices[i] <- n |> price
        n <- next n
        
    prices

let getPriceChanges (prices: int[]) =
    prices
    |> Array.pairwise
    |> Array.map(fun (p1, p2) -> p2, p2 - p1)

let applySequences (sequences: Dictionary<int * int * int * int, int>) (changes: (int * int)[]) =
    let seen = HashSet<int * int * int * int>()
    for i in 3 .. changes.Length - 1 do
        let key = snd changes[i-3], snd changes[i-2], snd changes[i-1], snd changes[i]
        if not (seen.Contains(key)) then
            seen.Add(key) |> ignore
            if sequences.ContainsKey(key) then sequences[key] <- sequences[key] + (fst changes[i])
            else sequences.Add(key, fst changes[i])

let initialSecrets = File.ReadAllLines "./Input/day22.txt" |> parse

let part1 () =

    initialSecrets
    |> Seq.map (getSecret 2000)
    |> Seq.sum
    |> printfn "[Day 22] Part 1: %d" // 15608699004

let part2 () =
    let sequences = Dictionary<int * int * int * int, int>()

    initialSecrets
    |> Seq.iter (getPrices 2000 >> getPriceChanges >> applySequences sequences)

    sequences.Values
    |> Seq.sortDescending
    |> Seq.head
    |> printfn "[Day 22] Part 2: %d" // 7448 too high
