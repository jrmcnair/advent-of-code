// https://adventofcode.com/2024/day/22
module Day22

open System.IO

let parse (input: string seq) = input |> Seq.map int64

let initialSecrets = File.ReadAllLines "./Input/day22.txt" |> parse

let mix (current: int64) (value: int64) = current ^^^ value

let prune (value: int64) = value % 16_777_216L

let next (current: int64) =
    let step1 = mix current (current * 64L) |> prune
    let step2 = mix step1 (step1 / 32L) |> prune
    mix step2 (step2 * 2048L) |> prune

let rec getSecret (count: int) (current: int64) =
    let secret = next current

    if count = 1 then secret
    else getSecret (count - 1) secret

let part1 () =
    initialSecrets
    |> Seq.map (getSecret 2000)
    |> Seq.sum
    |> printfn "[Day 22] Part 1: %d" // 106551420 too low

let part2 () = ()
