module Day?

open System.IO

let part1 (input:string seq) = 0

let part2 (input:string seq) = 0

let run () =
    let input = File.ReadAllLines "./Input/day?.txt"

    part1 input
    |> printfn "[Day ?] Part 1: %d"

    part2 input
    |> printfn "[Day ?] Part 2: %d"
