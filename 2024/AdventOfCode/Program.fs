open Day9.Part1
open Day9.Part2

let input = System.IO.File.ReadAllLines "./Input/day9.txt"

part1(input |> Seq.head)
|> printfn "part1: %d"

part2(input |> Seq.head)
|> printfn "part2: %d"
