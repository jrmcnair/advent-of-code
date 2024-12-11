module Day7

open System.IO
open System.Text.RegularExpressions

[<Struct>]
[<StructuredFormatDisplay("{Result}: {Numbers}")>]
type Equation (result: int64, numbers: int64[]) =
    member _.Result = result
    member _.Numbers = numbers

let parse (line:string) =
    let matches = Regex.Matches(line, "\d+")
    let result = (Seq.head matches).Value |> int64
    let numbers = (Seq.tail matches) |> Seq.map (fun x -> x.Value |> int64) |> Array.ofSeq
    Equation(result, numbers)
    
let testEquationV1 (equation: Equation) =
    equation.Numbers
    |> Array.fold (fun current next ->
        if List.isEmpty current
        then [ next ]
        else current |> List.map (fun x -> [ next + x; next * x ]) |> List.concat
        ) []
    |> List.tryPick (fun x -> if x = equation.Result then Some equation.Result else None)

let part1 input =
    input
    |> Seq.map testEquationV1
    |> Seq.choose id
    |> Seq.sum

let testEquationV2 (equation: Equation) =
    equation.Numbers
    |> Array.fold (fun current next ->
        if List.isEmpty current
        then [ next ]
        else current |> List.map (fun x -> [ next + x; next * x; $"{x}{next}" |> int64 ]) |> List.concat
        ) []
    |> List.tryPick (fun x -> if x = equation.Result then Some equation.Result else None)

let part2 input =
    input
    |> Seq.map testEquationV2
    |> Seq.choose id
    |> Seq.sum

let run () =
    let input =
        File.ReadAllLines "./Input/day7.txt"
        |> Seq.map parse


    part1 input
    |> printfn "[Day 7] Part 1: %d"

    part2 input
    |> printfn "[Day 7] Part 2: %d"