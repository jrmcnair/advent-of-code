module AdventOfCode2025.Day6

open System.IO

type Problem =
    { Numbers: int64 []
      Operation: Operation }
and Operation =
    | Add
    | Multiply

module Problem =
    let solve (problem: Problem) : int64 =
        match problem.Operation with
        | Add -> Array.sum problem.Numbers
        | Multiply -> Array.fold (*) 1L problem.Numbers
    
    let toOperation = function
        | '+' -> Add
        | '*' -> Multiply
        | _ -> failwith "Unknown operation"

module Part1 =
    let parse (size: int) (input: string[]) : Problem array =
        let numbers =
            input[0..size-1]
            |> Array.map (fun (line: string) ->
                line.Split(" ", System.StringSplitOptions.RemoveEmptyEntries)
                |> Array.map int64)

        let operations =
            input[size]
            |> _.Split(" ", System.StringSplitOptions.RemoveEmptyEntries)
            |> Array.map (function
                | "+" -> Add
                | "*" -> Multiply
                | _ -> failwith "Unknown operation")
            
        [| 0 .. operations.Length - 1 |]
        |> Array.map (fun col ->
            { Numbers = Array.init size (fun row -> numbers[row][col])
              Operation = operations[col] })

    let run () =
        let problems =
            "./input/day6.txt"
            |> File.ReadAllLines
            |> parse 4

        problems
        |> Array.map Problem.solve
        |> Array.sum
        |> printfn "Part1: %d"

module Part2 =
    let getNumber (input: string[]) (col: int) =
        [| 0 .. input.Length - 1 |]
        |> Array.map (fun row -> input[row][col])
        |> string
        |> int64

    let parse (size: int) (input: string[]) : Problem array =
        let operations = input[size]

        let loop (problems: Problem[]) (col: int) : Problem[] =
            let operation = operations[col] |> Problem.toOperation

            operations.
            
                        // find next operation
            
            let x =
                { Numbers = [|  |]
                    // [| 0..size-1 |]
                    // |> Array.map (fun row -> input[row][col])
                    // |> string
                    // |> int64
                  Operation = Problem.toOperation (input[size][col]) }
            // get operation at idx
            // starting at idx, get number at each char until we reach next operation or end of line
            
            [| x |]

        loop [] 0

    let run () =
        0
        |> printfn "Part2: %d"
