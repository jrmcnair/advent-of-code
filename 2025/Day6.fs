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
    let parse (size: int) (input: string[]) : Problem[] =
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
        let chars =
            [| 0 .. input.Length - 2 |]
            |> Array.map (fun row -> input[row][col])
            |> Array.filter (fun c -> not (System.Char.IsWhiteSpace c))
        let str = new string(chars)
        
        str |> int64

    let parse (input: string[]) : Problem list =
        let operations = input[input.Length - 1]

        let findNextOperation (start: int) : int option =
            if start >= operations.Length then
                None
            else
                operations
                |> Seq.skip start
                |> Seq.indexed
                |> Seq.tryFind (fun (_, c) -> not (System.Char.IsWhiteSpace c))
                |> Option.map (fun (idx, _) -> idx + start)
        
        let rec loop (problems: Problem list) (col: int) : Problem list =
            match findNextOperation (col + 1) with
            | Some next ->
                let problem =
                    { Numbers = [col..next - 2] |> List.map (getNumber input) |> List.toArray
                      Operation = operations[col] |> Problem.toOperation }

                loop (problem::problems) next
            | None ->
                let problem =
                    { Numbers = [col .. operations.Length - 1] |> List.map (getNumber input) |> List.toArray
                      Operation = operations[col] |> Problem.toOperation }
                problem::problems

        loop [] 0

    let run () =
        let problems =
            "./input/day6.txt"
            |> File.ReadAllLines
            |> parse

        problems
        |> List.map Problem.solve
        |> List.sum
        |> printfn "Part2: %d"
