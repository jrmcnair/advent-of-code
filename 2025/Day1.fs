module AdventOfCode2025.Day1

open System.IO

type Rotation = {
    Direction:  Direction
    Distance: int
}
and Direction = | Left | Right

let parseData (input: string[]) : Rotation[] =
    input
    |> Array.map (fun line ->
        let dir =
            match line.[0] with
            | 'L' -> Left
            | 'R' -> Right
            | _ -> failwith "Invalid direction"
        let dist = line.[1..] |> int
        { Direction = dir; Distance = dist })

let data : Rotation[] =
    "./input/day1.txt"
    |> File.ReadLines
    |> Array.ofSeq
    |> parseData

module Part1 =
    let getPassword (rotations: Rotation[]): int =
        rotations
        |> Array.fold (fun (position, counter) rotation ->
            let next =
                match rotation.Direction with
                | Right -> (position + rotation.Distance) % 100
                | Left -> (position - rotation.Distance) % 100

            (next, if next = 0 then counter + 1 else counter)
        ) (50, 0)
        |> snd

    let run () =
        data
        |> getPassword
        |> printfn "Part1: Password = %d"

module Part2 =
    let executeRotation (start:int) (rotation: Rotation): int =
        let cycles = rotation.Distance / 100

        match rotation.Direction with
        | Right -> start + rotation.Distance
        | Left -> start - rotation.Distance

    let getPassword (rotations: Rotation[]): int =
        // Implementation for Part 2 would go here
        0
    
    let run () =
        // Implementation for Part 2 would go here
        printfn "Part2: Not implemented yet"