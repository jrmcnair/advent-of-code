module AdventOfCode2025.Day1

open System.IO

// https://adventofcode.com/2025/day/1

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
    let executeRotation (start:int) (rotation: Rotation): int * int =
        match rotation.Direction with
        | Right ->
            let next = start + rotation.Distance
            next % 100, next / 100
        | Left ->
            let total = start - rotation.Distance
            let initialCount = abs(total / 100)
            let next =
                if total % 100 < 0 then
                    100 + (total % 100)
                else
                    total % 100

            let finalCount =
                if start = 0 then
                    initialCount
                else if next = 0 || total < 0 then
                    initialCount + 1
                else
                    initialCount
            
            next, finalCount

    let getPassword (rotations: Rotation[]): int =
        rotations
        |> Array.fold (fun (position, counter) rotation ->
             let next, passes = executeRotation position rotation
             (next, counter + passes)
             ) (50, 0)
        |> snd
    
    let run () =
        data
        |> getPassword
        |> printfn "Part2: Password = %d"
