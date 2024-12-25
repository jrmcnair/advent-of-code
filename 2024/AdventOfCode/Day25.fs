// https://adventofcode.com/2024/day/25
module Day25

open System
open System.Collections.Generic
open System.IO
open System.Text.Json
open System.Text.RegularExpressions

(*
schematics of locks and keys
locks have top row filled (#) and bottom row empty (.)
keys have top row empty (.) and bottom row filled (#)

each schematic is a list of heights, one per column so
    extending downward from top for locks, upward from bottom for keys

            #####
            .####  =  0,5,3,4,3
            .####
            .####
            .#.#.
            .#...
            .....

            .....
            #....  =  5,0,2,1,3
            #....
            #...#
            #.#.#
            #.###
            #####

Those don't fit, because the last column 3 + 3 > 5
*)

type Schematic = | Key | Lock

let parseSchematic (schematic: string[]) =
    let counts =
        [| 0..4 |]
        |> Array.map (fun col ->
            [1..5]
            |> Seq.filter (fun row -> schematic[row][col] = '#')
            |> Seq.length)

    let schematicType = if schematic[0][0] = '#' then Lock else Key
    
    schematicType, counts

let parse (input: string seq) =
    input
    |> Seq.chunkBySize 8
    |> Seq.map parseSchematic
    |> Seq.groupBy fst
    |> Seq.map(fun (st, x) -> st, Seq.map snd x |> List.ofSeq)
    |> dict

let keyFitsLock (key: int[]) (lock: int[]) =
    [0..4]
    |> Seq.map (fun i -> lock[i] + key[i])
    |> Seq.filter (fun count -> count <= 5)
    |> Seq.length
    |> fun l -> l = 5

let input = File.ReadAllLines "./Input/day25.txt"
let schematics = parse input

let countKeysThatFit (schematics: IDictionary<Schematic, int[] list>) =
    let mutable count = 0

    for lock in schematics[Lock] do
        for key in schematics[Key] do
            if keyFitsLock key lock then count <- count + 1

    count

let part1 () =
    countKeysThatFit schematics
    |> printfn "[Day 25] Part 1: %d"

let part2 () =
    printfn "[Day 25] Part 2: Did you complete all the stars?"
