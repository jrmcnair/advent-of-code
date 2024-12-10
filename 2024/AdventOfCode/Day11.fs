module Day11

open System
open System.Collections.Generic

let digitsAreEven (n: uint64) =
    n.ToString().Length % 2 = 0

let splitStone (stone: uint64) =
    let s = stone.ToString()
    let l = s.Length/2
    UInt64.Parse(s.Substring(0,l)), UInt64.Parse(s.Substring(l))

module Part1 =
    let parse (input: string) =
        input.Split(" ")
        |> Seq.choose (fun x -> Some (x |> string |> uint64))
        |> LinkedList

    let blink (stones: LinkedList<uint64>) =
        let rec execute (current: LinkedListNode<uint64>) =
            if current.Value = 0UL then current.Value <- 1UL
            else
                if digitsAreEven current.Value then
                    let (n1, n2) = splitStone current.Value
            
                    stones.AddBefore(current, LinkedListNode(n1))
                    current.Value <- n2
                else
                    current.Value <- current.Value * 2024UL

            if not (isNull current.Next) then execute current.Next
            
        execute stones.First
        stones

    let execute (input:string) (blinks: int) =
        let initial = input |> parse
        
        for i = 1 to blinks do
            blink initial |> ignore

        initial.Count

let part2 (input:string) = 0