module Day11

open System

let digitsAreEven (n: uint64) =
    n.ToString().Length % 2 = 0

let splitStone (stone: uint64) =
    let s = stone.ToString()
    let l = s.Length/2
    UInt64.Parse(s.Substring(0,l)), UInt64.Parse(s.Substring(l))

let parse (input: string) =
    input.Split(" ")
    |> Seq.choose (fun x -> Some (x |> string |> uint64))
    |> Seq.groupBy id
    |> Seq.map (fun (n, l) -> n, Seq.length l |> uint64)
    |> Map.ofSeq

let blinkStone (stone: uint64) =
    match stone with
    | 0UL -> seq { 1UL }
    | s when digitsAreEven s ->
        let (s1, s2) = splitStone s
        seq { s1; s2 }
    | s -> seq { s * 2024UL }

let blink (total: int) (start: Map<uint64, uint64>) =
    let rec go (counter: int) (current: Map<uint64, uint64>) =
        if counter <= total then
            current
            |> Seq.map(fun x -> blinkStone x.Key |> Seq.map (fun y -> (y, x.Value)))
            |> Seq.concat
            |> Seq.groupBy fst
            |> Seq.map (fun (n, x) -> n, x |> Seq.map snd |> Seq.sum)
            |> Map.ofSeq
            |> go (counter + 1)
        else
            current
    
    go 1 start

let execute (input:string) (blinkCount: int) =
    let result = 
        input
        |> parse
        |> blink blinkCount
        |> Seq.map (fun x -> x.Value |> uint64)

    result |> Seq.sum
