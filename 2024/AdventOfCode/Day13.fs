module Day13

open System
open System.IO
open System.Text.RegularExpressions

type Machine = {
    A: int64 * int64
    B: int64 * int64
    Prize: int64 * int64
}
module Machine =
    let create (isPart2: bool) (ax: int, ay: int) (bx: int, by: int) (plx: int, ply: int) =
        let inc = 10000000000000L
        let ax, ay = int64 ax, int64 ay
        let bx, by = int64 bx, int64 by
        let plx, ply = if isPart2 then (int64 plx + inc, int64 ply + inc) else (int64 plx, int64 ply)
        
        { A = (ax,ay); B = (bx,by); Prize = (plx,ply) }


let parse (isPart2: bool) (input: string seq) =
    let parseLine (line: string) =
        let matches = Regex.Matches(line, "(\d+)") |> Seq.map (fun x -> int x.Value) |> Array.ofSeq
        int matches[0], int matches[1]
        
    input |> Seq.filter (fun s -> s <> "") |> Seq.chunkBySize 3 |> Seq.map(fun x ->
        Machine.create isPart2 (parseLine x[0]) (parseLine x[1]) (parseLine x[2]) )


let solve (m: Machine) : int64 option =
    let ax, ay = m.A
    let bx, by = m.B
    let px, py = m.Prize

    let a =
        let numerator = px*by - py*bx
        let denominator = ax*by - ay*bx
        if denominator = 0L then Some 0L
        else if numerator % denominator = 0L then Some (numerator/denominator)
        else None

    let b =
        let numerator = px*ay - py*ax
        let denominator = ay*bx - ax*by
        if denominator = 0L then Some 0L
        else if numerator % denominator = 0L then Some (numerator/denominator)
        else None
    
    match a, b with
    | Some 0L, Some 0L -> None
    | Some a, Some b -> Some (a * 3L + b)
    | _ -> None

let part1 (input: string seq) =
    input
    |> parse false
    |> Seq.choose solve
    |> Seq.sum

let part2 (input: string seq) =
    input
    |> parse true
    |> Seq.choose solve
    |> Seq.sum

let run () =
    let input = File.ReadAllLines "./Input/day13.txt"

    part1 input
    |> printfn "[Day 13] Part 1: %d"

    part2 input
    |> printfn "[Day 13] Part 2: %d"
