module Day8

open System.IO

[<Struct>]
[<StructuredFormatDisplay("({Row},{Col})")>]
type Coord =
    val Row : int
    val Col : int
    new (row: int, col: int) =
        {Row = row; Col = col}

let parse (row: int) (line: string) =
    line.ToCharArray()
    |> Seq.indexed
    |> Seq.choose (fun (col, frequency) ->
        if (frequency = '.')
        then None
        else (frequency, Coord(row, col)) |> Some)

let toPairs (coords: Coord seq) =
    Seq.allPairs coords coords
    |> Seq.filter (fun (a, b) -> not (a = b))
    // TODO: filter out duplicate pairs (a,b) (b,a)

let toAntinodePair (size: int) (a1: Coord, a2: Coord) =
    let isValid (c: Coord) = c.Row >= 0 && c.Row < size && c.Col >= 0 && c.Col < size
    let rowDiff, colDiff = a1.Row - a2.Row, a1.Col - a2.Col

    seq {
        Coord(a1.Row + rowDiff, a1.Col + colDiff)
        Coord(a2.Row - rowDiff, a2.Col - colDiff)
    } |> Seq.filter isValid

let toAntinodeSeq (size: int) (a1: Coord, a2: Coord) =
    let isValid (c: Coord) = c.Row >= 0 && c.Row < size && c.Col >= 0 && c.Col < size
    let rowDiff, colDiff = a1.Row - a2.Row, a1.Col - a2.Col

    let rec execute op (l: Coord seq) (c: Coord) =
        if isValid c
        then
            let l = Coord(op c.Row rowDiff, op c.Col colDiff) |> execute op l
            Seq.append l [c]
        else l

    a1 |> execute (+) []
    |> Seq.append (a2 |> execute (-) [])

let countAntinodes (antinodes: Map<char, Coord seq>) =
    antinodes |> Map.toSeq |> Seq.map snd |> Seq.concat |> Seq.distinct |> Seq.length

let part1 (antennas: Map<char, Coord seq>) (size: int) =
    antennas
    |> Map.map (fun _ -> toPairs >> Seq.map (toAntinodePair size) >> Seq.concat)
    |> countAntinodes

let part2 (antennas: Map<char, Coord seq>) (size: int) =
    antennas
    |> Map.map (fun _ -> toPairs >> Seq.map (toAntinodeSeq size) >> Seq.concat)
    |> countAntinodes

let run () =
    let (size, antennas) =
        let lines = File.ReadAllLines "./Input/day8.txt" |> List.ofSeq
        let antennas =
            lines
            |> List.mapi parse
            |> Seq.concat
            |> Seq.groupBy fst
            |> Map.ofSeq
            |> Map.map (fun _ x -> x |> Seq.map snd)
        (lines.Length, antennas)

    part1 antennas size
    |> printfn "[Day 8] Part 1: %d"

    part2 antennas size
    |> printfn "[Day 8] Part 2: %d"
