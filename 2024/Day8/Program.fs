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

let (size, antennas) =
    let lines = File.ReadAllLines "input.txt" |> List.ofSeq
    let antennas =
        lines
        |> List.mapi parse
        |> Seq.concat
        |> Seq.groupBy fst
        |> Map.ofSeq
        |> Map.map (fun _ x -> x |> Seq.map snd)
    (lines.Length, antennas)

let toPairs (coords: Coord seq) =
    Seq.allPairs coords coords
    |> Seq.filter (fun (a, b) -> not (a = b))
    // TODO: filter out duplicate pairs (a,b) (b,a)

let toAntinodePair (a1: Coord, a2: Coord) =
    let isValid (c: Coord) = c.Row >= 0 && c.Row < size && c.Col >= 0 && c.Col < size
    let rowDiff, colDiff = a1.Row - a2.Row, a1.Col - a2.Col

    seq {
        Coord(a1.Row + rowDiff, a1.Col + colDiff)
        Coord(a2.Row - rowDiff, a2.Col - colDiff)
    } |> Seq.filter isValid

let toAntinodeSeq (a1: Coord, a2: Coord) =
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

antennas
|> Map.map (fun _ -> toPairs >> Seq.map toAntinodePair >> Seq.concat)
|> countAntinodes
|> printfn "part1: %A"

antennas
|> Map.map (fun _ -> toPairs >> Seq.map toAntinodeSeq >> Seq.concat)
|> countAntinodes
|> printfn "part2: %A"
