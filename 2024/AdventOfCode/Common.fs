namespace AdventOfCode.Common

// TODO: Add examples for searches like Dijkstra, DFS and BFS searches in a Searches.fs file?

module Array2D =
    let findIndex (v:'T) (a2d: 'T[,]) =
        let maxCol = (a2d |> Array2D.length2) - 1
        let rec go row col =
            if col > maxCol then go (row + 1) 0
            elif a2d.[row,col] = v then (row,col)
            else go row (col + 1)
        go 0 0

    let tryFindIndex (v:'T) (a2d: 'T[,]) = 
        let maxRow = (a2d |> Array2D.length1) - 1
        let maxCol = (a2d |> Array2D.length2) - 1
        let rec go row col =
            if row > maxRow then None
            elif col > maxCol then go (row + 1) col
            elif a2d.[row,col] = v then Some (row,col)
            else go row (col + 1)
        go 0 0

    let tryGet (row: int, col: int) (a2d: 'T[,]) =
        if row < 0 || row >= Array2D.length1 a2d then None
        elif col < 0 || col >= Array2D.length2 a2d then None
        else Some a2d[row, col]

    let ofChars (f: char -> 'T) (input: string seq) =
        input |> Seq.map (Seq.map f) |> array2D

[<Struct>]
type Loc = { Row: int; Col: int }
module Loc =
    let create (row: int) (col: int) = { Row = row; Col = col }
    let ofTuple (row: int, col:int) = create row col
    let toTuple (loc: Loc) = loc.Row, loc.Col

[<Struct>]
type Direction = | N | S | E | W
module Direction =
    let next (loc: Loc) = function
        | N -> { loc with Row = loc.Row - 1 }
        | S -> { loc with Row = loc.Row + 1 }
        | E -> { loc with Col = loc.Col + 1 }
        | W -> { loc with Col = loc.Col - 1 }

    let turnLeft = function
        | N -> W
        | S -> E
        | E -> N
        | W -> S

    let turnRight = function
        | N -> E
        | S -> W
        | E -> S
        | W -> N
