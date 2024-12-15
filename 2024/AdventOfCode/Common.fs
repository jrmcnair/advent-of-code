namespace AdventOfCode.Solutions.Common

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

[<Struct>]
type Loc = { Row: int; Col: int }
module Loc =
    let create (row: int) (col: int) = { Row = row; Col = col }
    let ofInt (row: int, col:int) = create row col
    let toInt (loc: Loc) = loc.Row, loc.Col