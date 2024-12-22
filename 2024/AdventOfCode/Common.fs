namespace AdventOfCode.Common

open System.Collections.Generic

// TODO: Add examples for searches like Dijkstra, DFS and BFS searches in a Searches.fs file?

type Coord(x: int, y: int) =
    member this.X = x
    member this.Y = y

    static member (+) (c1: Coord, c2: Coord) = Coord(c1.X + c2.X, c1.Y + c2.Y)
    static member (-) (c1: Coord, c2: Coord) = Coord(c1.X - c2.X, c1.Y - c2.Y)

    member this.Distance (other: Coord) = abs (other.X - this.X) + abs(other.Y - this.Y)
    override this.ToString() = $"(%d{this.X}, %d{this.Y})"

module Array2D =
    let findIndex (predicate: 'T -> bool) (a2d: 'T[,]) =
        let maxRow = (a2d |> Array2D.length1) - 1
        let maxCol = (a2d |> Array2D.length2) - 1
        let rec go row col =
            if row > maxRow then raise (KeyNotFoundException())
            if col > maxCol then go (row + 1) 0
            elif predicate a2d.[row,col] then (row,col)
            else go row (col + 1)
        go 0 0

    let tryFindIndex (predicate: 'T -> bool) (a2d: 'T[,]) = 
        let maxRow = (a2d |> Array2D.length1) - 1
        let maxCol = (a2d |> Array2D.length2) - 1
        let rec go row col =
            if row > maxRow then None
            elif col > maxCol then go (row + 1) col
            elif predicate a2d.[row,col] then Some (row, col)
            else go row (col + 1)
        go 0 0

    let isValid (a2d: 'T[,]) (row: int, col: int) =
        row >= 0 && row < Array2D.length1 a2d
        && col >= 0 && col < Array2D.length2 a2d
        
    let tryGet (row: int, col: int) (a2d: 'T[,]) =
        if isValid a2d (row,col) then Some a2d[row, col] else None

    let ofChars (f: char -> 'T) (input: string seq) =
        input |> Seq.map (Seq.map f) |> array2D

    let cardinalNeighbors (row:int, col:int) (a2d:'T[,]) =
        [(row - 1, col); (row + 1, col); (row, col - 1); (row, col + 1)]
        |> Seq.choose (fun (r, c) -> tryGet (r,c) a2d |> Option.map (fun v -> (r, c), v))

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
