module Day6

open System
open System.IO

[<Struct>]
[<StructuredFormatDisplay("({Row},{Col})")>]
type Coord (row: int, col: int) =
    member _.Row = row
    member _.Col = col

type Tile =
    | Empty
    | Obstacle
    | Visited
    | Guard
module Tile =
    let toTile = function
        | '^' -> Guard
        | '#' -> Obstacle
        | _ -> Empty
    let isBlocked tile  = tile = Obstacle

type Direction =
    | East
    | North
    | South
    | West

module Movement =
    let turn = function
        | East -> South
        | North -> East
        | South -> West
        | West -> North

    let isOffGrid (size: int) (loc: Coord) =
        loc.Row < 0 || loc.Row >= size || loc.Col < 0 || loc.Col >= size

    let next (loc: Coord) (dir: Direction) (size: int) =
        let nxt =
            match dir with
            | East -> Coord(loc.Row, loc.Col + 1)
            | North -> Coord(loc.Row - 1, loc.Col)
            | South -> Coord(loc.Row + 1, loc.Col)
            | West -> Coord(loc.Row, loc.Col - 1)

        if isOffGrid size nxt
        then None
        else Some nxt

let filename = "./Input/day6.txt"

let loadData filename  =
    filename
    |> File.ReadLines
    |> Seq.map (fun x -> x.ToCharArray() |> Seq.map Tile.toTile |> Array.ofSeq) 
    |> Array.ofSeq

// TODO: look at Array.pick
let findGuard (grid: Tile[][]) =
    grid
    |> Array.mapi (fun row cols ->
        cols |> Array.mapi (fun col tile ->
            match tile with
            | Guard -> Coord(row, col) |> Some
            | _ -> None))
    |> Array.collect id
    |> Array.find Option.isSome
    |> Option.get

let grid = filename |> loadData
let start = findGuard grid

let patrol (grid:Tile[][]) =
    let rec go (loc:Coord) (dir: Direction) (seen: (Coord * Direction) list) =
        if seen |> List.contains (loc, dir)
        then []
        else
            let seen = (loc,dir) :: seen
            match Movement.next loc dir (Array.length grid) with
            | Some next ->
                if Tile.isBlocked grid[next.Row].[next.Col]
                then go loc (Movement.turn dir) seen
                else go next dir seen
            | None -> seen |> List.map fst |> List.distinct
    
    go start North []

let tilesGuarded = patrol grid

// -- part 1 -------------------------------------------------------------------

let part1 () =
    tilesGuarded
    |> List.length
    |> printfn "part1: %d"

// -- part 2 -------------------------------------------------------------------

let getGridWithNewObstacle (grid: Tile[][]) (loc: Coord) =
    grid |> Array.updateAt loc.Row (grid[loc.Row] |> Array.updateAt loc.Col Obstacle )

let startTime = DateTime.Now

let part2 () =
    tilesGuarded
    |> List.filter (fun loc -> loc <> start)
    |> List.filter (fun loc -> getGridWithNewObstacle grid loc |> patrol |> List.isEmpty)
    |> List.length
    |> printfn "part2: %d"
