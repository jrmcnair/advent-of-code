module Day15

open System.IO
open AdventOfCode.Solutions.Common

module Part1 =
    type Tile = | Box | Empty | Robot | Wall
    module Tile =
        let ofChar = function
            | 'O' -> Box
            | '@' -> Robot
            | '#' -> Wall
            | _ -> Empty
        let gps (row: int, col: int) = function
            | Box -> row * 100 + col
            | _ -> 0

    type Direction = | N | S | E | W
    module Direction =
        let ofChar = function
            | '^' -> N
            | 'v' -> S
            | '>' -> E
            | _ -> W
        let next (wh: Tile[,]) (loc: Loc) = function
            | N -> { loc with Row = loc.Row - 1 }
            | S -> { loc with Row = loc.Row + 1 }
            | E -> { loc with Col = loc.Col + 1 }
            | W -> { loc with Col = loc.Col - 1 }

    let parse (input: string seq) =
        let inputList = input |> List.ofSeq
        let splitIndex = inputList |> Seq.findIndex (fun x -> x = "")
        
        let warehouse =
            inputList[0..splitIndex-1]
            |> Seq.map (Seq.map Tile.ofChar)
            |> array2D

        let moves =
            inputList[splitIndex+1..inputList.Length]
            |> Seq.map (Seq.map Direction.ofChar)
            |> Seq.concat
            |> List.ofSeq
        
        (warehouse, moves)

    let findEmpty (warehouse: Tile[,]) (dir: Direction) (start: Loc) =
        let rec find (cur: Loc) =
            let nxt = Direction.next warehouse cur dir
            match warehouse[nxt.Row, nxt.Col] with
            | Empty | Robot -> Some nxt
            | Box -> find nxt
            | Wall -> None
        find start

    let move (warehouse: Tile[,]) (moves: Direction list) (start: Loc) =
        let mutable cur = start
            
        // TODO: make this recursive instead of mutable vars?
        moves
        |> List.iter (fun dir ->
            let nxt = Direction.next warehouse cur dir
            match warehouse[nxt.Row, nxt.Col] with
            | Box ->
                match findEmpty warehouse dir nxt with
                | Some loc ->
                    warehouse[loc.Row, loc.Col] <- Box
                    warehouse[nxt.Row, nxt.Col] <- Empty
                    cur <- nxt
                | _ -> ()
            | Empty | Robot ->
                cur <- nxt
            | Wall -> ())
        
        // TODO: would a mutable collection of coords be more efficient?
        //       if we move to a Dictionary then creating a Coord may be easier to work with
        // TODO: would it help much to combine consecutive similar directions?

    let execute (input:string seq) =
        let warehouse, moves = parse input
        let start = warehouse |> Array2D.findIndex Tile.Robot |> Loc.ofInt
        //warehouse[start] <- Empty // can do this if it gets ugly
        
        move warehouse moves start
        
        warehouse
        |> Array2D.mapi (fun row col tile -> tile |> Tile.gps (row,col))
        |> Seq.cast<int>
        |> Seq.sum

module Part2 =
    
    let execute (input:string seq) = 0


let run () =
    let input = File.ReadAllLines "./Input/day15.txt"

    Part1.execute input
    |> printfn "[Day 15] Part 1: %d"

    Part2.execute input
    |> printfn "[Day 15] Part 2: %d"
