module Day15

open System.Collections.Generic
open System.IO
open AdventOfCode.Common

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
        let next (loc: Loc) = function
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
            let nxt = Direction.next cur dir
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
            let nxt = Direction.next cur dir
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
        let start = warehouse |> Array2D.findIndex (fun x -> x = Robot) |> Loc.ofTuple
        //warehouse[start] <- Empty // can do this if it gets ugly
        
        move warehouse moves start
        
        warehouse
        |> Array2D.mapi (fun row col tile -> tile |> Tile.gps (row,col))
        |> Seq.cast<int>
        |> Seq.sum

module Part2 =

    type Tile = | BoxLeft | BoxRight | Empty | Robot | Wall
    module Tile =
        let ofChar = function
            | 'O' -> seq { BoxLeft; BoxRight }
            | '@' -> seq { Robot; Empty }
            | '#' -> seq { Wall; Wall }
            | _ -> seq { Empty; Empty }
        let gps (row: int, col: int) = function
            | BoxLeft -> row * 100 + col
            | _ -> 0

    type Direction = | N | S | E | W
    module Direction =
        let ofChar = function
            | '^' -> N | 'v' -> S | '>' -> E | _ -> W
        let next (loc: Loc) = function
            | N -> { loc with Row = loc.Row - 1 }
            | S -> { loc with Row = loc.Row + 1 }
            | E -> { loc with Col = loc.Col + 1 }
            | W -> { loc with Col = loc.Col - 1 }

    let parse (input: string seq) =
        let inputList = input |> List.ofSeq
        let splitIndex = inputList |> Seq.findIndex (fun x -> x = "")
        
        let warehouse =
            inputList[0..splitIndex-1]
            |> Seq.map (Seq.map Tile.ofChar >> Seq.concat)
            |> array2D

        let moves =
            inputList[splitIndex+1..inputList.Length]
            |> Seq.map (Seq.map Direction.ofChar)
            |> Seq.concat
            |> List.ofSeq
        
        (warehouse, moves)

    let findEmpty (warehouse: Tile[,]) (dir: Direction) (start: Loc) =
        let tiles = HashSet<Loc>()

        let rec horizontal (cur: Loc) =
            tiles.Add(cur) |> ignore
            let nxt = Direction.next cur dir
            match warehouse[nxt.Row, nxt.Col] with
            | Empty -> true
            | BoxLeft | BoxRight -> horizontal nxt
            | _ -> false
        
        let rec vertical (curL: Loc) (curR: Loc) =
            tiles.Add(curL) |> ignore
            tiles.Add(curR) |> ignore

            let nxtL, nxtR = Direction.next curL dir, Direction.next curR dir
            match warehouse[nxtL.Row, nxtL.Col], warehouse[nxtR.Row, nxtR.Col] with
            // ....
            //  []
            | Empty, Empty -> true
            // .[].
            //  []
            | BoxLeft, BoxRight -> vertical nxtL nxtR
            // []..
            //  []
            | BoxRight, Empty -> vertical (Direction.next nxtL W) nxtL
            // ..[]
            //  []
            | Empty, BoxLeft -> vertical nxtR (Direction.next nxtR E)
            // [][]
            //  []
            | BoxRight, BoxLeft ->
                vertical (Direction.next nxtL W) nxtL
                && vertical nxtR (Direction.next nxtR E)
            | _ -> false

        let result =
            match dir with
            | N | S ->
                if warehouse[start.Row, start.Col] = BoxLeft
                then vertical start {start with Col = start.Col + 1}
                else vertical {start with Col = start.Col - 1} start
            | E | W -> horizontal start

        if result then Some tiles else None

    let pushBoxes (warehouse: Tile[,]) (tiles: HashSet<Loc>) = function
        | N ->
            tiles
            |> Seq.map id
            |> Seq.sortBy _.Row
            |> Seq.iter (fun loc ->
                warehouse[loc.Row - 1, loc.Col] <- warehouse[loc.Row, loc.Col]
                warehouse[loc.Row, loc.Col] <- Empty )
        | S ->
            tiles
            |> Seq.map id
            |> Seq.sortByDescending _.Row
            |> Seq.iter (fun loc ->
                warehouse[loc.Row + 1, loc.Col] <- warehouse[loc.Row, loc.Col]
                warehouse[loc.Row, loc.Col] <- Empty )
        | E ->
            tiles
            |> Seq.map id
            |> Seq.sortByDescending _.Col
            |> Seq.iter (fun loc ->
                warehouse[loc.Row, loc.Col + 1] <- warehouse[loc.Row, loc.Col]
                warehouse[loc.Row, loc.Col] <- Empty )
        | W ->
            tiles
            |> Seq.map id
            |> Seq.sortBy _.Col
            |> Seq.iter (fun loc ->
                warehouse[loc.Row, loc.Col - 1] <- warehouse[loc.Row, loc.Col]
                warehouse[loc.Row, loc.Col] <- Empty )
    
    let move (warehouse: Tile[,]) (moves: Direction list) (start: Loc) =
        let mutable cur = start
            
        // TODO: make this recursive instead of mutable vars?
        moves
        |> List.iter (fun dir ->
            let nxt = Direction.next cur dir

            match warehouse[nxt.Row, nxt.Col] with
            | BoxLeft | BoxRight ->
                match findEmpty warehouse dir nxt with
                | Some tiles ->
                    pushBoxes warehouse tiles dir 
                    cur <- nxt
                | _ -> ()
            | Empty ->
                cur <- nxt
            | _ -> ()
        )

    let execute (input:string seq) =
        let warehouse, moves = parse input
        let start = warehouse |> Array2D.findIndex (fun x -> x = Robot) |> Loc.ofTuple
        warehouse[start.Row, start.Col] <- Empty
        
        move warehouse moves start
        
        warehouse
        |> Array2D.mapi (fun row col tile -> tile |> Tile.gps (row,col))
        |> Seq.cast<int>
        |> Seq.sum

let run () =
    let input = File.ReadAllLines "./Input/day15.txt"

    Part1.execute input
    |> printfn "[Day 15] Part 1: %d"

    Part2.execute input
    |> printfn "[Day 15] Part 2: %d"
