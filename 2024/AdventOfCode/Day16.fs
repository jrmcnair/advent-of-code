
// https://adventofcode.com/2024/day/16
module Day16

open System
open System.Collections.Generic
open System.IO
open AdventOfCode.Common

type Tile = | Empty | Start | End | Wall
module Tile =
    let ofChar = function
        | 'S' -> Start
        | 'E' -> End
        | '#' -> Wall
        | _ -> Empty

let parse (input: string seq) = input |> Array2D.ofChars Tile.ofChar
let getStart (maze: Tile[,]) = maze |> Array2D.findIndex (fun x -> x = Start) |> Loc.ofTuple
let getEnd (maze: Tile[,]) = maze |> Array2D.findIndex (fun x -> x = End) |> Loc.ofTuple

type Id = Loc * Direction * HashSet<Loc>

let findBestScore (maze: Tile[,]) =
    let startLoc = getStart maze
    let pq = PriorityQueue<Loc * Direction, int * Set<Loc>>()
    let visited = Dictionary<Loc * Direction, int>() // loc,dir -> best score
    let scores = Dictionary<int, Set<Loc>>() // score -> locations along the path
    let mutable bestScore = Int32.MaxValue
        
    let rec dijkstra () =
        match pq.TryDequeue() with
        | true, (loc, dir), (currentScore, path) ->
            if visited.ContainsKey(loc,dir) && visited[(loc,dir)] < currentScore
            then dijkstra ()
            else
                if visited.ContainsKey(loc,dir)
                then visited[(loc,dir)] <- currentScore
                else visited.Add((loc,dir), currentScore)
            
                let newPath = path |> Set.add loc
                let straightScore = currentScore + 1
                let turnScore = currentScore + 1000

                let straightLoc = Direction.next loc dir
                let straight = maze[straightLoc.Row, straightLoc.Col]

                if straight = Empty then pq.Enqueue((straightLoc, dir), (straightScore, newPath))
                elif straight = End then
                    if straightScore <= bestScore then
                        let finalPath = newPath |> Set.add straightLoc
                        if scores.ContainsKey(bestScore) then scores[bestScore] <- scores[bestScore] |> Set.union finalPath
                        else scores.Add(straightScore, finalPath)
                        bestScore <- straightScore

                let leftDir = dir |> Direction.turnLeft
                let leftLoc = Direction.next loc leftDir
                if maze[leftLoc.Row, leftLoc.Col] <> Wall then pq.Enqueue((loc, leftDir), (turnScore, newPath))

                let rightDir = dir |> Direction.turnRight
                let rightLoc = Direction.next loc rightDir
                if maze[rightLoc.Row, rightLoc.Col] <> Wall then pq.Enqueue((loc, rightDir), (turnScore, newPath))
                                
                dijkstra ()
        | _ -> ()

    pq.Enqueue((startLoc, E), (0, Set.empty))
    dijkstra ()

    let seats = scores[bestScore] |> Set.count

    bestScore, seats

let run () =
    let input = File.ReadAllLines "./Input/day16.txt"
    let maze = parse input

    let score, seats = findBestScore maze

    score |> printfn "[Day 16] Part 1: %d"
    seats |> printfn "[Day 16] Part 2: %d"
