// https://adventofcode.com/2024/day/21
module Day21

open System.Collections.Generic
open AdventOfCode.Common

let keypad = Map [
    '7', Coord(0, 0); '8', Coord(1, 0); '9', Coord(2, 0)
    '4', Coord(0, 1); '5', Coord(1, 1); '6', Coord(2, 1)
    '1', Coord(0, 2); '2', Coord(1, 2); '3', Coord(2, 2)
    '0', Coord(1, 3); 'A', Coord(2, 3);
]

let remote = Map [
    '^', Coord(1, 0); 'A', Coord(2, 0)
    '<', Coord(0, 1); 'v', Coord(1, 1); '>', Coord(2, 1)
]

let keypadCommands (curr: Coord) (next: Coord) =
    let diff = next - curr
    let vert = if diff.Y < 0 then new string('^', abs diff.Y) else new string('v', abs diff.Y)
    let horz = if diff.X < 0 then new string('<', abs diff.X) else new string('>', abs diff.X)

    let cmds =
        if diff.X = 0 then vert
        elif diff.Y = 0 then horz
        elif curr.Y = 3 && next.X = 0 then vert + horz // avoid gap
        elif curr.X = 0 && next.Y = 3 then horz + vert // avoid gap
        elif diff.X < 0 && diff.Y < 0 then horz + vert // up/left
        elif diff.X < 0 && diff.Y > 0 then horz + vert // down/left
        else vert + horz // down/right or up/right

    cmds + "A"

let remoteCommands (curr: Coord) (next: Coord) =
    let diff = next - curr
    let vert = if diff.Y < 0 then new string('^', abs diff.Y) else new string('v', abs diff.Y)
    let horz = if diff.X < 0 then new string('<', abs diff.X) else new string('>', abs diff.X)

    let cmds =
        if diff.X = 0 || diff.Y = 0 then horz + vert
        elif curr.X = 0 && next.Y = 0 then horz + vert // avoid gap
        elif next.X = 0 && curr.Y = 0 then vert + horz // avoid gap
        elif diff.X < 0 && diff.Y < 0 then horz + vert // up/left
        elif diff.X < 0 && diff.Y > 0 then horz + vert // down/left
        else vert + horz // down/right or up/right

    cmds + "A"

let getKeypadCommands (buttons: string)=
    let rec toRobot (current: char) (remaining: string) (commands: string) =
        let next = Seq.head remaining
        let newCmds = commands + keypadCommands keypad[current] keypad[next]
                        
        if remaining.Length = 1 then newCmds
        else toRobot next (remaining.Substring(1)) newCmds

    toRobot 'A' buttons ""

let getRemoteCommands (buttons: string) =
    let rec toRobot (current: char) (remaining: string) (commands: string) =
        let next = Seq.head remaining
        let newCmds = commands + remoteCommands remote[current] remote[next]
                        
        if remaining.Length = 1 then newCmds
        else toRobot next (remaining.Substring(1)) newCmds

    toRobot 'A' buttons ""

let pushButtons (numRobots: int) (buttons: string) =
    let rec robotChain (robotsRemaining: int) (input: string) =
        if robotsRemaining = 0 then input
        else
            getRemoteCommands input
            |> robotChain (robotsRemaining - 1)
    
    robotChain numRobots buttons
   
let complexity (code: string) (cmds: string) =
   int (code.Substring(0,3)) * cmds.Length

let solve (numRobots: int) (codes: string list)=
    codes
    |> Seq.map (fun code ->
        getKeypadCommands code
        |> pushButtons numRobots
        |> complexity code)
    |> Seq.sum

let codes = ["869A"; "170A"; "319A"; "349A"; "489A"]

let part1 () =
    solve 2 codes
    |> printfn "[Day 21] Part 1: %d" // 156714

let part2 () =
    solve 25 codes
    |> printfn "[Day 21] Part 2: %d"
