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
    "A" + buttons
    |> Seq.pairwise
    |> Seq.map (fun (curr, next) -> keypadCommands keypad[curr] keypad[next])
    |> String.concat ""

// this is not part of the solution, but was needed to see the output for testing
let getRemoteCommand (numRobots: int) (keypadButtonsToPush: string) =
    let rec depthFirstSearch (buttons: string) (robot: int) (commands: string) =
        "A" + buttons
        |> Seq.toList
        |> List.pairwise
        |> List.map (fun (curr, next) ->
            let newCmds = remoteCommands remote[curr] remote[next]
            if robot = numRobots then commands + newCmds
            else depthFirstSearch newCmds (robot + 1) (commands + newCmds))
        |> String.concat ""
    
    depthFirstSearch keypadButtonsToPush 1 ""

let getRemoteCommandLength (numRobots: int) (keypadButtonsToPush: string) =
    let cache = Dictionary<char * char * int, int64>()

    let rec depthFirstSearch (buttons: string) (robot: int) (total: int64) =
        let cmdLength =
            "A" + buttons
            |> Seq.toList
            |> List.pairwise
            |> List.map (fun (curr, next) ->
                let cacheKey = curr, next, robot
                if cache.ContainsKey(cacheKey) then cache[cacheKey]
                else
                    let commands = remoteCommands remote[curr] remote[next]
                    let count =
                        if robot = numRobots then int64 commands.Length
                        else depthFirstSearch commands (robot + 1) total

                    cache.Add(cacheKey, count)
                    count)
            |> List.sum

        total + cmdLength
    
    depthFirstSearch keypadButtonsToPush 1 0

let complexity (code: string) (cmdLength: int64) =
   int64 (code.Substring(0,3)) * cmdLength

let solve (numRobots: int) (codes: string list)=
    codes
    |> List.map (fun code ->
        getKeypadCommands code
        |> getRemoteCommandLength numRobots
        |> complexity code)

    |> List.sum

let codes = ["869A"; "170A"; "319A"; "349A"; "489A"]

let part1 () =
    solve 2 codes
    |> printfn "[Day 21] Part 1: %d" // 156714

let part2 () =
    solve 25 codes
    |> printfn "[Day 21] Part 2: %d" // 191139369248202
