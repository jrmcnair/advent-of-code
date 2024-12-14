module Day14

open System.IO
open System.Text.RegularExpressions

type Robot = {
    Pos: int * int
    Vel: int * int
}
module Robot =
    let create (line:string) =
        let matches = Regex.Matches(line, "-?\d+") |> Seq.map (fun x -> int x.Value) |> Array.ofSeq
        { Pos = (matches[0],matches[1]); Vel = (matches[2],matches[3]) }

    let advance (w: int, h: int) (seconds: int) (robot: Robot) =
        let dir (size: int) (start: int) (vel: int) =
            let result = (start + (vel * seconds)) % size
            if result < 0 then result + size
            else result

        let (x,y), (vx,vy) = robot.Pos, robot.Vel
        (dir w x vx, dir h y vy)                

let moveToFinalPositions (w: int, h: int) (seconds: int) (robots: Robot seq) =
    robots
    |> Seq.groupBy (Robot.advance (w,h) seconds)
    |> Seq.map (fun (pos, robots) -> pos, Seq.length robots)
    |> Map.ofSeq

let safetyFactor (width: int, height: int) (positions: Map<int*int, int>) =
    let midX, midY = width/2, height/2
    
    positions
    |> Seq.choose (fun kvp ->
        match kvp.Key with
        | (x,y) when x < midX && y < midY -> Some ("nw", kvp.Value)
        | (x,y) when x < midX && y > midY -> Some ("ne", kvp.Value)
        | (x,y) when x > midX && y < midY -> Some ("sw", kvp.Value)
        | (x,y) when x > midX && y > midY -> Some ("se", kvp.Value)
        | _ -> None)
    |> Seq.groupBy fst
    |> Seq.map (fun (_, x) -> x |> Seq.map snd |> Seq.sum)
    |> Seq.reduce (*)

let part1 robots dimensions seconds =
    robots
    |> moveToFinalPositions dimensions seconds
    |> safetyFactor dimensions

let part2 (robots: Robot seq) (dimensions: int * int)  =
    let robotCount = Seq.length robots

    [2 .. 100000]
    |> Seq.find (fun i ->
        let locCount =
            robots |> moveToFinalPositions dimensions i
            |> Seq.length

        // assume solution means every robot is in a unique position
        locCount = robotCount)

let run () =
    let input = File.ReadAllLines "./Input/day14.txt"
    let robots = Seq.map Robot.create input |> Seq.cache
    let dimensions = (101, 103)

    part1 robots dimensions 100
    |> printfn "[Day 14] Part 1: %d"

    part2 robots dimensions
    |> printfn "[Day 14] Part 2: %d"
