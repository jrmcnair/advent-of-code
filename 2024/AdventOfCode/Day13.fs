module Day13

open System.IO
open System.Text.RegularExpressions

type ClawMachine = {
    ButtonA: int * int
    ButtonB: int * int
    EfficientButton: Button
    PrizeLocation: int * int
    MaxIterations: int
}
and Button = | A | B
module ClawMachine =
    let create (ax: int, ay: int) (bx: int, by: int) (plx: int, ply: int) =
        let efficiency (x: int, y:int) (cost: int) = sqrt (float (x * x + y * y)) / (float cost)
        let iterations t n1 n2 = if n1 > n2 then (t / n2) + 1 else (t / n1) + 1

        let efficientButton = if efficiency (ax,ay) 3 >= efficiency (bx,by) 1 then A else B
        
        let maxIterations =
            let iterX = iterations plx ax bx
            let iterY = iterations ply ay by
            if iterX >= iterY then iterX else iterY
        
        { ButtonA = (ax,ay); ButtonB = (bx,by); EfficientButton = efficientButton
          PrizeLocation = (plx,ply); MaxIterations = maxIterations }

    let advance (machine: ClawMachine) (loc: int * int) =
        let move (x:int, y:int) (dx: int, dy: int) = (x + dx, y+dy)

        match machine.EfficientButton with
        | A -> move loc machine.ButtonB
        | B -> move loc machine.ButtonA
    
    let distanceToPrize (machine: ClawMachine) (x: int, y:int) =
        let px, py = machine.PrizeLocation
        px - x, py - y 
    let canReachPrizeEfficiently (machine: ClawMachine) (x:int, y:int) =
        let dx, dy = distanceToPrize machine (x,y)
        let mx, my = if machine.EfficientButton = A then machine.ButtonA else machine.ButtonB

        if dx % mx = 0 && dy % my = 0 && dx/mx = dy/my
        then Some (dx/mx)
        else None

    let countTokens (machine: ClawMachine) (efficient: int) (inefficient: int) =
        match machine.EfficientButton with
        | A -> (efficient * 3) + inefficient
        | B -> (inefficient * 3) + efficient

let parse (input: string seq) =
    let parseLine (line: string) =
        let matches = Regex.Matches(line, "(\d+)") |> Seq.map (fun x -> int x.Value) |> Array.ofSeq
        int matches[0], int matches[1]
        
    input |> Seq.filter (fun s -> s <> "") |> Seq.chunkBySize 3 |> Seq.map(fun x ->
        ClawMachine.create (parseLine x[0]) (parseLine x[1]) (parseLine x[2]) )

let tokensToPrize (machine: ClawMachine) =
    let finalX, finalY = machine.PrizeLocation
    
    let rec walk (count: int) (x: int, y:int) =
        if x = finalX && y = finalY then Some (0, count)
        else if x > finalX || y > finalY then None
        else
            match ClawMachine.canReachPrizeEfficiently machine (x,y) with
            | Some num -> Some (num, count)
            | None -> ClawMachine.advance machine (x,y) |> walk (count + 1)

    match walk 0 (0,0) with
    | Some (efficient, inefficient) -> ClawMachine.countTokens machine efficient inefficient
    | None -> 0

let part1 (input:string seq) =
    input
    |> parse
    |> Seq.map tokensToPrize
    |> Seq.sum

let part2 (input:string seq) = 0

let run () =
    let input = File.ReadAllLines "./Input/day13.txt"

    part1 input
    |> printfn "[Day 13] Part 1: %d"

    part2 input
    |> printfn "[Day 13] Part 2: %d"

