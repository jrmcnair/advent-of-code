module Day12

open System.Collections.Generic
open System.IO

type Coord = int * int // TODO: should this have Row/Col?
type Plant = char

type Plots = Map<Coord, Plant>
module Plots =
    let load (input: string seq) =
        input
        |> Seq.mapi (fun row cols -> cols |> Seq.mapi (fun col plant -> (row, col), plant))
        |> Seq.concat
        |> Map.ofSeq

    let processNeighbors (plots: Plots) (row:int, col:int) =
        let mutable perimeters = 0
        let mutable sides = 0
        let plant = plots[row,col]

        let plantMatches (c: Coord) = plots |> Map.containsKey c && plots[c] = plant
        let dirs =
            Map [ "N", (-1,0); "S", (1,0); "E", (0,1); "W", (0,-1)
                  "NE", (-1,1); "SE", (-1,1); "NW", (-1,-1); "SW", (1,-1) ]
            |> Map.map (fun _ (r, c) ->
                let loc = (row + r, col + c)
                loc, loc |> plantMatches)

        let nCoord, nMatches = dirs["N"]
        let sCoord, sMatches = dirs["S"]
        let eCoord, eMatches = dirs["E"]
        let wCoord, wMatches = dirs["W"]
        let _, neMatches = dirs["NE"]
        let _, nwMatches = dirs["NW"]
        let _, swMatches = dirs["SW"]
       
        if not nMatches then
            perimeters <- perimeters + 1
            if not (wMatches && not nwMatches) then sides <- sides + 1 
        if not wMatches then
            perimeters <- perimeters + 1
            if not (nMatches && not nwMatches) then sides <- sides + 1 
        if not eMatches then
            perimeters <- perimeters + 1
            if not (nMatches && not neMatches) then sides <- sides + 1 
        if not sMatches then
            perimeters <- perimeters + 1
            if not (wMatches && not swMatches) then sides <- sides + 1 

        let cardinalNeighbors =
            seq {
                if nMatches then yield nCoord
                if sMatches then yield sCoord
                if eMatches then yield eCoord
                if wMatches then yield wCoord
            }
            
        (cardinalNeighbors, perimeters, sides)

type Region = {
    Area: int
    Perimeter: int
    Sides: int
}
and RegionFinder = Plots -> HashSet<Coord> ->  Coord -> Region
module Region =
    let empty = { Area = 0; Perimeter = 0; Sides = 0 }
    let priceByPerimeter (r:Region) = r.Area * r.Perimeter
    let priceBySide (r:Region) = r.Area * r.Sides
    let add (area: int) (perimeter: int) (sides: int) (r:Region) =
        { r with Area = r.Area + area
                 Perimeter = r.Perimeter + perimeter
                 Sides = r.Sides + sides }

let regionFinder (plots: Plots) (visited: HashSet<Coord>) (start: Coord) =
    let mutable region: Region = Region.empty
    
    let rec walk (loc: Coord) (plant: Plant) =
        visited.Add(loc) |> ignore
    
        let neighbors, perimeters, sides = loc |> Plots.processNeighbors plots
        region <- region |> Region.add 1 perimeters sides

        neighbors
        |> Seq.filter (fun c -> not <| visited.Contains(c))
        |> Seq.iter (fun c -> walk c plant)
    
    walk start plots[start]
    region

let toRegions (plots: Plots): Region seq =
    let visited : HashSet<Coord> = HashSet()

    seq {
        for loc in plots.Keys do
            if not <| visited.Contains(loc) then
                yield regionFinder plots visited loc
    }

let part1 regions =
    regions
    |> Seq.map Region.priceByPerimeter |> Seq.sum

let part2 regions =
    regions
    |> Seq.map Region.priceBySide |> Seq.sum

let run () =
    let plots = File.ReadAllLines "./Input/day12.txt" |> Plots.load

    plots |> toRegions |> part1 |> printfn "[Day 12] Part 1: %d" // 1433460
    plots |> toRegions |> part2 |> printfn "[Day 12] Part 2: %d" // 855082
