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

    let neighbors (plots: Plots) (row:int, col:int) : (Coord * Plant) list =
        [
            if Map.containsKey(row - 1, col) plots then yield (row - 1, col), plots[(row - 1, col)] // up
            if Map.containsKey(row + 1, col) plots then yield (row + 1, col), plots[(row + 1, col)] // down
            if Map.containsKey(row, col - 1) plots then yield (row, col - 1), plots[(row, col - 1)] // left
            if Map.containsKey(row, col + 1) plots then yield (row, col + 1), plots[(row, col + 1)] // right
        ]
type Region = {
    Area: int
    Fences: int
}
and RegionFinder = Plots -> HashSet<Coord> ->  Coord -> Region
module Region =
    let empty = { Area = 0; Fences = 0 }
    let price (r:Region) = r.Area * r.Fences
    let add (area: int) (fences: int) (r:Region) =
        { r with Area = r.Area + area; Fences = r.Fences + fences }
    let fromPlots (finder: RegionFinder) (plots: Plots):  Region seq =
        let visited : HashSet<Coord> = HashSet()

        seq {
            for loc in plots.Keys do
                if not <| visited.Contains(loc) then
                    yield finder plots visited loc
        }

module Part1 =
    let regionFinder (plots: Plots) (visited: HashSet<Coord>) (start: Coord) =
        let mutable region: Region = Region.empty

        let rec walk (loc: Coord) (plant: Plant) =
            visited.Add(loc) |> ignore

            let neighbors = loc |> Plots.neighbors plots
            let regionNeighbors = neighbors |> Seq.filter (fun (_, p) -> p = plant)

            regionNeighbors
            |> Seq.filter (fun (l, p) -> not <| visited.Contains(l))
            |> Seq.iter (fun (l, p) -> walk l plant)
            
            region <- region |> Region.add 1 (4 - (regionNeighbors |> Seq.length))

        walk start plots[start]
        region

    let execute (plots: Map<Coord, Plant>) =
        plots |> Region.fromPlots regionFinder |> Seq.map (Region.price) |> Seq.sum

module Part2 =
    let execute (plots: Map<Coord, Plant>) = 0

let run () =
    let plots = File.ReadAllLines "./Input/day12.txt" |> Plots.load

    Part1.execute plots
    |> printfn "[Day 12] Part 1: %d"


    Part2.execute plots
    |> printfn "[Day 12] Part 2: %d"
