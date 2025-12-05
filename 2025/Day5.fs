module AdventOfCode2025.Day5

open System
open System.IO

type Range = { From: int64; To: int64 }
module Range =
    let create (range: string) =
        let bounds = range.Split('-') |> Array.map int64
        { From = bounds[0]; To = bounds[1] }
    
    let count (range: Range) : int64 =
        range.To - range.From + 1L

module Parse =
    let freshIngredientRanges (input: string) =
        input.Split(Environment.NewLine + Environment.NewLine).[0]
        |> _.Split(Environment.NewLine)
        |> Array.map Range.create
        |> Array.toList

    let availableIngredients (input: string) =
        input.Split(Environment.NewLine + Environment.NewLine).[1]
        |> _.Split(Environment.NewLine)
        |> Seq.map int64

module Part1 =
    let ingredientIsFresh (freshRanges: Range list) (ingredient: int64) =
        freshRanges
        |> List.exists (fun r -> ingredient >= r.From && ingredient <= r.To)

    let run () =
        let freshRanges, availableIngredients =
            let input =
                "./input/day5.txt"
                |> File.ReadAllText

            (Parse.freshIngredientRanges input, Parse.availableIngredients input)

        availableIngredients
        |> Seq.filter (ingredientIsFresh freshRanges)
        |> Seq.length
        |> printfn "Part1: Fresh Ingredients = %d"

module Part2 =
    let mergeRanges (ranges: Range list) : Range list =
        ranges
        |> List.sortBy _.From
        |> List.fold (fun acc r ->
            match acc with
            | [] -> [ r ]
            | last :: rest ->
                if r.From <= last.To + 1L then
                    { From = last.From; To = max last.To r.To } :: rest
                else
                    r :: acc
        ) []
        |> List.rev
        
    let run () =
        "./input/day5.txt"
        |> File.ReadAllText
        |> Parse.freshIngredientRanges
        |> mergeRanges
        |> List.map Range.count
        |> List.sum
        |> printfn "Part2: Possible Fresh Ingredients = %d"
