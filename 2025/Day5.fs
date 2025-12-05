module AdventOfCode2025.Day5

open System
open System.IO

type Range = { From: int64; To: int64 }
module Range =
    let create (range: string) =
        let bounds = range.Split('-') |> Array.map int64
        { From = bounds[0]; To = bounds[1] }

module Parse =
    let freshIngredientRanges (input: string) =
        input.Split(Environment.NewLine + Environment.NewLine).[0]
        |> _.Split(Environment.NewLine)
        |> Array.map Range.create

    let availableIngredients (input: string) =
        input.Split(Environment.NewLine + Environment.NewLine).[1]
        |> _.Split(Environment.NewLine)
        |> Seq.map int64

module Part1 =
    let ingredientIsFresh (freshRanges: Range[]) (ingredient: int64) =
        freshRanges
        |> Array.exists (fun r -> ingredient >= r.From && ingredient <= r.To)

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
    let run () =
        2
        |> printfn "Part2: TBD = %d"
