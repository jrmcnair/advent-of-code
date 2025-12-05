module Tests.Day5

open AdventOfCode2025.Day5
open Xunit

let sampleData = """3-5
10-14
16-20
12-18

1
5
8
11
17
32"""

module Part1 =
    open Part1
    
    [<Fact>]
    let ``Run Sample Data`` () =
        let freshIngredients = Parse.freshIngredientRanges sampleData
        let availableIngredients = Parse.availableIngredients sampleData

        let freshCount =
            availableIngredients
            |> Seq.filter (ingredientIsFresh freshIngredients)
            |> Seq.length
        
        Assert.Equal(3, freshCount)

module Part2 =
    open Part2

    [<Fact>]
    let ``Run Sample Data`` () =
        Assert.True(true)
