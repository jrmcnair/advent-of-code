module Tests.Day6

open Xunit
open AdventOfCode2025.Day6

let sampleData = """123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  """

module Part1 =
    open Part1
    
    [<Fact>]
    let ``Run Sample Data`` () =
        let expected = 4277556L
        
        let actual =
            sampleData.Split(System.Environment.NewLine)
            |> parse 3
            |> Array.map Problem.solve
            |> Array.sum
        
        Assert.Equal(expected, actual)

module Part2 =
    open Part2

    [<Fact>]
    let ``Run Sample Data`` () =
        let actual =
            sampleData.Split(System.Environment.NewLine)
            |> parse 3

        Assert.Equal(4, actual.Length)