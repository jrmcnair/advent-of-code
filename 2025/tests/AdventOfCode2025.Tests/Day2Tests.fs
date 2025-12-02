module Tests.Day2

open AdventOfCode2025.Day2
open Xunit

let sampleData = """11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"""

module Part1 =
    open Part1
    
    [<Theory>]
    [<InlineData(11L, true)>]
    [<InlineData(12L, false)>]
    [<InlineData(1010L, true)>]
    [<InlineData(1221L, false)>]
    [<InlineData(123456L, false)>]
    [<InlineData(999999L, true)>]
    let ``isInvalidProductId is Correct`` (productId: int64, isInvalid: bool) =
        let expected = if isInvalid then Some productId else None
        let actual = isInvalidProductId productId
        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Run Sample Data`` () =
        let parsedData = parseData sampleData
        let expected = 1227775554L

        let actual =
            parsedData
            |> Array.map processRange
            |> Array.sum

        Assert.Equal(expected, actual)
