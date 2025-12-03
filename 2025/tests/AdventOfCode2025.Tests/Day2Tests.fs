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

module Part2 =
    open Part2
         
    [<Theory>]
    [<InlineData("11", 1, true)>]
    [<InlineData("22", 1, true)>]
    [<InlineData("99", 1, true)>]
    [<InlineData("111", 1, true)>]
    [<InlineData("999", 1, true)>]
    [<InlineData("1010", 1, false)>]
    [<InlineData("1010", 2, true)>]
    [<InlineData("1188511885", 1, false)>]
    [<InlineData("1188511885", 2, false)>]
    [<InlineData("1188511885", 3, false)>]
    [<InlineData("1188511885", 4, false)>]
    [<InlineData("1188511885", 5, true)>]
    [<InlineData("222222", 1, true)>]
    [<InlineData("222222", 2, true)>]
    [<InlineData("222222", 3, true)>]
    [<InlineData("446446", 1, false)>]
    [<InlineData("446446", 2, false)>]
    [<InlineData("446446", 3, true)>]
    [<InlineData("38593859", 1, false)>]
    [<InlineData("38593859", 2, false)>]
    [<InlineData("38593859", 3, false)>]
    [<InlineData("38593859", 4, true)>]
    [<InlineData("565656", 1, false)>]
    [<InlineData("565656", 2, true)>]
    [<InlineData("565656", 3, false)>]
    [<InlineData("824824824", 1, false)>]
    [<InlineData("824824824", 2, false)>]
    [<InlineData("824824824", 3, true)>]
    [<InlineData("824824824", 4, false)>]
    [<InlineData("2121212121", 1, false)>]
    [<InlineData("2121212121", 2, true)>]
    [<InlineData("2121212121", 3, false)>]
    [<InlineData("2121212121", 4, false)>]
    [<InlineData("2121212121", 5, false)>]
    let ``chunkedProductIdIsInvalid is Correct`` (input: string, chunkSize: int, expected: bool) =
        let actual = chunkedProductIdIsInvalid input chunkSize 
        Assert.Equal(expected, actual)
         
    [<Theory>]
    [<InlineData(11L, true)>]
    [<InlineData(12L, false)>]
    [<InlineData(1010L, true)>]
    [<InlineData(1221L, false)>]
    [<InlineData(123456L, false)>]
    [<InlineData(999999L, true)>]
    [<InlineData(999L, true)>]
    [<InlineData(121212L, true)>]
    [<InlineData(99990, false)>]
    [<InlineData(1261212L, false)>]
    let ``isInvalidProductId is Correct`` (productId: int64, isInvalid: bool) =
        let expected = if isInvalid then Some productId else None
        let actual = isInvalidProductId productId
        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Run Sample Data`` () =
        let parsedData = parseData sampleData
        let expected = 4174379265L

        let actual =
            parsedData
            |> Array.map processRange
            |> Array.sum

        Assert.Equal(expected, actual)
