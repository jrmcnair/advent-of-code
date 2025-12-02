module Tests.Day2

open AdventOfCode2025.Day2
open Xunit

let sampleData = """11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"""

module Part1 =
    open Part1
    
    [<Theory>]
    [<InlineData(11, true)>]
    [<InlineData(12, false)>]
    [<InlineData(1010, true)>]
    [<InlineData(1221, false)>]
    [<InlineData(123456, false)>]
    [<InlineData(999999, true)>]
    let ``isInvalidProductId is Correct`` (productId: int, isInvalid: bool) =
        let actual = isInvalidProductId productId
        Assert.Equal(isInvalid, actual)
