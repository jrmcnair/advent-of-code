module Tests.Day11

open Xunit
open Day11

let sample = "125 17"

[<Fact>]
let ``parse: two entries`` () =
    let expected = Map [ (17UL, 1); (125UL, 1) ]
    let actual = parse sample

    Assert.Equivalent(expected, actual)

[<Fact>]
let ``parse: duplicate entry`` () =
    let expected = Map [ (1UL, 2) ]
    let actual = parse "1 1"

    Assert.Equivalent(expected, actual)

[<Fact>]
let ``blinkStone: rule 1 - 0 to 1`` () =
    let expected = [ 1UL ]
    Assert.Equivalent(expected, blinkStone 0UL)

[<Fact>]
let ``blinkStone: rule 2 - split if even # of digits`` () =
    let expected = [ 20UL; 24UL ]
    Assert.Equivalent(expected, blinkStone 2024UL)

[<Fact>]
let ``blinkStone: rule 2 - reduce zeros`` () =
    let expected = [ 20UL; 0UL ]
    Assert.Equivalent(expected, blinkStone 2000UL)

[<Fact>]
let ``blinkStone: rule 3 - x 2024`` () =
    let expected = [ 2024UL ]
    Assert.Equivalent(expected, blinkStone 1UL)

[<Fact>]
let ``blink test`` () =
    let expected = Map [ (1UL,1); (7UL,1); (253000UL,1) ]
    let actual = parse sample |> blink 1

    Assert.Equivalent(expected, actual)

[<Fact>]
let ``blink with duplicates`` () =
    let expected = Map [ (2UL,2); (0UL,1); (4UL,1) ]
    let actual = parse "20 24" |> blink 1

    Assert.Equivalent(expected, actual)

[<Fact>]
let ``blink with overlaps`` () =
    let expected = Map [ (2UL,2); (0UL,1); (4UL,1); (8096UL, 1) ]
    let actual = parse "20 24 4" |> blink 1

    Assert.Equivalent(expected, actual)

[<Theory>]
[<InlineData(1, 3UL)>]
[<InlineData(2, 4UL)>]
[<InlineData(3, 5UL)>]
[<InlineData(4, 9UL)>]
[<InlineData(5, 13UL)>]
[<InlineData(6, 22UL)>]
let ``execute gets correct count`` (blinks: int, expected: uint64) =
    Assert.Equal(expected, execute sample blinks)
