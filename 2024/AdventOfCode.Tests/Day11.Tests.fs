module Tests.Day11

open System.Collections.Generic
open Xunit

let sample = "125 17"

module Part1 =
    open Day11.Part1
    
    [<Fact>]
    let ``parse: two entries`` () =
        let expected = [ 125UL; 17UL ] |> LinkedList
        let actual = parse sample

        Assert.Equivalent(expected, actual)

    [<Fact>]
    let ``blink: rule 1 - 0 to 1`` () =
        let stones = parse "0"
        let expected = [ 1UL ] |> LinkedList

        Assert.Equivalent(expected, blink stones)

    [<Fact>]
    let ``blink: rule 2 - split if even # of digits`` () =
        let stones = parse "2024"
        let expected = [ 20UL; 24UL ] |> LinkedList

        Assert.Equivalent(expected, blink stones)

    [<Fact>]
    let ``blink: rule 2 - reduce zeros`` () =
        let stones = parse "2000"
        let expected = [ 20UL; 0UL ] |> LinkedList

        Assert.Equivalent(expected, blink stones)

    [<Fact>]
    let ``blink: rule 3 - x 2024`` () =
        let stones = parse "1"
        let expected = [ 2024UL ] |> LinkedList

        Assert.Equivalent(expected, blink stones)

    [<Fact>]
    let ``blink: all rules`` () =
        let stones = parse "0 1 10 99 999"
        let expected = [ 1UL; 2024UL; 1UL; 0UL; 9UL; 9UL; 2021976UL ] |> LinkedList

        Assert.Equivalent(expected, blink stones)

    [<Theory>]
    [<InlineData(6, 22)>]
    [<InlineData(25, 55312)>]
    let ``part1: sample`` (blinks: int, expected: int) =
        Assert.Equal(expected, execute sample blinks)
