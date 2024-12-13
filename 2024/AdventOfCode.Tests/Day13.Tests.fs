module Tests.Day13

open System
open Xunit
open Day13

let sample = "Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279"

let loadInput (raw: string) =
    raw.Split Environment.NewLine

module CreateTests =

    [<Fact>]
    let ``create`` () =
        let expected =
          { A = (4L,4L)
            B = (1L,1L)
            Prize = (200L, 100L) }
        let actual = Machine.create false (4,4) (1,1) (200, 100)

        Assert.Equivalent(expected, actual)

module ParseTests =

    [<Fact>]
    let ``single line`` () =
        let input = loadInput sample |> Seq.take 4
        let expected = [
            Machine.create false (94,34) (22,67) (8400,5400)
        ]

        Assert.Equivalent(expected, input |> parse false)

    [<Fact>]
    let ``multiple lines`` () =
        let expected = [
            Machine.create false (94,34) (22,67) (8400,5400)
            Machine.create false (26,66) (67,21) (12748,12176)
            Machine.create false (17,86) (84,37) (7870,6450)
            Machine.create false (69,23) (27,71) (18641,10279)
        ]

        Assert.Equivalent(expected, loadInput sample |> parse false)

module SolveTests =

    [<Fact>]
    let ``unsolvable`` () =
        let machine = Machine.create false (2,2) (3,3) (1,1)
        let expected = None

        Assert.Equal(expected, solve machine)
    
    [<Fact>]
    let ``simple test`` () =
        let machine = Machine.create false (1,3) (25,25) (2,6)
        let expected = Some 6L
    
        Assert.Equal(expected, solve machine)

    [<Fact>]
    let ``sample machine 1`` () =
        let machine = Machine.create false (94,34) (22,67) (8400, 5400)
        let expected = Some 280L

        Assert.Equal(expected, solve machine)

module PartTests =

    [<Fact>]
    let ``part1: sample`` () =
        let input = loadInput sample
        let expected = 480L

        Assert.Equal(expected, part1 input)

    [<Fact>]
    let ``part2: sample`` () =
        let input = loadInput ""
        let expected = 0L

        Assert.Equal(expected, part2 input)
