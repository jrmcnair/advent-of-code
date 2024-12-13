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

module ClawMachine =

    [<Fact>]
    let ``create: button A Efficient`` () =
        let expected =
          { ButtonA = (4,4)
            ButtonB = (1,1)
            EfficientButton = A
            PrizeLocation = (200, 100)
            MaxIterations = 201 }
        let actual = ClawMachine.create (4,4) (1,1) (200, 100)

        Assert.Equivalent(expected, actual)

    [<Fact>]
    let ``create: button B Efficient`` () =
        let expected =
          { ButtonA = (2,2)
            ButtonB = (1,1)
            EfficientButton = B
            PrizeLocation = (200, 100)
            MaxIterations = 201 }
        let actual = ClawMachine.create (2,2) (1,1) (200, 100)

        Assert.Equivalent(expected, actual)

module Parse =

    [<Fact>]
    let ``single line`` () =
        let input = loadInput sample |> Seq.take 4
        let expected = [
            ClawMachine.create (94,34) (22,67) (8400,5400)
        ]

        Assert.Equivalent(expected, input |> parse)

    [<Fact>]
    let ``multiple lines`` () =
        let expected = [
            ClawMachine.create (94,34) (22,67) (8400,5400)
            ClawMachine.create (26,66) (67,21) (12748,12176)
            ClawMachine.create (17,86) (84,37) (7870,6450)
            ClawMachine.create (69,23) (27,71) (18641,10279)
        ]

        Assert.Equivalent(expected, loadInput sample |> parse)

module tokensToPrize =

    [<Fact>]
    let ``can get to price with efficient button only`` () =
        let machine = ClawMachine.create (4,4) (1,1) (8,8)
        let expected = 6

        Assert.Equal(expected, tokensToPrize machine)

    [<Fact>]
    let ``can get to price with inefficient button only`` () =
        let machine = ClawMachine.create (1,1) (4,4) (8,8)
        let expected = 2

        Assert.Equal(expected, tokensToPrize machine)

    [<Fact>]
    let ``must use a mix`` () =
        let machine = ClawMachine.create (4,4) (1,1) (9,9)
        let expected = 7

        Assert.Equal(expected, tokensToPrize machine)

    [<Fact>]
    let ``sample machine 1`` () =
        let machine = ClawMachine.create (94,34) (22,67) (8400, 5400)
        let expected = 280

        Assert.Equal(expected, tokensToPrize machine)

module Parts =

    [<Fact>]
    let ``part1: sample`` () =
        let input = loadInput sample
        let expected = 480

        Assert.Equal(expected, part1 input)

    [<Fact>]
    let ``part2: sample`` () =
        let input = loadInput ""
        let expected = 0

        Assert.Equal(expected, part2 input)
