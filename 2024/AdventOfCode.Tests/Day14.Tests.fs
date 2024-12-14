module Tests.Day14

open System
open Xunit
open Day14

let sample = "p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3"

let loadInput (raw: string) =
    raw.Split(Environment.NewLine)
    |> Seq.map Robot.create

module Create =

    [<Fact>]
    let ``works with all number types`` () =
        let input = "p=0,4 v=3,-3"
        let expected = { Pos = (0,4); Vel = (3,-3) }
        
        Assert.Equal(expected, Robot.create input)

module Advance =
    let dim = (11,7)

    [<Fact>]
    let ``one second, SE, no wrap`` () =
        let robot = { Pos = 0,0; Vel = 1,2 }
        let expected = (1,2)
        
        Assert.Equal(expected, Robot.advance dim 1 robot)

    [<Fact>]
    let ``one second, NW, no wrap`` () =
        let robot = { Pos = 2,3; Vel = -1,-2 }
        let expected = (1,1)
        
        Assert.Equal(expected, Robot.advance dim 1 robot)

    [<Fact>]
    let ``two seconds, SE, no wrap`` () =
        let robot = { Pos = 0,0; Vel = 1,2 }
        let expected = (2,4)
        
        Assert.Equal(expected, Robot.advance dim 2 robot)

    [<Fact>]
    let ``two seconds, NW, no wrap`` () =
        let robot = { Pos = 5,6; Vel = -1,-2 }
        let expected = (3,2)
        
        Assert.Equal(expected, Robot.advance dim 2 robot)

    [<Fact>]
    let ``three seconds, E, wraps`` () =
        let robot = { Pos = 0,0; Vel = 4,0 }
        let expected = (1,0)
        
        Assert.Equal(expected, Robot.advance dim 3 robot)

    [<Fact>]
    let ``three seconds, E, wraps, start near edge`` () =
        let robot = { Pos = 9,0; Vel = 4,0 }
        let expected = (10,0)
        
        Assert.Equal(expected, Robot.advance dim 3 robot)

    [<Fact>]
    let ``three seconds, S, wraps`` () =
        let robot = { Pos = 0,0; Vel = 0,4 }
        let expected = (0,5)
        
        Assert.Equal(expected, Robot.advance dim 3 robot)

    [<Fact>]
    let ``three seconds, W, wraps`` () =
        let robot = { Pos = 4,5; Vel = -3,0 }
        let expected = (6,5)
        
        Assert.Equal(expected, Robot.advance dim 3 robot)

    [<Fact>]
    let ``three seconds, N, wraps`` () =
        let robot = { Pos = 8,1; Vel = 0,-5 }
        let expected = (8,0)
        
        Assert.Equal(expected, Robot.advance dim 3 robot)

module Solve =

    [<Fact>]
    let ``part1: sample`` () =
        let robots = loadInput sample
        let expected = 12

        Assert.Equal(expected, part1 robots (11,7) 100)
