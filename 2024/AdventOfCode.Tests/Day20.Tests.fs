module Tests.Day20

open System
open System.Collections.Generic
open Xunit
open AdventOfCode.Common
open Day20

let loadInput (raw: string) =
    raw.Split(Environment.NewLine)

let sample = "###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############"

module Parse =

    [<Fact>]
    let ``parse initial test`` () =
        let input = loadInput "#.SE"
        
        let expected = array2D [[ '#'; '.'; 'S'; 'E' ]]

        Assert.Equal(expected, parse input)

    [<Fact>]
    let ``parse sample`` () =
        let input = loadInput sample
        let result = parse input

        Assert.Equal(15, Array2D.length1 result)
        Assert.Equal(15, Array2D.length2 result)
        Assert.Equal('S', result[3,1])
        Assert.Equal('E', result[7,5])

module walkPath =

     [<Fact>]
     let ``test sample`` () =
        let track = sample |> loadInput |> parse
        let start = track |> Array2D.findIndex (fun x -> x = 'S')
        let maxCheats = 2
        let expectedFairTime = 84
        
        let visited = walkPath track start
        
        Assert.Equal(expectedFairTime, visited[7, 5])
        Assert.Equal(expectedFairTime, visited.Count - 1)

module PossibleCheats =    

     [<Fact>]
     let ``find cheats with one cheat possibility`` () =
        let input = "####
#SE#
####"
        let track = input |> loadInput |> parse
        let start = track |> Array2D.findIndex (fun x -> x = 'S')
        let maxCheats = 2
        let expectedCheats = dict [ (1,1), set [(1, 2)] ]
       
        let visited = walkPath track start
        let possibleCheats = possibleCheats track maxCheats visited
        
        Assert.Equivalent(expectedCheats, possibleCheats)
   
     [<Fact>]
     let ``find cheats with no effective cheats`` () =
        let input = "#####
#..E#
#.###
#S###
#####"
        let track = input |> loadInput |> parse
        let start = track |> Array2D.findIndex (fun x -> x = 'S')
        let maxCheats = 2
        let expectedCheats =
            dict [ (3,1), set [(2, 1); (1, 1)]
                   (2,1), set [(1, 1); (1, 2)]
                   (1,1), set [(1, 2); (1, 3)]
                   (1,2), set [(1, 3)] ]
       
        let visited = walkPath track start
        let possibleCheats = possibleCheats track maxCheats visited
        
        Assert.Equivalent(expectedCheats, possibleCheats)
   
     [<Fact>]
     let ``find cheats with single effective cheat`` () =
        let input = "#####
#...#
#.#E#
#S###
#####"
        let track = input |> loadInput |> parse
        let start = track |> Array2D.findIndex (fun x -> x = 'S')
        let maxCheats = 2
        let expectedCheats =
            dict [ (3,1), set [(2, 1); (1, 1)]
                   (2,1), set [(1, 1); (1, 2); (2, 3)]
                   (1,1), set [(1, 2); (1, 3)]
                   (1,2), set [(1, 3); (2, 3)]
                   (1,3), set [(2, 3)] ]
       
        let visited = walkPath track start
        let possibleCheats = possibleCheats track maxCheats visited

        Assert.Equivalent(expectedCheats, possibleCheats)

module CheatCount =
   
    [<Fact>]
    let ``part1: cheat with no effective cheats on single step`` () =
        let input = "####
#SE#
####"
        let track = input |> loadInput |> parse
        let start = track |> Array2D.findIndex (fun x -> x = 'S')
        let maxCheats = 2
        let timeSaved = 2
        let expected = 0
        
        let visited = walkPath track start

        Assert.Equal(expected, cheatCount track maxCheats timeSaved visited)
   
    [<Fact>]
    let ``part1: cheat with no effective cheats on multiple steps`` () =
        let input = "#####
#..E#
#.###
#S###
#####"
        let track = input |> loadInput |> parse
        let start = track |> Array2D.findIndex (fun x -> x = 'S')
        let maxCheats = 2
        let timeSaved = 2
        let expected = 0
        
        let visited = walkPath track start

        Assert.Equal(expected, cheatCount track maxCheats timeSaved visited)
   
    [<Fact>]
    let ``part1: cheat with an effective cheat with desired time saving`` () =
        let input = "#####
#...#
#.#E#
#S###
#####"
        let track = input |> loadInput |> parse
        let start = track |> Array2D.findIndex (fun x -> x = 'S')
        let maxCheats = 2
        let timeSaved = 2
        let expected = 1
        
        let visited = walkPath track start

        Assert.Equal(expected, cheatCount track maxCheats timeSaved visited)

    [<Fact>]
    let ``part 2: cheat with savings of 76 from sample`` () =
        let input = sample |> loadInput
        let track = parse input
        let start = track |> Array2D.findIndex (fun x -> x = 'S')
        let maxCheats = 20
        let timeSaved = 76
        let expected = 3
        
        let visited = walkPath track start

        Assert.Equal(expected, cheatCount track maxCheats timeSaved visited)

module Race =

     [<Theory>]
     [<InlineData(2, 44)>]
     [<InlineData(4, 30)>]
     [<InlineData(6, 16)>]
     [<InlineData(8, 14)>]
     [<InlineData(10, 10)>]
     [<InlineData(12, 8)>]
     [<InlineData(20, 5)>]
     [<InlineData(36, 4)>]
     [<InlineData(38, 3)>]
     [<InlineData(40, 2)>]
     [<InlineData(64, 1)>]
     let ``part1: sample w/ different time savings`` (timeSaved: int, expected: int) =
         let input = sample |> loadInput
         let maxCheats = 2
    
         Assert.Equal(expected, race input maxCheats timeSaved)
    
     [<Theory>]
     [<InlineData(50, 285)>]
     [<InlineData(52, 253)>]
     [<InlineData(54, 222)>]
     [<InlineData(56, 193)>]
     [<InlineData(58, 154)>]
     [<InlineData(60, 129)>]
     [<InlineData(62, 106)>]
     [<InlineData(64, 86)>]
     [<InlineData(66, 67)>]
     [<InlineData(68, 55)>]
     [<InlineData(70, 41)>]
     [<InlineData(72, 29)>]
     [<InlineData(74, 7)>]
     [<InlineData(76, 3)>]
     let ``part2 test`` (timeSaved: int, expected: int) =
         let input = sample |> loadInput
         let track = parse input
         let start = track |> Array2D.findIndex (fun x -> x = 'S')
         let maxCheats = 20
             
         Assert.Equal(expected, race input maxCheats timeSaved)
