module Tests.Day21

open Xunit
open Day21

module GetKeypadCommands =
    
    [<Fact>]
    let ``GetKeypadCommands for 029A`` () =
        let buttons = "029A"
        let expected = "<A^A^^>AvvvA"
        
        Assert.Equal(expected, getKeypadCommands buttons)

    [<Fact>]
    let ``GetKeypadCommands for 379A`` () =
        let buttons = "379A"
        let expected = "^A<<^^A>>AvvvA"

        Assert.Equal(expected, getKeypadCommands buttons)

module GetRemoteCommandLength =
     
     [<Fact>]
     let ``One robot for 029A`` () =
         let buttons =  getKeypadCommands "029A"
         let expected = 28L // "v<<A>>^A<A>A<AAv>A^A<vAAA^>A"

         let actual = getRemoteCommandLength 1 buttons
         
         Assert.Equal(expected, actual)

     [<Fact>]
     let ``Two robots for 029A`` () =
         let buttons = getKeypadCommands "029A"
         let expected = 68L // "<vA<AA>>^AvAA<^A>Av<<A>>^AvA^Av<<A>>^AA<vA>A^A<A>Av<<A>A^>AAA<Av>A^A"
     
         let actual = getRemoteCommandLength 2 buttons
         
         Assert.Equal(expected, actual)
     
     [<Fact>]
     let ``One robot for 379A`` () =
         let buttons =  getKeypadCommands "379A" // <A>Av<<AA>^AA>AvAA^A<vAAA^>A
         let expected = 28L

         let actual = getRemoteCommandLength 1 buttons
         
         Assert.Equal(expected, actual)

     [<Fact>]
     let ``Two robots for 379A`` () =
         let buttons = getKeypadCommands "379A"
         let expected = 64L // "<v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A"
     
         let actual = getRemoteCommandLength 2 buttons
         
         Assert.Equal(expected, actual)

module Complexity =

    [<Theory>]
    [<InlineData("029A", 1972L)>]
    [<InlineData("980A", 58800L)>]
    [<InlineData("179A", 12172L)>]
    [<InlineData("456A", 29184L)>]
    [<InlineData("379A", 24256L)>]
    let ``complexity for two robots entering each sample code`` (code:string, expected: int64) =
        let actual =
            getKeypadCommands code
            |> getRemoteCommandLength 2
            |> complexity code

        Assert.Equal(expected, actual)

module Part1 =

    [<Fact>]
    let ``part1 on single sample codes`` () =
        let codes = ["029A"; "980A"; "179A"; "456A"; "379A" ]
        let expected = 126384L
        Assert.Equal(expected, solve 2 codes)

