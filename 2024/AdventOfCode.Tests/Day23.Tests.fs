module Tests.Day23

open System.Collections.Generic
open Xunit
open Day23
open AdventOfCode.Tests.Common

let sample = "kh-tc
qp-kh
de-cg
ka-co
yn-aq
qp-ub
cg-tb
vc-aq
tb-ka
wh-tc
yn-cg
kh-ub
ta-co
de-co
tc-td
tb-wq
wh-td
ta-ka
td-qp
aq-cg
wq-ub
ub-vc
de-ta
wq-aq
wq-vc
wh-yn
ka-de
kh-ta
co-tc
wh-qp
tb-vc
td-yn"

[<Fact>]
let ``parse sample`` () =
    let input = loadInput sample
    let expected = [| "kh", "tc"; "qp", "kh"; "de", "cg"; "ka", "co"; "yn", "aq" |]
    let actual = parse input |> Array.ofSeq
    
    Assert.Equivalent(expected, actual[0..4])

[<Fact>]
let ``connections sample`` () =
    let pairs = [ "a", "b"; "b", "c"; "a", "c" ]
    let expected = dict [
        "a", HashSet([ "b"; "c" ])
        "b", HashSet([ "a" ])
        "c", HashSet([ "a" ])
    ]

    Assert.Equivalent(expected, connections pairs)

[<Fact>]
let ``find triangles with simple data`` () =
    let pairs = [ "ta", "aa"; "aa", "ab"; "ta", "ab" ]
    let links = connections pairs 
    let expected = [ set [ "aa"; "ab"; "ta" ] ]
    
    Assert.Equivalent(expected, triangles links)
    
[<Fact>]
let ``find triangles with sample data`` () =
    let links = sample |> loadInput |> parse |> connections 
    let expected = [
        set [ "co"; "de"; "ta" ]
        set [ "co"; "ka"; "ta" ]
        set [ "de"; "ka"; "ta" ]
        set [ "qp"; "td"; "wh" ]
        set [ "tb"; "vc"; "wq" ]
        set [ "tc"; "td"; "wh" ]
        set [ "td"; "wh"; "yn" ]
    ]
    
    Assert.Equivalent(expected, triangles links)
