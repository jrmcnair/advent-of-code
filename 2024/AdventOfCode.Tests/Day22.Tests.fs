module Tests.Day22

open System
open Xunit
open Day22

let loadInput (raw: string) = raw.Split(Environment.NewLine)

let sample = "1
10
100
2024"

module Parse =

    [<Fact>]
    let ``parse sample`` () =
        let input = sample |> loadInput
        let expected = [ 1; 10; 100; 2024 ]

        Assert.Equivalent(expected, parse input)

module Mix =
    
    [<Fact>]
    let ``mix example`` () =
        let secret = 42L
        let mixValue = 15L
        let expected = 37L
        
        Assert.Equal(expected, mix secret mixValue)

module Prune =
    
    [<Fact>]
    let ``prune example`` () =
        let secret = 100000000L
        let expected = 16113920L
        
        Assert.Equal(expected, prune secret)

module Next =

    [<Theory>]
    [<InlineData(123L, 15887950L)>]
    [<InlineData(15887950L, 16495136L)>]
    [<InlineData(16495136L, 527345L)>]
    [<InlineData(527345L, 704524L)>]
    [<InlineData(704524L, 1553684L)>]
    [<InlineData(1553684L, 12683156L)>]
    [<InlineData(12683156L, 11100544L)>]
    [<InlineData(11100544L, 12249484L)>]
    [<InlineData(12249484L, 7753432L)>]
    [<InlineData(7753432L, 5908254L)>]
    let ``first 10 iterations of 123 secret`` (secret: int64, expected: int64) =
        Assert.Equal(expected, next secret)
    
module GetSecret =

    [<Theory>]
    [<InlineData(123L, 1, 15887950L)>]
    [<InlineData(123L, 2, 16495136L)>]
    [<InlineData(123L, 3, 527345L)>]
    [<InlineData(123L, 4, 704524L)>]
    [<InlineData(123L, 5, 1553684L)>]
    [<InlineData(123L, 6, 12683156L)>]
    [<InlineData(123L, 7, 11100544L)>]
    [<InlineData(123L, 8, 12249484L)>]
    [<InlineData(123L, 9, 7753432L)>]
    [<InlineData(123L, 10, 5908254L)>]
    let ``first 10 iterations of 123 secret`` (initial: int64, iterations: int, expected: int64) =
        Assert.Equal(expected, getSecret iterations initial)


    [<Theory>]
    [<InlineData(1L, 8685429L)>]
    [<InlineData(10L, 4700978L)>]
    [<InlineData(100L, 15273692L)>]
    [<InlineData(2024L, 8667524L)>]
    let ``sample entries at 2000 iterations`` (initial: int64, expected: int64) =
        Assert.Equal(expected, getSecret 2000 initial)

module Price =
    
    [<Theory>]
    [<InlineData(123L, 3)>]
    [<InlineData(15887950L, 0)>]
    [<InlineData(16495136L, 6)>]
    [<InlineData(527345L, 5)>]
    let ``example prices from 123 secrets`` (secret: int64, expected: int) =
        Assert.Equal(expected, price secret)
    
    [<Fact>]
    let ``getPrices from 4 itersations of secret 123`` () =
        let secret = 123L
        let expected = [| 3; 0; 6; 5 |]

        Assert.Equivalent(expected, getPrices 4 secret)
    
    [<Fact>]
    let ``getPriceChangesfrom 4 itersations of secret 123`` () =
        let secret = 123L
        let prices = getPrices 4 secret
        let expected = [| (0, -3); (6, 6); (5, -1) |]
        
        Assert.Equivalent(expected, getPriceChanges prices)
