module Day5

open System.IO

let toOrderingRulePair (line: string) =
    line.Split('|')
    |> fun x -> x.[0] |> int, x.[1] |> int

let toDependencyMap pairs : Map<int, int list> =
    pairs
    |> Seq.fold (fun current (page, beforePage) ->
        current.Change(page, fun x ->
            match x with
            | Some pages -> pages @ [ beforePage ] |> List.sort |> List.distinct |> Some
            | None -> [ beforePage ] |> Some
        )
    ) Map[]

let toUpdateList (line: string) =
    line.Split(',') |> Seq.map int |> List.ofSeq

let loadData filename =
    let raw = filename |> File.ReadLines |> List.ofSeq

    let (orderingRuleLines, updateLines) =
        List.splitAt (raw |> List.findIndex (fun x -> x = "")) raw

    (orderingRuleLines |> List.map toOrderingRulePair |> toDependencyMap,
     updateLines |> List.filter (fun x -> x <> "") |> List.map toUpdateList)


let updateIsValid rules (update: int list) =
    update
    |> List.mapi (fun i page ->
        match rules |> Map.tryFind page with
        | Some rules ->
            update.[0..i-1]
            |> List.forall (fun x -> List.contains x rules |> not)
        | None -> true )
    |> List.forall id
    
let getMiddlePage (update: int list) =
    update.[List.length update / 2]

// -- part 1 -------------------------------------------------------------------

let part1 updates rules =
    updates
    |> List.filter (updateIsValid rules)
    |> List.map getMiddlePage
    |> List.sum

printfn $"part 1 result: {part1}"

// -- part 2 -------------------------------------------------------------------

let fixUpdate rules (update: int list) =
    update
    |> List.fold ( fun current page ->
        if Map.containsKey page rules
        then
            match current |> List.tryFindIndex (fun x -> List.contains x rules[page]) with
            | Some i -> List.insertAt i page current
            | None -> current @ [ page ]
        else current @ [ page ] ) []

let part2 updates rules =
    updates
    |> List.filter (updateIsValid rules >> not)
    |> List.map (fixUpdate rules)
    |> List.map getMiddlePage
    |> List.sum

let run () =
    let (rules, updates) = loadData "./Input/day5.txt"

    part1 updates rules
    |> printfn "[Day 5] Part 1: %d"

    part2 updates rules
    |> printfn "[Day 5] Part 2: %d"
