module Day9

open System.IO

let diskMap (input: string) =
    input
    |> _.ToCharArray()
    |> Seq.map (fun x -> x |> string |> int)
    |> Seq.chunkBySize 2

module Part1 =

    type Block = | File of int | Free
    module Block =
        let isFile = function | File _ -> true | _ -> false
        let isFree = function | Free -> true | _ -> false

    let toBlocks (id: int) (values: int[]) =
        seq {
            for _ = 1 to values[0] do
                yield File id

            if values.Length = 2 then
                for _ = 1 to values[1] do
                    yield Free
        }

    let compress (uncompressed: Block seq) : Block [] =
        let mutable filesystem = Array.ofSeq uncompressed

        let rec go () =
            let nextFreePos = filesystem |> Seq.findIndex Block.isFree
            let lastFilePos = filesystem |> Seq.findIndexBack Block.isFile
           
            if nextFreePos < lastFilePos then
                filesystem[nextFreePos] <- filesystem[lastFilePos]
                filesystem[lastFilePos] <- Free
                go ()

        if filesystem |> Array.exists Block.isFree then go ()

        filesystem

    let checksum (compressed: Block[]) : int64 =
        compressed
        |> Seq.indexed
        |> Seq.choose (fun (i, b) ->
            match b with
            | File id -> Some(int64 i * int64 id)
            | Free -> None)
        |> Seq.sum

    let run (input: string) =
        diskMap input
        |> Seq.mapi toBlocks
        |> Seq.concat
        |> compress
        |> checksum

module Part2 =

    type Block =
        | File of id: int * size: int
        | Free of size: int
    module Block =
        let size = function | File (_, size) -> size | Free size -> size
        let isFile = function | File _ -> true | _ -> false
        let isFree = function | Free _ -> true | _ -> false
        let isFreeAndFits (size: int) = function | Free s -> s >= size | File _ -> false
        let toNums = function
            | Free s -> Seq.init s (fun _ -> 0)
            | File (i, s) -> Seq.init s (fun _ -> i) 

    let toBlocks (id: int) (values: int[]) =
        seq {
            yield File (id, values[0])
            if values.Length = 2 then yield Free values[1]
        }

    let compressByFile (uncompressed: Block seq) : Block[] =
        let mutable filesystem = Array.ofSeq uncompressed
        
        let rec go (pos: int) =
            if pos > 0 then
                let block = filesystem[pos]
                
                if Block.isFile block then
                    let fileSize = Block.size block

                    match filesystem |> Array.tryFindIndex (Block.isFreeAndFits fileSize) with
                    | Some freePos when freePos < pos ->
                        let freeBlock = filesystem[freePos]
                        let freeSize = Block.size freeBlock
                        let sizeDiff = freeSize - fileSize

                        filesystem[pos] <- Free fileSize
                        filesystem[freePos] <- block
                        
                        if sizeDiff > 0 then
                            filesystem <- Array.insertAt (freePos + 1) (Free sizeDiff) filesystem
                    | _ -> ()

                go (pos - 1)

        go (filesystem.Length - 1)

        filesystem
    
    let checksum (compressed: Block[]) =
        compressed
        |> Seq.map (Block.toNums)
        |> Seq.concat
        |> Seq.mapi (fun i n -> int64 i * int64 n)
        |> Seq.sum

    let run (input: string) =
        diskMap input
        |> Seq.mapi toBlocks
        |> Seq.concat
        |> compressByFile
        |> checksum

let run () =
    let input = File.ReadAllLines "./Input/day9.txt" |> Seq.head

    Part1.run input
    |> printfn "[Day 9] Part 1: %d"

    Part2.run input
    |> printfn "[Day 9] Part 2: %d"
