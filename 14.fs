open System
open System.Collections

let input = "vbqugkhl"

let getHash (source:string) =
    let doRound lengths tape (skip: int ref) (pos: int ref) =
        let getSegment tape pos length =
            Array.append tape tape
            |> Array.mapi (fun idx item -> (idx % tape.Length, item))
            |> Array.skip pos
            |> Array.take length

        let revertSegment (segment:('a * 'b)[]) =
            let mapper idx item =
                let wrappedIdx = segment.Length - idx - 1
                let newIdx = fst item
                let newValue = snd segment.[wrappedIdx]
                (newIdx, newValue)

            Array.mapi mapper segment

        for length in lengths do
            let segment = getSegment tape !pos length
            let reverted = revertSegment segment

            for item in reverted do
                tape.[fst item] <- snd item

            pos := (!pos + length + !skip) % tape.Length
            skip := !skip + 1

    let getDenseHashChar (tape: byte[]) pos =
        let blockSize = 16
        tape
        |> Array.skip (pos * blockSize)
        |> Array.take blockSize
        |> Seq.fold (^^^) (byte 0)

    let srcLengths = source.ToCharArray () |> Array.map (byte >> int)
    let lengths = Array.append srcLengths [| 17; 31; 73; 47; 23 |]

    let mutable tape = Array.map byte [| 0 .. 255 |]
    let skip = ref 0
    let pos = ref 0

    for _ in 1 .. 64 do
        doRound lengths tape skip pos

    Array.map (getDenseHashChar tape) [| 0 .. 15 |]

let grid =
    let getRow (bytes: byte[]) =
        seq {
            for currByte in bytes do
                let ba = BitArray ([| currByte |])
                for i = ba.Length - 1 downto 0 do
                    yield if ba.[i] then Some false else None
        } |> Seq.toArray

    [| 0..127 |]
    |> Array.map ((sprintf "%s-%d" input) >> getHash >> getRow)

let answer1 =
    grid
    |> Seq.collect id
    |> Seq.filter Option.isSome
    |> Seq.length

let answer2 =
    let isUnvisited x y =
        grid.[y].[x] = Some false

    let rec markSegment x y =
        let isInRange = x >= 0 &&
                        x < grid.[0].Length &&
                        y >= 0 &&
                        y < grid.Length

        if isInRange && (isUnvisited x y) then
            grid.[y].[x] <- Some true

            for offset in [ (0, 1); (0, -1); (1, 0); (-1, 0) ] do
                markSegment (x + fst offset) (y + snd offset)

    let segments = ref 0

    for y = 0 to grid.Length - 1 do
        for x = 0 to grid.[y].Length - 1 do
            if isUnvisited x y then
                markSegment x y
                incr segments

    !segments