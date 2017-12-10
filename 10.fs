open System

let source = "14,58,0,116,179,16,1,104,2,254,167,86,255,55,122,244"

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

let doRound lengths tape (skip: int ref) (pos: int ref) =
    for length in lengths do
        let segment = getSegment tape !pos length
        let reverted = revertSegment segment

        for item in reverted do
            tape.[fst item] <- snd item

        pos := (!pos + length + !skip) % tape.Length
        skip := !skip + 1

let answer1 =
    let lengths = 
        source.Split([| "," |], StringSplitOptions.None)
        |> Array.map int

    let mutable tape = [| 0 .. 255 |]
    let skip = ref 0
    let pos = ref 0

    doRound lengths tape skip pos

    tape.[0] * tape.[1]

let answer2 =
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

    let chars =
        { 0 .. 15 }
        |> Seq.map (getDenseHashChar tape)
        |> Seq.map (fun x -> x.ToString("x2"))
    
    String.Join("", chars)