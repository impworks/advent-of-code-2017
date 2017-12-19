open System
open System.IO

let input =
    File.ReadAllLines "19.txt"
    |> Array.map (fun line -> line.ToCharArray())

let startPos =
    (input.[0] |> Seq.findIndex (fun x -> x <> ' '), -1)

let (+>>) pos dir =
    (fst pos + fst dir, snd pos + snd dir)

let getChar pos =
    input.[snd pos].[fst pos]

let getNextDirection pos dir =
    let dirs =
        if (fst dir) = 0
        then [ (-1, 0); (1, 0); ]
        else [ (0, -1); (0, 1); ]

    dirs
    |> Seq.where (fun dir -> getChar (pos +>> dir) <> ' ')
    |> Seq.head

let answer1 =
    let rec walkChars oldPos dir chars =
        let currPos = oldPos +>> dir
        match getChar currPos with
        | '-' | '|' -> walkChars currPos dir chars
        | '+' ->
            let nextDir = getNextDirection currPos dir
            walkChars currPos nextDir chars
        | ' ' -> chars
        | ch -> walkChars currPos dir (ch :: chars)

    let chars =
        walkChars startPos (0, 1) List.empty
        |> List.rev
        |> List.toArray
        
    String(chars)

let answer2 =
    let rec walkLength oldPos dir len =
        let currPos = oldPos +>> dir
        match getChar currPos with
        | '-' | '|' -> walkLength currPos dir (len + 1)
        | '+' ->
            let nextDir = getNextDirection currPos dir
            walkLength currPos nextDir (len + 1)
        | ' ' -> len
        | _ -> walkLength currPos dir (len + 1)

    walkLength startPos (0, 1) 0