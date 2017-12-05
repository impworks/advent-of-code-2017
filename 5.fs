open System.IO
let source =
    File.ReadAllLines "5.txt"
    |> Array.map int

let execute handler =
    let mutable steps = 0
    let mutable pos = 0
    let mutable isCompleted = false
    let src = source |> Seq.toArray

    while not isCompleted do
        let nextPos = pos + src.[pos]
        src.[pos] <- handler src.[pos]
        steps <- steps + 1
        pos <- nextPos
        isCompleted <- pos >= src.Length

    steps

let answer1 = execute ((+) 1)
let answer2 = execute (fun x -> if x >= 3 then x - 1 else x + 1)