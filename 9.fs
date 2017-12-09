open System
open System.IO
open System.Collections.Generic

type Group =
    {
        Depth: int;
    }

let source = File.ReadAllText "9.txt"

let answers =
    let groups = new List<Group> ()
    let mutable depth = 0
    let mutable isGarbage = false
    let mutable isEscape = false
    let mutable garbageSize = 0

    for index = 0 to source.Length - 1 do
        let ch = source.[index]
        if isGarbage then
            if isEscape then
                isEscape <- false
            else
                match ch with
                | '!' -> isEscape <- true
                | '>' -> isGarbage <- false
                | _ -> garbageSize <- garbageSize + 1
        else                
            match ch with
            | '{' ->
                depth <- depth + 1
                groups.Add { Depth = depth }
            | '}' -> depth <- depth - 1
            | '<' -> isGarbage <- true
            | ',' -> ()

    (groups |> Seq.sumBy (fun x -> x.Depth), garbageSize)
