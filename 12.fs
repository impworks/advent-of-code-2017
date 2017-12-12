open System
open System.IO
open System.Collections.Generic

let input =
    let parse line =
        let split (str:string) (delim:string) =
            str.Split([| delim |], StringSplitOptions.None)

        let parts = split line " <-> "
        let refs = split parts.[1] ", "
        Array.map int refs

    File.ReadAllLines "12.txt"
    |> Array.map parse

let hasPath dest src =
    let traversed = HashSet<_> ()
    let rec hasPathRec dest src =
        if dest = src then
            true
        else
            traversed.Add src |> ignore
            input.[src]
            |> Array.where (traversed.Contains >> not)
            |> Array.exists (hasPathRec dest)
    hasPathRec dest src

let indices = { 0 .. input.Length - 1 }
let groupMembers elem = Seq.where (hasPath elem) indices

let answer1 =
    Seq.length (groupMembers 0)

let answer2 =
    let excluded = HashSet<int> ()
    let groups = ref 0

    while excluded.Count < input.Length do
        let initial =
            indices
            |> Seq.filter (excluded.Contains >> not)
            |> Seq.head

        let elems = groupMembers initial
        for elem in elems do
            excluded.Add elem |> ignore
        
        incr groups

    !groups