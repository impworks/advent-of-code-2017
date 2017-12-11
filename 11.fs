open System
open System.IO

type HexCoordinate =
    {
        X: int;
        Y: int;
        Z: int;
    }

let source =
    let text = File.ReadAllText "11.txt"
    text.Split ','

let step coord dir =
    match dir with
    | "n"  -> { X = coord.X; Y = coord.Y + 1; Z = coord.Z - 1 }
    | "s"  -> { X = coord.X; Y = coord.Y - 1; Z = coord.Z + 1 }
    | "ne" -> { Y = coord.Y; X = coord.X + 1; Z = coord.Z - 1 }
    | "sw" -> { Y = coord.Y; X = coord.X - 1; Z = coord.Z + 1 }
    | "nw" -> { Z = coord.Z; Y = coord.Y + 1; X = coord.X - 1 }
    | "se" -> { Z = coord.Z; Y = coord.Y - 1; X = coord.X + 1 }

let distance coord =
    [ coord.X; coord.Y; coord.Z ]
    |> Seq.sumBy (fun x -> Math.Max(0, x))

let answer1 =
    let first = { X = 0; Y = 0; Z = 0 }
    let last = source |> Seq.fold step first
    distance last

let answer2 = 
    let first = { X = 0; Y = 0; Z = 0 }
    let mutable curr = first
    let mutable max = 0

    for dir in source do
        let dist = distance curr
        max <- Math.Max(max, dist)
        curr <- step curr dir

    max