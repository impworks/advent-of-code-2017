open System
open System.IO

let source = File.ReadAllText "2.txt"

let numbers =
    let split (delim:string) (str:string) =
        str.Split([| delim |], StringSplitOptions.RemoveEmptyEntries)

    split "\n" source
    |> Array.map (fun x -> (split "\t" x) |> Array.map int)

let diff row =
    let min = Seq.min row
    let max = Seq.max row
    max - min

let division (row:int[]) =
    let pairs = seq {
        for idx1 = 0 to row.Length - 1 do
            for idx2 = 0 to row.Length - 1 do
                if idx1 <> idx2 then
                    yield (double row.[idx1], double row.[idx2])
    }
    pairs
    |> Seq.map (fun pair -> fst pair / snd pair)
    |> Seq.tryFind (fun value -> (value - (Math.Round value)) = 0.0)

let answer1 = numbers |> Seq.sumBy diff
let answer2 =
    numbers
    |> Seq.map division
    |> Seq.choose id
    |> Seq.sum
    |> int