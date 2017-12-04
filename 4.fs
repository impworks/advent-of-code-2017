open System.IO
open System
let source = File.ReadAllLines "4.txt"

let isValid (proj:string -> string) (line:string) =
    line.Split( [| " " |], StringSplitOptions.RemoveEmptyEntries)
    |> Seq.groupBy proj 
    |> Seq.map (snd >> Seq.length)
    |> Seq.exists (fun x -> x > 1)
    |> not

let normalize (str:string) =
    let chars =
        str.ToCharArray ()
        |> Seq.sort
        |> Seq.toArray
    new string (chars)

let answer proj =
    source
    |> Seq.filter (isValid proj)
    |> Seq.length

let answer1 = answer id
let answer2 = answer normalize