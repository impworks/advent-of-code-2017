open System

let getSequence factor start mult =
    let factor64 = int64 factor
    let max = int64 Int32.MaxValue
    let mutable curr = int64 start
    seq {
        while true do
            curr <- (curr * factor64) % max
            if mult = None || curr % (int64 mult.Value) = 0L then
                yield int curr
    }

let compare (a, b) =
    (a &&& 0xFFFF) = (b &&& 0xFFFF)

let answer takes mult1 mult2 =
    let seq1 = getSequence 16807 591 mult1
    let seq2 = getSequence 48271 393 mult2

    seq1
    |> Seq.zip seq2
    |> Seq.take takes
    |> Seq.where compare
    |> Seq.length

let answer1 =
    answer 40000000 None None

let answer2 =
    answer 5000000 (Some 4) (Some 8)