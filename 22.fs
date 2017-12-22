open System
open System.Collections.Generic
open System.IO

type Position =
    {
        X: int;
        Y: int;
        Dir: int;
    }

type State =
    | Clean
    | Weakened
    | Infected
    | Flagged

type Map () =
    let map =
        let parseLine (line:string) =
            line.ToCharArray ()
            |> Array.map (fun x -> if x = '#' then Infected else Clean)

        let source = 
            File.ReadAllLines "22.txt"
            |> Array.map parseLine

        let height = source.Length
        let width = source.[0].Length

        let result = Dictionary<(int * int), State>()

        for y = 0 to height - 1 do
            for x = 0 to width - 1 do
                let pos = (x - width/2, y - height/2);
                result.[pos] <- source.[y].[x]

        result
    member this.getState (pos: int * int) =
        if map.ContainsKey pos then map.[pos] else Clean

    member this.toggleState1 (pos: int * int) =
        map.[pos] <-
            match this.getState pos with
            | Clean -> Infected
            | _ -> Clean

    member this.toggleState2 (pos: int * int) =
        map.[pos] <-
            match this.getState pos with
            | Clean -> Weakened
            | Weakened -> Infected
            | Infected -> Flagged
            | Flagged -> Clean

let step pos state =
    let rotation = match state with
    | Clean -> -90
    | Weakened -> 0
    | Infected -> 90
    | Flagged -> 180

    let newDir = pos.Dir + rotation
    let newDirRads = (double newDir) * Math.PI / 180.0
    let xOff = int (Math.Cos newDirRads)
    let yOff = int (Math.Sin newDirRads)
    { X = pos.X + xOff; Y = pos.Y + yOff; Dir = newDir }

let answer1 =
    let map = Map ()
    let mutable pos = { X = 0; Y = 0; Dir = -90 }
    let count = ref 0

    for i = 1 to 10000 do
        let state = map.getState (pos.X, pos.Y)
        if state = Clean then incr count
        map.toggleState1 (pos.X, pos.Y)
        pos <- step pos state

    !count

let answer2 =
    let map = Map ()
    let mutable pos = { X = 0; Y = 0; Dir = -90 }
    let count = ref 0

    for i = 1 to 10000000 do
        let state = map.getState (pos.X, pos.Y)
        if state = Weakened then incr count
        map.toggleState2 (pos.X, pos.Y)
        pos <- step pos state

    !count