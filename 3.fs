open System
open System.Collections.Generic

type Direction =
    | Up
    | Down
    | Left
    | Right

type Pos = { x: int; y: int }
type Leg = { dir: Direction; length: int }

let getNextLeg leg =
    let nextDir = match leg.dir with
    | Up -> Left
    | Left -> Down
    | Down -> Right
    | Right -> Up

    let nextLength = match leg.dir with
    | Up | Down -> leg.length + 1
    | _ -> leg.length

    { dir = nextDir; length = nextLength }

let getStep dir =
    match dir with
    | Up -> { x = 0; y = -1 }
    | Left -> { x = -1; y = 0 }
    | Down -> { x = 0; y = 1 }
    | Right -> { x = 1; y = 0 }

let move pos step =
    { x = pos.x + step.x; y = pos.y + step.y }

let getDistance pathLen =
    let mutable pos = { x = 0; y = 0 }
    let mutable leg = { dir = Right; length = 1 }
    let mutable step = getStep leg.dir
    let mutable legIdx = 0

    for i = 0 to pathLen - 2 do
        pos <- move pos step
        legIdx <- legIdx + 1
        if legIdx = leg.length then
            leg <- getNextLeg leg
            step <- getStep leg.dir
            legIdx <- 0

    (Math.Abs pos.x) + (Math.Abs pos.y)

let neighbourSteps =
    seq {
        for x in -1 .. 1 do 
            for y in -1 ..1 do
                yield { x = x; y = y }
    }
    |> Seq.toArray

let getNextValue (pos:Pos) (map:Dictionary<Pos, int>) =
    neighbourSteps
    |> Seq.map (move pos)
    |> Seq.sumBy (fun pt -> if (map.ContainsKey pt) then map.[pt] else 0)

let firstLarger num =
    let map = Dictionary<_, _> ()
    let mutable pos = { x = 0; y = 0 }
    let mutable leg = { dir = Right; length = 1 }
    let mutable step = getStep leg.dir
    let mutable legIdx = 0
    let mutable value = 1

    while value <= num do
        map.Add (pos, value)

        pos <- move pos step
        value <- getNextValue pos map
        legIdx <- legIdx + 1

        if legIdx = leg.length then
            leg <- getNextLeg leg
            step <- getStep leg.dir
            legIdx <- 0

    value

let input = 265149
let answer1 = getDistance input
let answer2 = firstLarger input