open System.IO
open System
open System.Collections.Generic
open System.Linq

let source =
    let raw = File.ReadAllText "6.txt"
    let parts = raw.Split([| "\t" |], StringSplitOptions.RemoveEmptyEntries)    
    parts
    |> Seq.map int
    |> Seq.toArray

let getHash (state: int[]) =
    String.Join(";", state)

let clone (state: int[]) =
    state |> Seq.toArray

let redistribute (state: int[]) =
    let newState = clone state
    let sourceId =
        newState.Select(fun item index -> item, index)
                .OrderByDescending(fun v -> fst v)
                .ThenBy(fun v -> snd v)
                .Select(fun v -> snd v)
                .First ()

    let sourceValue = newState.[sourceId]
    newState.[sourceId] <- 0

    for i = 1 to sourceValue do
        let targetId = (sourceId + i) % newState.Length
        newState.[targetId] <- newState.[targetId] + 1

    newState

let answers =
    let mutable state = source
    let mutable stop = false
    let mutable loopSize = 0

    let lookup = Dictionary<string, int> ()

    while not stop do
        let hash = getHash state
        let timesSeen = if lookup.ContainsKey hash then lookup.[hash] else 0
        lookup.[hash] <- timesSeen + 1

        stop <- timesSeen = 2
        if timesSeen = 1 then loopSize <- loopSize + 1

        state <- redistribute state

    (lookup.Count, loopSize)