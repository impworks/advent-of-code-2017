open System
open System.IO
open System.Collections.Generic

let source =
    let raw = File.ReadAllText "16.txt"
    raw.Split ','

let (|Spin|_|) (input:string) =
    if input.[0] = 's'
    then
        let offset = int (input.Substring 1)
        Some (fun (state:string) -> 
            let pos = state.Length - offset
            state.Substring(pos) + state.Substring(0, pos)
        )
    else None

let (|Exchange|_|) (input:string) =
    if input.[0] = 'x'
    then
        let parts = input.Substring(1).Split('/')
        let p1 = int parts.[0]
        let p2 = int parts.[1]
        Some (fun (state:string) ->
            let chars = state.ToCharArray ()
            let tmp = chars.[p1]
            chars.[p1] <- chars.[p2]
            chars.[p2] <- tmp
            String chars
        )
    else None

let (|Partner|_|) (input:string) =
    if input.[0] = 'p'
    then
        let c1 = input.[1]
        let c2 = input.[3]
        Some (fun (state:string) -> state.Replace(c1, '#').Replace(c2, c1).Replace('#', c2))
    else None

let initial =
    let chars = { 'a' .. 'p' } |> Seq.toArray 
    String chars

let answer startState =
    let folder state elem =
        let mapper =
            match elem with
            | Spin m -> m
            | Exchange m -> m
            | Partner m -> m
            | x -> failwith (sprintf "Unknown pattern: %s" x)
        mapper state

    source
    |> Seq.fold folder startState

let memoize fn =
    let cache = Dictionary<string, string> ()
    fun input ->
        if not (cache.ContainsKey(input)) then
            cache.[input] <- fn input

        cache.[input]

let answer1 = answer initial

let answer2 =
    let memAnswer = memoize answer
    { 1 .. 1000000000 }
    |> Seq.fold (fun state _ -> memAnswer state) initial