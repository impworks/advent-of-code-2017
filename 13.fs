open System
open System.IO

type Scanner =
    {
        Depth: int;
        Direction: int;
        Position: int;
    }

let initialState =
    let source =
        let parse (line: string) =
            let parts = line.Split([|": "|], StringSplitOptions.None)
            (int parts.[0], int parts.[1])

        File.ReadAllLines "13.txt"
        |> Array.map parse

    let find id =
        source
        |> Seq.tryFind (fun x -> fst x = id)
        |> Option.map (fun def -> { Depth = snd def; Direction = 1; Position = 0 })

    let layersCount =
        source
        |> Seq.map fst
        |> Seq.max

    { 0 .. layersCount }
    |> Seq.map find
    |> Seq.toArray

let answer1 =
    let step state =
        let stepScanner scopt =
            match scopt with
            | None -> None
            | Some s ->
                let changeDir = (s.Position = 0 && s.Direction = -1) || (s.Position = s.Depth - 1 && s.Direction = 1)
                let dir = s.Direction * (if changeDir then -1 else 1)
                let pos = s.Position + dir
                Some { s with Direction = dir; Position = pos }

        Array.map stepScanner state

    let getSeverity (state: Scanner option []) pos =
        match state.[pos] with
        | Some p when p.Position = 0 -> pos * p.Depth
        | _ -> 0

    let rec traverse state severity pos =
        if pos = initialState.Length then
            severity
        else
            let newSeverity = severity + (getSeverity state pos)
            let newState = step state
            let newPos = pos + 1
            traverse newState newSeverity newPos

    traverse initialState 0 0

let answer2 =
    let posAt depth time =
        let leg = depth - 1
        let ntime = time - 1
        if ntime = -1 then 0
        else
            let forward = (ntime / leg) % 2 = 0
            let legOffset = (ntime % leg)
            if forward then
                legOffset + 1
            else
                depth - legOffset - 2

    let isWalkSafe (state:Scanner option []) delay = 
        let isNoticed (state:Scanner option []) delay pos = 
            match state.[pos] with
            | None -> false
            | Some sc ->
                posAt (sc.Depth) (pos + delay) = 0

        { 0 .. initialState.Length - 1 }
        |> Seq.map (isNoticed state delay)
        |> Seq.forall not

    let rec findDelay delay =
        if isWalkSafe initialState delay
        then delay
        else findDelay (delay + 1)

    findDelay 0
