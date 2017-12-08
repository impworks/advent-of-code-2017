open System
open System.IO
open System.Collections.Generic

type Instruction =
    {
        Register: string;
        Direction: int;
        Value: int;

        ConditionRegister: string;
        Condition: string;
        ConditionValue: int;
    }

let parse (line: string) =
    let parts = line.Split([| " " |], StringSplitOptions.RemoveEmptyEntries)
    {
        Register = parts.[0];
        Direction = if parts.[1] = "inc" then 1 else -1;
        Value = int parts.[2];
        ConditionRegister = parts.[4];
        Condition = parts.[5];
        ConditionValue = int parts.[6]
    }

let answers =
    let mutable max = Int32.MinValue
    let registers = Dictionary<string, int> ()
    let getRegister name =
        if registers.ContainsKey name then registers.[name] else 0

    let checkCondition instr =
        let actual = getRegister instr.ConditionRegister
        let expected = instr.ConditionValue
        let op =
            match instr.Condition with
            | "==" -> (=)
            | "!=" -> (<>)
            | "<" -> (<)
            | "<=" -> (<=)
            | ">" -> (>)
            | ">=" -> (>=)
        op actual expected

    let instructions =
        File.ReadAllLines "8.txt"
        |> Array.map parse

    for instr in instructions do
        if checkCondition instr then
            let r = instr.Register
            let oldValue = getRegister r
            let newValue = oldValue + (instr.Direction * instr.Value)
            registers.[r] <- newValue
            max <- Math.Max(max, newValue)

    let maxLast =
        registers.Values
        |> Seq.sortDescending
        |> Seq.head

    (maxLast, max)