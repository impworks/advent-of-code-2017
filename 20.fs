open System
open System.IO
open System.Collections.Generic
open System.Text.RegularExpressions

type Vector =
    {
        X: int;
        Y: int;
        Z: int;
    }

type Particle =
    {
        mutable Position: Vector;
        mutable Velocity: Vector;
        Acceleration: Vector;
    }

let len vec =
    let x = double vec.X
    let y = double vec.Y
    let z = double vec.Z
    Math.Sqrt (x * x + y * y + z * z)

let add vec1 vec2 =
    { X = vec1.X + vec2.X; Y = vec1.Y + vec2.Y; Z = vec1.Z + vec2.Z }

let getSource () =
    let regex = Regex("[apv<>= ]")
    let parse (line:string) =
        let bare = regex.Replace(line, "")
        let p = bare.Split ',' |> Array.map int
        {
            Position = { X = p.[0]; Y = p.[1]; Z = p.[2] };
            Velocity = { X = p.[3]; Y = p.[4]; Z = p.[5] };
            Acceleration = { X = p.[6]; Y = p.[7]; Z = p.[8] };
        }

    File.ReadAllLines "20.txt"
    |> Array.map parse

let answer1 =
    getSource ()
    |> Seq.mapi (fun i x -> (i, x))
    |> Seq.sortBy (fun x -> len (snd x).Acceleration)
    |> Seq.map fst
    |> Seq.head

let answer2 =
    let step particle =
        particle.Velocity <- add particle.Velocity particle.Acceleration
        particle.Position <- add particle.Position particle.Velocity

    let particles = new List<Particle> (getSource())

    // most likely enough
    for i = 1 to 2000 do
        for p in particles do step p

        let overlapping =
            particles
            |> Seq.groupBy (fun x -> x.Position)
            |> Seq.where (fun x -> Seq.length (snd x) > 1)
            |> Seq.collect snd

        for overlap in overlapping do
            particles.Remove overlap |> ignore

    particles.Count