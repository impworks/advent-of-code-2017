open System
open System.IO
open System.Collections.Generic

type Node =
    {
        Name: string
        Weight: int
        ChildrenNames: string[]

        mutable Parent: Option<Node>
        mutable Children: Option<Node[]>               
    }

let split (str:string) (delim:string) =
    str.Split([| delim |], StringSplitOptions.RemoveEmptyEntries)

let parseNode line =
    let parts = split line " -> "
    let childNames = if parts.Length > 1 then split parts.[1] ", " else [| |]
    let leftSide = split parts.[0] " "
    {
        Name = leftSide.[0];
        Weight = int (leftSide.[1].Trim ([| '('; ')'|]));
        ChildrenNames = childNames;
        Parent = None;
        Children = None;
    }

let source =
    let nodes =
        File.ReadAllLines "7.txt"
        |> Seq.map parseNode
        |> Seq.map (fun v -> (v.Name, v))
        |> dict

    for elem in nodes.Values do
        let children =
            elem.ChildrenNames
            |> Array.map (fun name -> nodes.[name])
        elem.Children <- Some children
        for child in children do
            child.Parent <- Some elem

    nodes

let equal (node1: Option<Node>) (node2: Option<Node>) =
    match (node1, node2) with
    | (Some n1, Some n2) -> n1.Name = n2.Name
    | _ -> false

let answer1 =
    let root =
        source.Values
        |> Seq.find (fun x -> x.Parent = None)
    root.Name

let answer2 =
    let totalWeights = Dictionary<string, int> ()
    let rec getTotalWeight (node: Node) =
        if not (totalWeights.ContainsKey node.Name) then
            let subWeights = Seq.sumBy getTotalWeight node.Children.Value
            totalWeights.[node.Name] <- node.Weight + subWeights
        totalWeights.[node.Name]

    let invalidNode =
        let isBalanced (node: Node) =
            match node.Children with
            | None -> true
            | Some children ->
                let weightGroups =
                    children
                    |> Seq.distinctBy getTotalWeight
                    |> Seq.length
                weightGroups = 1

        let isInvalid (node: Node) =
            match node.Parent with
            | None -> false
            | Some parent ->
                let neighbourWeights =
                    parent.Children.Value
                    |> Array.map getTotalWeight
                let selfWeight = getTotalWeight node
                let equalNeighbours =
                    neighbourWeights
                    |> Seq.where (fun x -> x = selfWeight)
                    |> Seq.length  
                let isSelfInvalid = equalNeighbours = 1

                isSelfInvalid && (isBalanced node)

        Seq.find isInvalid source.Values

    let validNeighbour =
        source.Values
        |> Seq.find (fun x -> (equal x.Parent invalidNode.Parent) && x.Name <> invalidNode.Name)

    let validWeight = getTotalWeight validNeighbour
    let childrenWeight = (getTotalWeight invalidNode) - invalidNode.Weight
    validWeight - childrenWeight