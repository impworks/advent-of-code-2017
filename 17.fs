open System

type Node =
    {
        Val: int;
        mutable Next: Node option;
    }

let input = 371

let answer1 =
    let count = 2017
    let first = { Val = 0; Next = None }
    first.Next <- Some first

    let mutable curr = first

    for i = 1 to count do
        for k = 1 to input do
            curr <- curr.Next.Value

        let newItem = { Val = i; Next = curr.Next }
        curr.Next <- Some newItem
        curr <- newItem

        if i % 10000 = 0 then
            Console.WriteLine ("{0} done", i)

    curr.Next.Value.Val

let answer2 =
    let count = 50000000
    let mutable afterZero = 1
    let mutable currPos = 0

    for i = 1 to count do
        currPos <- ((currPos + input) % i) + 1
        if currPos = 1 then afterZero <- i

    afterZero