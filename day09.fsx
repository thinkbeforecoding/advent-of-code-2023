open System

let lines = IO.File.ReadAllLines "./input/day09.txt"
let series = 
    lines
    |> Array.map (fun l -> l.Split(' ') |> Array.map int)

/// this one was quite easy.
/// it uses a recursive function 
let rec predict sum (values: int[]) = 
    // compute the different of consecutive numbers
    let derived = values |> Seq.pairwise |> Seq.map (fun (x, y) -> y-x) |> Seq.toArray
    // and we must sum last numbers
    if derived |> Seq.forall ((=) 0) then
        sum + values[values.Length-1]
    else
        // sum this level last numbers with previous
        predict (sum + values[values.Length-1])  derived


series
|> Array.map (predict 0)
|> Array.sum

// this one is easier to write without tail recursion
let rec predict2 (values: int[]) = 
    let derived = values |> Seq.pairwise |> Seq.map (fun (x, y) -> y-x) |> Seq.toArray
    if derived |> Seq.forall ((=) 0) then
        values[0]
    else
        values[0] - predict2 derived

series
|> Array.map predict2
|> Array.sum
