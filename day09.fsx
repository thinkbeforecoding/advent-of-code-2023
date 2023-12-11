open System

let lines = IO.File.ReadAllLines "./input/day09.txt"
let series = 
    lines
    |> Array.map (fun l -> l.Split(' ') |> Array.map int)

let rec predict (values: int[]) = 
    let derived = values |> Seq.pairwise |> Seq.map (fun (x, y) -> y-x) |> Seq.toArray
    if derived |> Seq.forall ((=) 0) then
        values[values.Length-1]
    else
        values[values.Length-1] + predict derived


series
|> Array.map predict
|> Array.sum

let rec predict2 (values: int[]) = 
    let derived = values |> Seq.pairwise |> Seq.map (fun (x, y) -> y-x) |> Seq.toArray
    if derived |> Seq.forall ((=) 0) then
        values[0]
    else
        values[0] - predict2 derived

series
|> Array.map predict2
|> Array.sum
