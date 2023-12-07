open System
let lines = IO.File.ReadAllLines("./input/day06.txt")
let times = (lines[0].Split(':')[1]).Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Array.map int 
let distances = (lines[1].Split(':')[1]).Split(' ',StringSplitOptions.RemoveEmptyEntries) |> Array.map int 

let inline distance holdTime totalTime =
    (totalTime - holdTime) * holdTime

let maxDistance totalTime record =
    [ for t in 0 .. totalTime do
        let dist = distance t totalTime 
        if dist > record then
            dist
     ]
    |> List.length
Array.map2 maxDistance times distances |> Array.fold ( * ) 1


let times2 = lines[0].Replace(" ","").Split(':')[1] |> int64
let distances2 = lines[1].Replace(" ","").Split(':')[1] |> int64

let low  (totalTime: double) (record: double) =
    (totalTime - sqrt(totalTime * totalTime - 4. * record)) / 2.
let high  (totalTime: double) (record: double) =
    (totalTime + sqrt(totalTime * totalTime - 4. * record)) / 2.

let min (totalTime: int64) (record: int64) =
    let mutable m = int64 (low (double totalTime) (double record))
    while distance m totalTime <= record do
        m <- m+1L
    m

let max (totalTime: int64) (record: int64) =
    let mutable m = int64 (high (double totalTime) (double record))
    while distance m totalTime <= record do
        m <- m-1L
    m

let range totalTime record =
    max totalTime record - min totalTime record + 1L

range times2 distances2
