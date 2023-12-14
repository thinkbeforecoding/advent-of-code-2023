open System
let input = // IO.File.ReadAllText( "./input/day14.txt")
    """O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....""".Replace("\n",Environment.NewLine)

let w = input.IndexOf(Environment.NewLine)
let lw = w+Environment.NewLine.Length
let h = (input.Length+Environment.NewLine.Length)/lw

let peek (x,y) = input[x+lw*y]

let limit = Array.zeroCreate<int> w

let mutable score = 0 
for y in 0 .. h-1 do
    for x in 0 .. w-1 do
        match peek(x,y) with
        | '#' -> limit[x] <- y+1
        | 'O' -> 
            let newy = limit[x] 
            score <- score + h-newy
            limit[x] <- newy+1
        | _ -> ()
score

let rockBase = 
    let arrayChars = Array.zeroCreate input.Length
    input.AsSpan().CopyTo(arrayChars.AsSpan())
    for i in 0 .. arrayChars.Length-1 do
        match arrayChars[i] with
        | 'O' -> arrayChars[i]<- '.'
        | _ -> ()
    arrayChars


let move