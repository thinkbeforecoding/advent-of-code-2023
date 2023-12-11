open System

let input = IO.File.ReadAllText "./input/day11.txt"

// once again we use a flat string as a map
// width of the map
let width = input.IndexOf(Environment.NewLine)
// total length of a line including newline
let lineWidth = width + Environment.NewLine.Length
let height = (input.Length + Environment.NewLine.Length) / lineWidth

/// get coordinates from index in string
let pos index =
    index%lineWidth, index / lineWidth

/// peek char at given coordinates
let peek (x,y) =
    input[x+y*lineWidth]

/// the empty rows
let emptyRows = 
    [ for r in 0 .. height-1 do
        let empty = [ for c in 0 .. width-1 -> peek (c,r) ] |> List.forall ((=) '.')
        if empty then
            r
     ]

/// the empty collumns
let emptyCols = 
    [ for c in 0 .. width-1 do
        let empty = [ for r in 0 .. height-1 -> peek (c,r) ] |> List.forall ((=) '.')
        if empty then
            c
     ]

/// the position of all stars
let stars = 
    [ for i in 0 .. input.Length-1 do
        if peek(pos i) = '#' then
            pos i
     ]

let countBeetween a b l =
    let vmin = min a b
    let vmax = max a b
    l |> List.sumBy (fun c -> if c > vmin && c < vmax then 1 else 0 )

/// count the empty columns between x1 and x2
let countEmptyCols x1 x2 = countBeetween x1 x2 emptyCols

/// count the empty rows between y1 and y1
let countEmptyRows y1 y2 = countBeetween y1 y2 emptyRows

// the distance is actually a manhattan distance, but empty columns and rows
// must be adapted for epansion
let dist (x1,y1) (x2,y2) expansion  =
    let extraCols = (int64 (countEmptyCols x1 x2)) * (expansion-1L)
    let extraRows = (int64 (countEmptyRows y1 y2)) * (expansion-1L)
    int64 (abs (x2-x1)) + extraCols + int64(abs(y2-y1)) + extraRows

// distance is computed on all pairs, then divided by 2.

// Part 1
(List.allPairs stars stars |> List.map (fun (s1, s2) -> dist s1 s2 2L) |> List.sum) / 2L
// Part 2
(List.allPairs stars stars |> List.map (fun (s1, s2) -> dist s1 s2 1_000_000) |> List.sum) / 2L



