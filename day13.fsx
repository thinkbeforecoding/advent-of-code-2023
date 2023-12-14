open System

let input = IO.File.ReadAllText("./input/day13.txt")
let patterns = input.Replace("\r","").Split("\n\n")

//let pattern = patterns[0]

type Pattern =
    { Map: string
      Width: int
      LineWidth: int
      Height: int}

// let pattern =
//     """#.##..##.
// ..#.##.#.
// ##......#
// ##......#
// ..#.##.#.
// ..##..##.
// #.#.##.#."""
// let pattern =
//     """#...##..#
// #....#..#
// ..##..###
// #####.##.
// #####.##.
// ..##..###
// #....#..#"""
let parse (p: string ) =
    let w = p.IndexOf('\n')
    let lw = w + 1
    let h = (p.Length+1)/lw
    { Map = p
      Width = w
      LineWidth = lw
      Height = h
    }


let transpose p =
    let b = Text.StringBuilder()
    for c in 0 .. p.Width-1 do
        for r in 0 .. p.Height-1 do
            b.Append(p.Map[r*p.LineWidth+c]) |> ignore
        b.Append("\n") |> ignore
    { Map = b.ToString()
      Width = p.Height
      LineWidth = p.Height + 1
      Height = p.Width }


let linesEqual p a b =
    let x = p.Map.AsSpan().Slice(a*p.LineWidth, p.Width)
    let y = p.Map.AsSpan().Slice(b*p.LineWidth,p.Width)
    x.SequenceEqual(y)

let rec checkEqual p a b =
    if linesEqual p a b then
        if a+1 = b then
            b
        else
            checkEqual p (a+1) (b-1)
    else
        0

let rec findEquals p a b =
    if a >= p.Height-1 then
        0
    elif b >= p.Height then
        findEquals p (a+1) (a+2)
    else
        if linesEqual p a b then
            let middle = (a+b)/2+1
            let above = middle
            let below = p.Height-middle
            let maxh = min above below
            let top = middle - maxh
            let bottom = middle + maxh - 1

            match checkEqual p top bottom with
            | 0 -> findEquals p a (b+1)
            | n -> n
        else
            findEquals p a (b+1)

let score p =
    match findEquals p 0 1 with
    | 0 -> 
        findEquals (transpose p) 0 1
    | n -> 100*n

patterns |> Array.sumBy (
        fun p ->
            try
                p |> parse |> score
            with
            | ex ->
                printfn "%A\n" p
                0
            )

let rec findSpec' (a: char ReadOnlySpan) (b: char ReadOnlySpan) i result =
    if i < a.Length && i < b.Length then
        if a[i] = b[i] then
            findSpec' a b (i+1) result
        elif result = -1 then
            findSpec' a b (i+1) i
        else
            -1
    else
        result
let speckPos p  a b =
    let span = p.Map.AsSpan()
    let aspan = span.Slice(a*p.LineWidth, p.Width)
    let bspan = span.Slice(b*p.LineWidth, p.Width)
    findSpec' aspan bspan 0 -1

let applySpec p x y =
    let a = Array.zeroCreate p.Map.Length
    p.Map.AsSpan().CopyTo(a.AsSpan())
    let index = y * p.LineWidth + x
    if a[index] = '#' then
        a[index] <- '.'
    else
        a[index] <- '#'
    { p with
        Map = String (Span.op_Implicit a) }

let rec findSpec p a b =
    if a >= p.Height-1 then
        0
    elif b >= p.Height then
        findSpec p (a+1) (a+2)
    else
        match speckPos p a b with
        | -1 -> findSpec p a (b+1)
        | n ->

            let middle = (a+b)/2+1
            let above = middle
            let below = p.Height-middle
            let maxh = min above below
            let top = middle - maxh
            let bottom = middle + maxh - 1

            match checkEqual p top bottom with
            | 0 -> findEquals p a (b+1)
            | n -> n
        else
            findEquals p a (b+1)





