open System

let input = IO.File.ReadAllText("./input/day13.txt")
let patterns = input.Replace("\r","").Split("\n\n")

// Part 1
// this one was not very difficult

// Parse the patterns keeping plain strings 
type Pattern =
    { Map: string
      Width: int
      LineWidth: int
      Height: int}

let parse (p: string ) =
    let w = p.IndexOf('\n')
    let lw = w + 1
    let h = (p.Length+1)/lw
    { Map = p
      Width = w
      LineWidth = lw
      Height = h
    }

// for vertical mirror, we transpose the map
// checking horizontal is faster and easier
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

// check whether to lines are equal
let linesEqual p a b =
    let x = p.Map.AsSpan().Slice(a*p.LineWidth, p.Width)
    let y = p.Map.AsSpan().Slice(b*p.LineWidth,p.Width)
    x.SequenceEqual(y)

// check that all lines between a and b are reflected horizonataly
let rec checkMirror p a b =
    if linesEqual p a b then
        if a+1 = b then
            b
        else
            checkMirror p (a+1) (b-1)
    else
        0

// but before this we don't even know where are similar lines
// try to find equal lines
let rec findMirror p a b =
    if a >= p.Height-1 then
        // we tested all possibilites, there is no horizontal mirror here
        0
    elif b >= p.Height then
        // we tried all b's, try next 'a'
        findMirror p (a+1) (a+2)
    else
        // check whether lines are equal
        if linesEqual p a b then
            // lines are equal, the mirror could be just in the middle
            let middle = (a+b)/2+1 // the +1 is to round up
            let above = middle      // the number of lines above the potential mirror
            let below = p.Height-middle // the number of lines below the potential mirror
            let maxh = min above below  // this would be the max number of lines on both sides
            let top = middle - maxh     // this is the top row to check
            let bottom = middle + maxh - 1  // this is the bottom row to check

            // check that all lines between match
            match checkMirror p top bottom with 
            | 0 -> findMirror p a (b+1) // nope, try again
            | n -> n    // yes, we found the mirror
        else
            // not equal try next b
            findMirror p a (b+1)

// Compute the score for a pattern
let score p =
    // try to find an horizontal mirror
    match findMirror p 0 1 with
    | 0 -> 
        // not found, transpose and look for mirror
        findMirror (transpose p) 0 1
    | n -> 100*n // found an horizontal mirror (score * 100)

patterns |> Array.sumBy (parse >> score)


// Part 2
//

// this function look for a smudge, returns its position of -1
// result is initialized to -1
let rec findSmudge' (a: char ReadOnlySpan) (b: char ReadOnlySpan) i result =
    if i < a.Length && i < b.Length then
        // not done yet, compare chars
        if a[i] = b[i] then
            // chars are the same, compare next
            findSmudge' a b (i+1) result
        elif result = -1 then
            // chars were different, and it's the first time
            // we may have found the smudge, continue to check
            // there is no other difference
            findSmudge' a b (i+1) i
        else
            // oh, this is the second difference, so it was not the smudge
            -1
    else
        // we reached the end, result if the position of the smudge if we found only one
        // or -1 if both input were equal
        result

// return smudgePos on lines a and b of pattern p
let smudgePos p  a b =
    let span = p.Map.AsSpan()
    let aspan = span.Slice(a*p.LineWidth, p.Width)
    let bspan = span.Slice(b*p.LineWidth, p.Width)
    findSmudge' aspan bspan 0 -1

// modify the map to clean the smudge
let applySmudge p x y =
    let a = Array.zeroCreate p.Map.Length
    p.Map.AsSpan().CopyTo(a.AsSpan())
    let index = y * p.LineWidth + x
    if a[index] = '#' then
        a[index] <- '.'
    else
        a[index] <- '#'
    { p with
        Map = String (Span.op_Implicit a) }

// look for the mirror including the smudge
// this is similar to findMirror, with a few commented differences
let rec findMirrorSmudge p a b =
    if a >= p.Height-1 then
        0
    elif b >= p.Height then
        findMirrorSmudge p (a+1) (a+2)
    else
        // compare lines finding potential smudge
        match smudgePos p a b with
        | -1 -> findMirrorSmudge p a (b+1)
        | n ->
            // those line are identical up to the smudge
            // compute the position of the mirror
            let middle = (a+b)/2+1
            let above = middle
            let below = p.Height-middle
            let maxh = min above below
            let top = middle - maxh
            let bottom = middle + maxh - 1

            // clean the smudge and check the mirror
            let p2 = applySmudge p n a
            match checkMirror p2 top bottom with
            | 0 -> findMirrorSmudge p a (b+1)
            | n -> n
                
// This is similar to par1, but including smudge
let scoreSmudge p =
    match findMirrorSmudge p 0 1 with
    | 0 -> 
        findMirrorSmudge (transpose p) 0 1
    | n -> 100*n

patterns |> Seq.sumBy (parse >> scoreSmudge)

