open System
open System.Collections.Generic

// this start to be classinc, using the string directly, is actually easy
// and works well
let input = IO.File.ReadAllText( "./input/day14.txt")
let w = input.IndexOf(Environment.NewLine)
let lw = w+Environment.NewLine.Length
let h = (input.Length+Environment.NewLine.Length)/lw

// peek a char in the map
let peek (map: char[]) (x,y) = map[x+lw*y]
// change a char in the map
let poke (map: char[]) c (x,y) = map[x+lw*y] <- c

/// Part 1

// we maintain, for each column the minimum row where a rock can move (initially 0)
let limit = Array.zeroCreate<int> w

// not very FP, but who cares... we mutate the map 😁
let map = input.ToCharArray()
// we compute the score on the fly 😱
let mutable score = 0
// run top down 
for y in 0 .. h-1 do
    for x in 0 .. w-1 do
        match peek map (x,y) with
        | '#' -> limit[x] <- y+1 // this rock will block the column at y+1
        | 'O' -> // this is a rollink rock
            let newy = limit[x] // it can roll down to the computed limit in the column
            score <- score + h-newy // add score for this rock
            limit[x] <- newy+1      // the rock blocks next rocks at newy+1
        | _ -> ()                   // do nothing on '.'
score


// Part 2
// we use the same strategy but with 4 possible direction
// It must be possible to be smarter and write it once 🥱
// or to rotate the map 

/// move it North
let moveN (map: char[]) =
    let limit = Array.zeroCreate<int> w
    for y in 0 .. h-1 do
        for x in 0 .. w-1 do
            match peek map (x,y) with
            | '#' -> limit[x] <- y+1
            | 'O' -> 
                let newy = limit[x] 
                poke map '.' (x,y)
                poke map 'O' (x,newy)
                limit[x] <- newy+1
            | _ -> ()
    map


/// move it South
let moveS (map: char[]) =
    let limit = Array.zeroCreate<int> w
    for y' in 0 .. h-1 do
        let y = h-1-y'
        for x in 0 .. w-1 do
            match peek map (x,y) with
            | '#' -> limit[x] <- y'+1
            | 'O' -> 
                let newy' = limit[x] 
                let newy = h-1-newy'
                poke map '.' (x,y)
                poke map 'O' (x,newy)
                limit[x] <- newy'+1
            | _ -> ()
    map

// Go West
let moveW (map: char[]) =
    let limit = Array.zeroCreate<int> h
    for x in 0 .. w-1 do
        for y in 0 .. h-1 do
            match peek map (x,y) with
            | '#' -> limit[y] <- x+1
            | 'O' -> 
                let newx = limit[y] 
                poke map '.' (x,y)
                poke map 'O' (newx,y)
                limit[y] <- newx+1
            | _ -> ()
    map

// move East
let moveE (map: char[]) =
    let limit = Array.zeroCreate<int> h
    for x' in 0 .. w-1 do
        let x = w-1-x'
        for y in 0 .. h-1 do
            match peek map (x,y) with
            | '#' -> limit[y] <- x'+1
            | 'O' -> 
                let newx' = limit[y] 
                let newx = w-1-newx'
                poke map '.' (x,y)
                poke map 'O' (newx,y)
                limit[y] <- newx'+1
            | _ -> ()
    map

/// a full cycle in each direction
let cycle map =
    map |> moveN |> moveW |> moveS |> moveE


/// running 1 billion cycles is far too long...
/// but the repetition make it expected to contain a cycle
/// find it

// this dictionary keep track of all seen maps at which step
let past = Dictionary<string,int>()
let rec run (map: char[]) n =
    let m = String map
    if past.ContainsKey(m) then
        // we found the same map.
        // report previous and current occurence
        m,past[m], n
    else
        // this is a new one, add it the dictionary
        past.Add(m,n)
        // and prepare the next cycle 
        let newMap = cycle map
        run newMap (n+1)

// It stops quite fast on a situation already seen
let m, repeatStart, repeatEnd = run  (input.ToCharArray()) 0
// the length of the loop
let repeatLength = repeatEnd - repeatStart

// this check that there is no off-by-one error, by doing the actual
// number of cycles and checking they end up in the same state
do
    let map = input.ToCharArray()
    for _ in 1 .. repeatStart do
        cycle map |> ignore
    let m1 = String map
    for _ in 1 .. repeatLength do
        cycle map |> ignore
    let m2 = String map
    printfn "%b" (m1 = m2)

// We need to do 1 billion cycles
let totalCycles = 1_000_000_000

// there is reapeatStart cycles before it loops, but then it will periodically
// return to same state, remainingCycles is the number of cycles remaning after
// the last full repetition
let remainingCycles = (totalCycles - repeatStart) % repeatLength

// now we can compute the end map, skiping all the repetitions (and it is far faster 😁)
let endMap =
    let map = input.ToCharArray()
    for _ in 1 .. repeatStart do
        cycle map |> ignore
    for _ in 1 .. remainingCycles do
        cycle map |> ignore
    map

// Compute the score for this map
score <- 0
for y in 0 .. h-1 do
    for x in 0 .. w-1 do
        match peek endMap (x,y) with
        | '#' -> ()
        | 'O' -> 
            score <- score + h-y
        | _ -> ()
score