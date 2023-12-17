open System

let input = IO.File.ReadAllText "./input/day16.txt"
let w = input.IndexOf(Environment.NewLine)
let lw = w+Environment.NewLine.Length
let h = (input.Length + Environment.NewLine.Length)/lw

/// peek value at xy
let peek(x,y) = input[x+lw*y]
/// check whether point is in the map
let inMap (x,y) = x >= 0 && x < w && y >= 0 && y < h

//       N 0
// W 3        E 1
//       S 2
//

// A structure to easily work with directions
[<Struct>]
type Dir = Dir of int
    with
    override this.ToString() =
        match this with
        | Dir 0 -> "N"  
        | Dir 1 -> "E"
        | Dir 2 -> "S"
        | Dir 3 -> "W"
        | _ -> failwith "Invalid value"

fsi.AddPrinter<Dir>(string)
module Dir =
    // we use &&& to keep the last two bit, instead of using a modulo
    // because modulo can return negative values
    let turnRight (Dir d) = Dir( (d + 1) &&& 3 )
    let turnLeft (Dir d) = Dir( (d - 1) &&& 3 )

    // the direction is vertical if bit 0 is 0 (N/S)
    let isVertical (Dir d)  = (d &&& 1) = 0 
    // and is horizontal if bit 0 is 1 (E/W)
    let isHorizontal (Dir d)  = (d &&& 1) = 1 

let N = Dir 0
let E = Dir 1
let S = Dir 2
let W = Dir 3

// an active pattern to make it look like an union
let (|N|E|S|W|) (Dir n) = 
    match n with
    | 0 -> N
    | 1 -> E
    | 2 -> S
    | 3 -> W
    | _ -> failwith "Invalid dir"

// move 1 step from x,y in give direction 
let move (x,y) dir =
    match dir with
    | N -> x,y-1
    | E -> x+1,y
    | S -> x,y+1
    | W -> x-1, y

// this function change the direction on a backslash \
// In the two horizontal cases, the light is deviated to the right:
//       ^
//       |
//  -----\------
//       |
//       v
// in the vertical cases, it is deviated to the left.
let backslash dir  =
    if Dir.isHorizontal dir then
        Dir.turnRight dir 
    else
        Dir.turnLeft dir

// For slash / it's the contrary
// In the two horizontal cases, the light is deviated to the left:
//       ^
//       |
//  -----/------
//       |
//       v
// in the vertical cases, it is deviated to the right.
let slash dir  =
    if Dir.isHorizontal dir then
        Dir.turnLeft dir 
    else
        Dir.turnRight dir

// the output array is initialized to 0.
// when a ray of light pass trough a cell, a flag is set for the direction
// this flag is the used to avoid running infinitely in case of loop
// however the flag is distinct for each direction as beams can cross. 

let energize (output: int[]) (x,y) (Dir dir) = 
    let index = x+y*w 
    output[index] <- output[index] + (1 <<< dir)

let isEnergized (output: int[]) (x,y) (Dir dir) =
    let index = x+y*w
    (output[index] &&& (1<<<dir)) <> 0

// let the light flow in
// the stack is used to keep track of path to explore because of bifuractions
let rec run output stack =
    match stack with
    | [] -> () // done
    | (pos, dir) :: rest ->
        // take the next ray, and walk it as far as possible
        let newStack = runRay output pos dir rest
        run output newStack
        
and runRay output pos dir stack =
    // check we're not out of map
    if inMap pos then
        if isEnergized output pos dir then
            // the cell is already energize for this direction... 
            // this is a loop, we can stop here
            // stop and move to next item in the stack 
            stack
        else
            // mark the cell for current direction
            energize output pos dir
            match peek pos with
            | '/' -> 
                // this is a slash mirror, compute new direction and move
                let newDir = slash dir
                let newPos = move pos newDir
                runRay output newPos newDir stack
            | '\\' -> 
                // this is a backslash mirror, compute new direction and move
                let newDir = backslash dir
                let newPos = move pos newDir
                runRay output newPos newDir stack
            | '|' when Dir.isHorizontal dir ->
                // this is a vertical splitter, and the light is horizontal
                // compute new directions and positions for both directions
                let newDir1 = Dir.turnRight dir
                let newPos1 = move pos newDir1
                let newDir2 = Dir.turnLeft dir
                let newPos2 = move pos newDir2
                // continue directly to the right, but stack the path to the left 
                runRay output newPos1 newDir1 ((newPos2, newDir2 ):: stack)
            | '-' when Dir.isVertical dir ->
                // this is an horizontal splitter and the light is vertical
                // compute new directions and positions for both directions
                let newDir1 = Dir.turnRight dir
                let newPos1 = move pos newDir1
                let newDir2 = Dir.turnLeft dir
                let newPos2 = move pos newDir2
                // continue directly to the right, but stack the path to the left 
                runRay output newPos1 newDir1 ((newPos2, newDir2 ):: stack)
            | _ -> 
                // otherwise just continue in the same direction
                runRay output (move pos dir) dir stack
    else
        stack
    
// compute the power
let power pos dir =
    let output = Array.zeroCreate<int>(w*h)
    run output  [pos, dir]
    // the result is the count of lines that have been energized
    output |> Array.sumBy (fun x -> if x <> 0 then 1 else 0)

// Part 1
power (0,0) E


// Part 2
// This is quite easy, we compute the power for each point of the 4 borders
// and take the max value
[ for x in 0..w-1 do
    power (x,0) S
    power (x,h-1) N
  for y in 0..h-1 do
    power (0,y) E
    power (w-1,y) W
] |> List.max