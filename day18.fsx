open System

let input = IO.File.ReadAllLines "./input/day18.txt"

let parse (line: string) =
    match line.Split(' ') with
    | [| dir; len; color |] -> dir[0], int len, color[2..color.Length-2]
    | _ -> failwith "Invalid line"

// parse direction and length
let moves = input |> Seq.map parse |> Seq.toList

// simple vector operations
// add two vectors
let inline (++) (x1,y1) (x2,y2) = x1+x2, y1+y2
// scale a vector by n
let inline ( ** ) (x1,y1) n = x1*n, y1*n

// get a unit vector from direction
let vec = function
    | 'U' -> (0,-1)
    | 'D' -> (0,1)
    | 'L' -> (-1, 0)
    | 'R' -> (1, 0)
    | _ -> failwith "Invalid direction"

// For Part1, numbers a reasonable small,
// the problem can be solved by storing data in a table

// compute positions of all points on the border
let rec positions p moves =
    seq { 
        match moves with
        | [] -> ()      // no more move... done!
        | (dir, len, _) :: tail ->
            // get a unit vector for direction
            let v = vec dir 
            // returns all points on the line
            for i in 0 .. len-1 do
                yield p ++ v**i
            // next move
            yield! positions (p ++ v ** len) tail
    }

// find the top left corner of the bounding box
let minx = positions (0,0) moves |> Seq.map fst |> Seq.min
let maxx = positions (0,0) moves |> Seq.map fst |> Seq.max

// x offset to avoid negative x coordinates 
let x0 = -minx
// width of the bounding box (the last item must be included => +1)
let w = maxx-minx + 1

// same thing for y coordinates
let miny = positions (0,0) moves |> Seq.map snd |> Seq.min
let maxy = positions (0,0) moves |> Seq.map snd |> Seq.max

let y0 = -miny
let h = maxy-miny + 1

// prepare the output map
let map = Array.replicate  (w*h) '.'

// mark all position on the border with #
for x,y in positions(x0,y0) moves do
    map[x+y*w] <- '#' 

// print the map
let printMap() = map |> Array.chunkBySize w |> Array.map String |> String.concat Environment.NewLine |> printfn "%s"
printMap()

// fills a line, by checking when inside/outside
// The problem is that borders have to be counted, and
// some borders are inside out transitions, while others not...
// Some examples:
//     #                       #  #          #
// out # in   in ##### in   in #### in   in  ### out
//     #         #   #                         #
//
//  edge       local max      local min      step
// the pattern is that when checing the next # at the start and end of a series of # 
// (could be a single # for a vertical border), if the # are on the same side, the
// edge doesnt flip in/out (this is the case for local min, local max)
// If the # are on different side, it flips in/out (this is the case of edge and step)


let rec fill index remaining inside =
    if remaining > 0 then
        // not at the end of the line, peek current cell
        match map[index] with
        | '#' ->
            // we're at the start of a border
            // look above and below
            let above = if index-w >= 0 then map[index-w] else '.'
            let below = if index+w < map.Length then map[index+w] else '.'
            // at least one of them should also be on the border
            if not (above = '#' || below = '#') then failwith "There is a problem with the border" 

            if above = '#' && below = '#' then
                // this is a vertical edge
                // we can skip it, and flip in/out
                fill (index+1) (remaining-1) (not inside)
            else
                // this is a long horizontal edge 
                // the skip function will find the end of the edge,
                // keeping in memory wheter we found a # above or not. 
                skip (index+1) (remaining-1) inside above
        | _ ->
            // this is not a border, fill it if we're inside
            if inside then
                // we fill with X, because border detection relies on #,
                // X will not break it
                map[index] <- 'X'
            fill (index+1) (remaining-1) inside

and skip index remaining inside comeFromAbove =
    if remaining > 0 then
        // we're not done with this line
        match map[index] with
        | '#' ->
            // we're still on the border, continue
            skip (index+1) (remaining-1) inside comeFromAbove
        | _ ->
            // we just left the border
            // check on previous cell if the boder continues above or below
            let above = if index-w-1 >= 0 then map[index-w-1] else '.'
            let below = if index+w-1 < map.Length then map[index+w-1] else '.'
            // at least one of them should also be on the border
            if not (above = '#' || below = '#') then failwith "There is a problem with the border" 

            if comeFromAbove = above then
                // the next edges are on the same side (true=true or false=false)
                // continue filling without flipping in/out
                fill index remaining inside
            else
                // the next edges are on different sides (true<>false or false<>true)
                // continue filling, with flipped in/out
                fill index remaining (not inside)


// call the fill function on each line
for y in 0 .. h-1 do
    fill (y*w) w false

// print the result
printMap()

// count border and inside cells
map |> Array.sumBy (function '#' | 'X' -> 1 | _ -> 0)

// Part 2
// The solution in part 1 will not work, because the result map would take several Peta bytes...
// We need to work smarter

// but first, let's parse the input differently

// use the color part of the first parsing and
// extract direction and length (keep color just for fun)
let parse2 (_,_,color: string) =
    match color[color.Length-1] with
    | '0' -> 'R'
    | '1' -> 'D'
    | '2' -> 'L'
    | '3' -> 'U'
    | _ -> failwith "invalid dir"
    , Int32.Parse(color[0..color.Length-2], Globalization.NumberStyles.HexNumber), color


// make a list of the new moves
let moves2 = input |> Seq.map (parse >> parse2) |> Seq.toList

// fix a vector to point left to right or top down
let inline fixdir ((x1,y1), (x2,y2)) =
    (min x1 x2, min y1 y2),(max x1 x2, max y1 y2)

// this time we don't ouput all points, but all vectors
let rec getVectors p moves =
    seq { 
        match moves with
        | [] -> ()  // we're done
        | (dir, len, _) :: tail ->

        // p is the start of the vector
        // get the unit direction vector
        let v = vec dir 
        // the other end of the vector
        let next = p ++ v ** len
        // return the fixed (left to right or top down) vector
        yield fixdir (p, next)
        // continue from new end for next move
        yield! getVectors next tail
    }

// Compute all border vectors 
let vectors = getVectors (0,0) moves2 |> Seq.toList

// An active pattern that detects horizontal border
// It returns the y as well as start and end x
let (|Horizontal|_|) ((x1,y1),(x2,y2)) =
    if y1 = y2 then
        Some(y1, x1, x2)
    else
        None
// An active pattern that detects vertical border
// It returns the x as well as the start and end y
let (|Vertical|_|) ((x1,y1),(x2,y2)) =
    if x1 = x2 then
        Some(x1,y1,y2)
    else
        None

// get a list of all the scan line (y where there is at least one horizontal edge)
let scanlines =     
    vectors
    |> List.choose (function Horizontal(y,_,_) -> Some y | _ -> None) // get the y of horizontal edge
    |> List.distinct    // get rid of duplicate
    |> List.sort        // sort top down

// filter vertical edges
let verticals = 
    vectors 
    |> List.choose (function Vertical(x,yt,yb) -> Some(x,yt,yb) | _ -> None) 

// check if a vertical border is define between ys and ye
// and returns its x corrdinate if it's the case
let intersectLine ys ye (x, yt, yb) = 
    if ys >= yb || ye <= yt then     
        // no interection between this edge and the line range ys => ye
        None
    else
        // the border exists between the line range ys => ye
        Some x

// He is a schema to see how we will proceed
// The scan lines are markes with < 
// we will first compute quickly what is between the scan lines ys ye (and not on the scan lines themselves)
// For a scan line range, we find all vertical borders spaning this range, and sort them from left to right
// The number of vertical border should always be even, so we group them by two, and we get a rectange.
// the height is the size of the range (ye-ys+1)-2  = ye-ys-1
// (+1 because the ys line is included, but -2 because we exclude both scanlines)
// the width is xe-xs+1 (we include the final #)
// 
//
//     /=========\          <   out  /\  out
//     #xxxxxxxxx#
//     #xxxxxxxxx#
//  /--/iiiiiiiii|          <   out // in | out
//  #xxxxxxxxxxxx#   
//  |iiiiiii/=\ii\==\       <    out | in /\ in \\ out
//  #xxxxxx# #xxxxxx#
//  #xxxxxx# #xxxxxx#
//  \======/ |iiiiii|       <    out \/ out | in | out
//           \======/       <    out \/ out
//
// we proceed with scan lines separately. The corners of the scan
// lines have been draw using / and \ to show edge transitions
// vertical borders have been draw using |
// on scan lines:
//  /--\ or \--/       : no in/out switch 
//  /--/ or \--\ or |  : in/out switch    
// Using this we can count the cells on the border, and known when inside outside



// compute first the area between scan lines
let areaBetweenScanLines =
    // proceed on pairs of consecutive scanlines 
    [ for ys,ye in List.pairwise scanlines do
        assert (ys >= ye)

        // this is the height strictly between scanlines
        // scanlines will be counted separately
        let h = int64 ( ye - ys - 1)
        // find vertical edges that span the scanline range
        // sort them by x and group by 2
        let pairs =
            verticals
            |> List.choose (intersectLine ys ye)
            |> List.sort
            |> List.chunkBySize 2
        
        // compute the size of each rectangle and sum them
        pairs
        |> List.map (fun p ->
            match p with
            | [s;e] ->
                let area = int64(e-s+1)* h
                area
            | _ -> failwith "Not a pair 2" )
        |> List.sum
    ] |> List.sum // sum the result on all scanline ranges


// now we must compute the area on the scan line themselves

// make a list of all vectors with previous and next vector
// it will help detect horizontal borders and where the previous and next vertical vectors are going
let vectorsWithPreviousAndNext = 
    [vectors[vectors.Length-1]] @ vectors @ [ vectors[0]]
    |> List.windowed 3
    |> List.map (function [vl;v;vr] -> vl,v,vr | _ -> failwith "invalid") |> List.toArray

let transitions =
    // work on each scanline
    [ for sy in scanlines do 
        [ for vl,v,vr in vectorsWithPreviousAndNext do
            // filter everything happening on this scanline
            match v with
            | Horizontal(y, xs, xe) when y = sy ->
                // this is an horizontal border that is on this scanline
                // the previous and next vectors should be vertical
                match vl,vr with
                | Vertical(_,ytl,_), Vertical(_,ytr, _) ->
                    // take the orientation (going up, or going down)
                    // of previous and next vectors
                    let orientl = ytl < y
                    let orientr = ytr < y
                    // in/out should flip if pervious/next vectors go in oposit directions
                    let flip = orientl <> orientr
                    // return the x (xs) of the edge (to sort transitions of the scanline)
                    // the length (+1 because end is included)
                    // and whether it flips in/out or not
                    (xs, xe-xs+1, flip)
                | _ -> failwith "Unexpected sequence"
            | Vertical(x,yt,yb) when yt < sy && yb > sy ->
                // this is a vertical border that spans this scan line
                // it has a length of 1, and always flips in/out
                (x, 1, true)
            | _ -> ()
        ] |> List.sortBy (fun (x,_,_) -> x) // sort transitions on x
    ]

// now that we have for each scan line a list of transitions
// we can count the occupied cells on the scanline
// cx maintains current x, so that we know the last position when inside
let rec countScanLine cx inside (sum: int64) transitions  =
    match transitions with
    | [] -> sum // all transitions done, return the sum
    | (x, len, flip) :: tail ->

        // flip in/out if needed (to avoid doing it twice below)
        let newInside =
            if flip then
                not inside
            else
                inside
                
        // here we check on in/out before flipping
        if not inside then
            // current position will be at x+len (the end of the border)
            // with the new inside (flipped or not)
            // and we add only the length of the border (because we're out)
            countScanLine (x+len) newInside (sum + int64 len) tail
        else
            // here also position will be at x+len (the end of the border)
            // with the new indide
            // but we add x+len - cx (the new x minus previous x), because we
            // are inside and must count all cells since last position
            countScanLine (x+len) newInside (sum + int64 ( x + len - cx)) tail

// compute the area on the scan lines
let areaOnScanLines =
    transitions
    |> List.map (countScanLine Int32.MinValue false 0L )  // count on each scan line
    |> List.sum // sum everything

// add area between scan lines and on scan lines
let area = areaBetweenScanLines + areaOnScanLines

// Phhewww



