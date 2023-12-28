
open System

// day24 Part 2 was especialy not easy
// but part 1 was ok
let input =  IO.File.ReadAllLines("./input/day24.txt")

// parse the vectors
let parseVec (l:String) = 
    match l.Split(',', StringSplitOptions.TrimEntries) with
    | [| x;y;z |] -> decimal x, decimal y, decimal z
    | _ -> failwith "Invalid vector"
let parse (l: string) =
    match l.Split('@') with
    | [|pos; speed|] -> parseVec pos, parseVec speed
    | _ -> failwith "invalid line"


// take vectors and keep only x,y
let hails = input |> Array.map parse |> Array.map (fun ((x,y,_),(vx,vy,_)) -> (x,y),(vx,vy))

// a few functions to work with the 2D vectors
// check if vector is vertical
let isVertical (_,(vx,_)) = vx = 0m
// take x from position
let x0 ((x,_),_) = x
// take y from position
let y0 ((_,y),_) = y
// take x from speed
let vx (_,(vx,_)) = vx
// take y from speed
let vy (_,(_, vy)) = vy

// compute the slop of the line
let slope (_,(vx,vy)) = vy / vx

// compute the y value at x' on the line
let value x' ((x,y),(vx,vy)) = vy / vx * (x'-x) + y

// check whether lines defined by h1 and h2 intersect in the future
let intersect h1 h2 =
    if isVertical h1 then
        // h1 is vertical
        if isVertical h2 then
            // h2 is also vertical so they are parallel
            None
        else
            // h2 will meet h1, at h1 x (all points are at x since it's vertical)
            let x = x0 h1 
            let y = value x h2
            // compare signs of vectors from origin to interection to check if the 
            // intersection is in the future for both
            if sign (vy h1) = sign (y - y0 h1) && sign (vy h2) = sign (y - y0 h2) then
                Some (x, y)
            else
                None
    elif isVertical h2 then
        // h2 is vertical, same as above
        let x = x0 h2
        let y = value x h1
        if sign (vy h1) = sign (y - y0 h1) && sign (vy h2) = sign (y - y0 h2) then
            Some (x, y)
        else
            None
    else
        // y = a (x-x0) + y0
        // y = a x + y0 - a x0
        // compute both slopes
        let a= slope h1
        let b = y0 h1 - a * x0 h1
        let a'= slope h2
        let b' = y0 h2 - a' * x0 h2

        if a = a' then
            // lines are parralel
            None
        else
            // compute the coordinates of the interesection
            // y = a x + b
            // y = a' x + b'
            // 0 = (a - a') x + (b - b')
            // x = (b'-b) / (a-a')  
            let x = (b' - b) / (a - a')
            let y = a * x + b
            
            // check if the intersection is in the future for both
            if sign (vx h1) = sign (x - x0 h1) &&
                sign (vx h2) = sign (x - x0 h2) then
                Some (x,y)
            else
                None

let area = (200000000000000m, 200000000000000m),(400000000000000m, 400000000000000m)
// check if a point is in the area
let inArea ((x0,y0),(x1,y1)) (x,y) = x >= x0 && x <= x1 && y >= y0 && y <= y1

// compute all intersections and keep the ones in the area
[ for i in 0 .. hails.Length-1 do
    for j in i+1 .. hails.Length-1 do
        intersect hails[i] hails[j] ]
|> List.choose (function Some p when inArea area p -> Some p | _ -> None )
|> List.length


// part 2

// using the 3 first lines we can make a system of 9 equations with 6 variables:
// x,y,z vx,vy,vy
// it generates a program for a z3 solver like https://jfmc.github.io/z3-play/

// parse the lines, but in 3D
let hails3d = input |> Array.map parse

// for each line we can write
// (x0 - x) * (vy - vy0) = (y0 - y) * (vx - vx0)
// (y0 - y) * (vz - vz0) = (z0 - z) * (vy - vy0)
// as z3 asserts
let c = 3
let equations = 
    hails3d |> Array.take c |> Array.mapi (fun i ((x,y,z),(vx,vy,vz)) ->
            [| $"(assert (= (* (- {x} x) (- vy {vy})) (* (- {y} y) (- vx {vx}))))"
               $"(assert (= (* (- {y} y) (- vz {vz})) (* (- {z} z) (- vy {vy}))))"
                 |]
    ) |> Array.concat |> String.concat "\n"

// the variable declarations (they are all Int)
let defs= 
    [ "(declare-const x Int)"
      "(declare-const vx Int)"
      "(declare-const y Int)"
      "(declare-const vy Int)"
      "(declare-const z Int)"
      "(declare-const vz Int)"
    ] |> String.concat "\n"

// print the program
printfn $"{defs}\n{equations}\n(check-sat)\n(get-model)"

// the three position coordinates in the solution are:
140604613634294L + 224390889669946L + 206098283112689L