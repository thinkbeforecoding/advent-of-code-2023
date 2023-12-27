
open System

let input =  IO.File.ReadAllLines("./input/day24.txt")
//     """19, 13, 30 @ -2,  1, -2
// 18, 19, 22 @ -1, -1, -2
// 20, 25, 34 @ -2, -2, -4
// 12, 31, 28 @ -1, -2, -1
// 20, 19, 15 @  1, -5, -3""".Split '\n'

let parseVec (l:String) = 
    match l.Split(',', StringSplitOptions.TrimEntries) with
    | [| x;y;z |] -> decimal x, decimal y, decimal z
    | _ -> failwith "Invalid vector"
let parse (l: string) =
    match l.Split('@') with
    | [|pos; speed|] -> parseVec pos, parseVec speed
    | _ -> failwith "invalid line"



let hails = input |> Array.map parse |> Array.map (fun ((x,y,_),(vx,vy,_)) -> (x,y),(vx,vy))


let isVertical (_,(vx,_)) = vx = 0m
let x0 ((x,_),_) = x
let y0 ((_,y),_) = y

let vx (_,(vx,_)) = vx
let vy (_,(_, vy)) = vy

let slope (_,(vx,vy)) = vy / vx

let value x' ((x,y),(vx,vy)) = vy / vx * (x'-x) + y


let intersect h1 h2 =
    if isVertical h1 then
        if isVertical h2 then
            None
        else
            let x = x0 h1 
            let y = value x h2
            if sign (vy h1) = sign (y - y0 h1) && sign (vy h2) = sign (y - y0 h2) then
                Some (x, y)
            else
                None
    elif isVertical h2 then
        let x = x0 h2
        let y = value x h1
        if sign (vy h1) = sign (y - y0 h1) && sign (vy h2) = sign (y - y0 h2) then
            Some (x, y)
        else
            None
    else
        // y = a (x-x0) + y0
        // y = a x + y0 - a x0
        let a= slope h1
        let b = y0 h1 - a * x0 h1
        let a'= slope h2
        let b' = y0 h2 - a' * x0 h2

        if a = a' then
            None
        else
            // y = a x + b
            // y = a' x + b'
            // 0 = (a - a') x + (b - b')
            // x = (b'-b) / (a-a')  
            let x = (b' - b) / (a - a')
            let y = a * x + b
            
            if sign (vx h1) = sign (x - x0 h1) &&
                sign (vx h2) = sign (x - x0 h2) then
                Some (x,y)
            else
                None

// let area = (7m,7m),(27m,27m)

let area = (200000000000000m, 200000000000000m),(400000000000000m, 400000000000000m)

let inArea ((x0,y0),(x1,y1)) (x,y) = x >= x0 && x <= x1 && y >= y0 && y <= y1

[ for i in 0 .. hails.Length-1 do
    for j in i+1 .. hails.Length-1 do
        intersect hails[i] hails[j] ]
|> List.choose (function Some p when inArea area p -> Some p | _ -> None )
|> List.length




let hails3d = input |> Array.map parse



let inline ( *^ ) (u1,u2,u3) (v1,v2,v3)  = u2*v3-u3*v2, u3*v1 - u1*v3, u1*v2 - u2*v1 
let inline ( *. ) (u1,u2,u3) (v1,v2,v3)  = u1*v1 + u2 *v2 + u3 * v3


let c = 3
let equations = 
    hails3d |> Array.take c |> Array.mapi (fun i ((x,y,z),(vx,vy,vz)) ->
            [| $"(assert (= (* (- {x} x) (- vy {vy})) (* (- {y} y) (- vx {vx}))))"
               $"(assert (= (* (- {y} y) (- vz {vz})) (* (- {z} z) (- vy {vy}))))"
                 |]
    ) |> Array.concat |> String.concat "\n"

let defs= 
    [ "(declare-const x Int)"
      "(declare-const vx Int)"
      "(declare-const y Int)"
      "(declare-const vy Int)"
      "(declare-const z Int)"
      "(declare-const vz Int)"
    ] |> String.concat "\n"


printfn $"{defs}\n{equations}\n(check-sat)\n(get-model)"

140604613634294L + 224390889669946L + 206098283112689L