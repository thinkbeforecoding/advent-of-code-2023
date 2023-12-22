open System
open System.Text.RegularExpressions

let input = IO.File.ReadAllLines "./input/day22.txt"
// vector structure for bricks ends
[<Struct>]
type Vec = { X: int; Y: int; Z: int}

// parse a brick
let regex = Regex(@"^(\d+),(\d+),(\d+)~(\d+),(\d+),(\d+)$")


// parse a line to a brick 
let parseBrick (l: string) =
    let m = regex.Match(l)
    let v (n: int) = int m.Groups[n].Value
    { X= v 1; Y = v 2;Z =  v 3}, { X = v 4; Y = v 5; Z = v 6 }

// fix a brick to always be bottom -> up, left -> right, front -> back
let fixOrient (s,e) =
    { X = min s.X e.X; Y = min s.Y e.Y; Z = min s.Z e.Z}, { X = max s.X e.X; Y = max s.Y e.Y; Z = max s.Z e.Z }

// parse all bricks and sort them bottom up first, so that
// we process them in order level by level
let bricks = input |> Seq.map (parseBrick >> fixOrient) |> Seq.sortBy (fst >> _.Z) |> Seq.toList

// we will make a map of current top z for each x,y.. compute width and length of the map
let w = (bricks |> Seq.map (snd >> _.X) |> Seq.max) + 1
let l = (bricks |> Seq.map (snd >> _.Y) |> Seq.max) + 1


// read an write in the map
let peek (topMap: (int*int)[]) (x,y) = topMap[x+w*y]
let poke (topMap: (int*int)[]) (x,y) z i = topMap[x+w*y] <- (z,i)

// returns all the x,y coordinates of cubes of a brick
let coords (b,t) = [ for x in b.X .. t.X do 
                        for y in b.Y .. t.Y do
                            x,y ]

// we manage two dependency maps
// one that for each bricks indecates which bricks it supports
// one that for each bricks indicates which bricks it relies on
// this adds an entry for a brick
let addDependency brick otherbrick deps =
    match Map.tryFind brick deps with
    | None -> Map.add brick (Set.singleton otherbrick) deps
    | Some s -> Map.add brick (Set.add otherbrick s) deps


let rec pile bricks (topMap: (int*int)[]) supports relyOn =
    match bricks with
    | [] -> supports, relyOn // at the end of the list, returns dependencies
    | (index,(bottom,top)) :: tail ->
        // peek all current heights in the map under the brick
        // it will stop on the highest brick + 1
        let newZ = 
            coords(bottom,top) |> List.map (fun p ->
                let mapz,_  = peek topMap p
                mapz+1) |> List.max

        // find all bricks in the map that are just under the new z
        let bricksUnder =
            coords(bottom,top) |> List.choose(fun p -> 
                let mapz, b  = peek topMap p

                if mapz = newZ-1 && b >= 0 then Some b else None)
            |> List.distinct

        // update the map with the top of the brick at new z, and indicates its index
        for p in coords(bottom,top) do
            poke topMap p (newZ + top.Z - bottom.Z) index

        // all brick under support this brick
        let supports = bricksUnder |> List.fold (fun deps u -> addDependency u index deps) supports
        // this brick relies on all bricks under
        let relyOn = bricksUnder |> List.fold (fun deps u -> addDependency index u deps) relyOn

        // move to next brick
        pile tail topMap supports relyOn


let supports, relyOn = pile (List.indexed bricks)  (Array.init<int*int> (w*l) (fun _ -> 0,-1)) Map.empty Map.empty

[ for i in 0 .. List.length bricks - 1  do
    // go through all bricks
        match Map.tryFind i supports with
        | None -> 1 // this brick supports no other brick
        | Some bricksOver ->
            // there are some bricks over this one
            // check if one of those bricks over rely only on this one
            if bricksOver |> Seq.exists (fun over ->
                (Map.find over relyOn).Count = 1 // the brick rely only on this one
            ) then
                // yes, it cannot be removed
                0
            else 
                // no, it can be removed
                1
] |> List.sum


// part 2

// for part 2, we already have the dependencies (support/relyOn)
// we just have to go through chain reaction

let desintegrate n =
    // in the loop, we maintain a set of falling bricks (due to desintegration of bricks bellow)
    // as it is as set, there is no duplicate, and we can take the smallest value (bricks are numbered bottom up)
    // desintegrated is a set of bricks that have exploded already
    let rec loop falling desintegrated = 
        if Set.isEmpty falling then
            // no more bricks to fall
            Set.count desintegrated - 1 // we don't count the 1st brick
        else
            // take the falling brick and remove it from the falling list
            let n' = Set.minElement falling
            let tail = Set.remove n' falling

            // find the bricks it rely on
            match Map.tryFind n' relyOn with
            | None -> failwith $"The brick should rely on something {n} {n'}"
            | Some under -> 
                // we get a list of brick under this one, and we exclude all
                // desintegrated bricks. Is it empty ? then this brick false
                if Set.isEmpty (under - desintegrated) then
                    // all bricks under are desintegrated, it is falling
                    // find all bricks it supports and add them to the falling set
                    // also add this brick to desintegrated ones 
                    let newFalling = Map.tryFind n' supports |> Option.defaultValue Set.empty
                    loop (tail + newFalling) (Set.add n' desintegrated)
                else
                    // there are remaining bricks under, it doen't fall
                    loop tail desintegrated
    
    // check which bricks it supports
    match Map.tryFind n supports with
    | None ->  0 // the brick supports no other brick, nothing falls, move on
    | Some over -> 
        // all bricks over should fall, current brick has been desintegrated
        loop over (Set.singleton n)

// desintegrate bricks 1 by on, and sum the results
[ for n in 0 .. bricks.Length - 1 -> desintegrate n] |> List.sum
