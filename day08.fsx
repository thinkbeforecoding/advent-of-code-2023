open System.IO

let lines = File.ReadAllLines "./input/day08.txt"
// let lines = """LR

// 11A = (11B, XXX)
// 11B = (XXX, 11Z)
// 11Z = (11B, XXX)
// 22A = (22B, XXX)
// 22B = (22C, 22C)
// 22C = (22Z, 22Z)
// 22Z = (22B, 22B)
// XXX = (XXX, XXX)""".Split('\n')
let moves = lines[0]
let map = lines |> Seq.skip 2 |> Seq.map (fun l -> l[0..2], (l[7..9], l[12..14])) |> Map.ofSeq

let move i = 
    match moves[i%moves.Length] with
    | 'L' -> fst
    | _ -> snd

let rec run i loc =
    match move i map[loc] with
    | "ZZZ" -> i+1
    | newLoc -> run (i+1) newLoc 
        
run 0 "AAA"

let rec run2 i locs =
    let newLocs = locs |> List.map (fun l -> move i map[l])
    if newLocs |> List.forall (fun l -> l[2] = 'Z') then
        i+1
    else
        run2 (i+1) newLocs

let rec cycleLen i loc =
    let newLoc = move i map[loc]
    if newLoc[2] = 'Z' then
        i+1
    else 
        cycleLen (i+1) newLoc 
        

run2 0 (map.Keys |> Seq.filter (fun l -> l[2]='A') |> Seq.toList)

let ghosts = map.Keys |> Seq.filter (fun l -> l[2]='A') |> Seq.toList

ghosts |> List.map (cycleLen 0)
