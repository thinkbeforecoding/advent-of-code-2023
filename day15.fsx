open System
open Microsoft.FSharp.Core.CompilerServices
open System.Runtime.Intrinsics

let input = IO.File.ReadAllText "./input/day15.txt"

// Part 1

// this is the hash function for a single char
// perf consideration: 
// 17 is 16 + 1, do a <<<4 and add
// %256 is bitwise AND 0xff
let hash (c: char) (h: int) =
    let t1 = h + int c
    (t1 + (t1 <<< 4)) &&& 0xff

// check that the hash of HASH is 52
0 |> hash 'H' |> hash 'A' |> hash 'S' |> hash 'H' 
    = 52

let hashSpan (s: char ReadOnlySpan) =
    let mutable h = 0
    for c in s do
        h <- hash c h
    h

let hashStr (s: string) = hashSpan (s.AsSpan())

hashStr "HASH" = 52

// sum the hash 
let hashAll (str: string) =
    str.Split(',')
    |> Array.sumBy hashStr

// check with sample
hashAll "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7" = 1320 

hashAll input

/// Part 2

// add a step with given label and value to the box
// it uses a ListCollector to build the new list on the go
let rec addToBox' label value list (result: ListCollector<_> byref) =
    match list with
    | [] -> 
        // this is the end of the list, we didn't find the label
        // add it to the end
        result.Add(label,value)
        result.Close()
    | (l,_) :: tail when l = label ->
        // the label is already in the box
        // add the new value and copy the end of the list
        result.Add(label, value)
        result.AddMany(tail)
        result.Close()
    | (l,v) :: tail ->
        // copy the existing label and value
        result.Add(l,v)
        addToBox' label value tail (&result)

let addToBox label value list =
    let mutable result = ListCollector()
    addToBox' label value list &result

// remove label from box
let removeFromBox label list =
    list |> List.filter (fun (l,_) -> l<>label)

    
// check the two functions
addToBox "cn" 4 ["rn", 1; "cn", 2; "pc", 3] = ["rn", 1; "cn", 4; "pc", 3]
removeFromBox "cn" ["rn", 1; "cn", 2; "pc", 3] = ["rn", 1; "pc", 3]

/// performs a step
let doStep (step: string) (boxes: _[]) =
    if step.EndsWith '-' then
        // we should remove the label
        let label = step[0..^1] // extract label
        let boxIndex= hashStr label // find box index
        // update the box by removing label
        boxes[boxIndex] <- removeFromBox label boxes[boxIndex]
    else
        let separator = step.IndexOf('=')
        let label = step[0..separator-1] // extract label

        let value = Int32.Parse(step.AsSpan().Slice(separator+1)) // extract value

        let boxIndex = hashStr label // find box index
        // add/update label and value 
        boxes[boxIndex] <- addToBox label value boxes[boxIndex]

let boxes = Array.replicate 256 []
for step in input.Split(',') do
    doStep step boxes

/// compute the power in a box
let powerInBox (slots: (string*int) list) =
    slots |> Seq.indexed |> Seq.sumBy (fun (i,(_,v)) -> (i+1) * v)

// compute the power for all boxes
boxes |> Seq.indexed |> Seq.sumBy (fun (i,slots) -> (i+1) * powerInBox slots  )





