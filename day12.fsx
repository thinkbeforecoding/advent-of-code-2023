open System

let input = IO.File.ReadAllLines "./input/day12.txt"

// parse input
let lines =
    input |> Array.map(fun l -> 
    match l.Split(' ') with
    | [| map; check |] ->
        map,
            check.Split(',') |> Seq.map int |> Seq.toList 
    | _ -> failwith "Invalid input")

// Part 1
// I used a straight forward, but naive solution for part1:
// It computes all possibilities to arrange groups in total size,
// and check if the fit with the given map.

// total is the total count of dots only (length - sum of groups)
// the result is an alternance of count of . and count of # (starting always with .)
let rec all first total (groups: int list)  =
    match groups with
    | [] -> seq {[ total] } // pad then end with dots
    | c :: tail -> 
    seq {
        let start =
            if first then
                // before 1st group, there can be 0 dots
                0
            else
                // there is at least 1 dot before other groups
                1

        // try all possible number of dots
        for i in start .. total-tail.Length do
            // do the same for the remaining groups, with less remaining dots
            let rest = all false (total-i) tail
            // compile all in a list
            for r in rest do
                i :: c :: r
    }

// create a string from the result of previous function
let toString list =
    let rec loop (builder: Text.StringBuilder) working list =
        match list with
        | [] -> builder.ToString()
        | count :: tail ->
            if working then
                for i in 0..count-1 do
                    builder.Append('.') |> ignore
            else
                for i in 0..count-1 do
                    builder.Append('#') |> ignore
            loop builder (not working) tail
    loop (Text.StringBuilder()) true list

// check if generated string matches input string
let isMatch a b =
    Seq.forall2( fun x y -> x = y || x = '?' ) a b

// count possibilities
let possibilities (map: string, counts)  =
    let lenMap = map.Length
    let broken = List.sum counts
    all true (lenMap - broken) counts // generate all arrangements
    |> Seq.map toString               // generate corresponding string
    |> Seq.filter (isMatch map)       // filter those that fit
    |> Seq.length                     // count the number possibilities

lines |> Seq.sumBy possibilities

// Part 2
// the previous solution was faaaaaar too slow for this one,
// especially on this pattern 
// ???????#???#?????? 1,1,7,1,1
// which has 5_034_059_954 possibilites once repeated 5 times
// the new approach is just advancing in the map, and the group list
// It is quite simple for . and #. For ? it explore both possibilites
// and sum them.
// To make it fast, it caches results count by index*groups.
// On '?', both possibilites are explored, using the cache, if one
// encounter a situation already seen in the other branch, it can
// reused the result directly. For the example above, the time
// went from hours to milliseconds. 



open System.Collections.Generic
let rec loopDot (map: string) index groups (cache: Dictionary<int*int list, int64>) =
    // Check in the cache if the same situation has not already been computer
    match cache.TryGetValue((index,groups)) with
    | true, v -> v  // yes, just use he value
    | false, _ ->
        // we have to compute it

        // check if given current index, there is enough room left to put remaining groups and separator spaces
        if index + List.sum groups + List.length groups - 1 > map.Length then
            0L // nope, no need to go further
        elif index >= map.Length then
            // there is necessarily no group left, or previous condition would have failed
            1L // we did it !! Found 1 working possibility
        else
            // not at the end, look at groups
            match groups with
            | [] -> 
                // no group left, there should be no # anymore after
                if map.IndexOf('#', index) < 0 then
                    1L  // no #, success
                else
                    0L // oh no, this is not a solution
            | len :: tail -> // len is the length of the next group
                let n =
                    match map[index] with
                    | '.' ->
                        // this is a simple dot, compute for next index
                        loopDot map (index+1) groups cache
                    | '#' -> 
                        // this is a hash, we compute it with this other function
                        // we remove directly this # from the len to avoid doing 
                        // the work twice 
                        loopHash map (index+1) (len-1) tail cache
                    | _ (*?*) ->
                        // this could be a hash or a dot, try both and sum the possibilites
                        loopHash map (index+1) (len-1) tail cache
                        + loopDot map (index+1) groups cache
                // now that we have the possibilities count at this index for this groups,
                // we add it to the cache, in case another path arrives at the same point
                cache.Add((index, groups), n)
                n
and loopHash (map: string) index len groups (cache: Dictionary<int*int list, int64>) =
    // this function take care of consuming a group
    let end' = index+len
    // check whether the end' of the group is after the end of the map
    if end' > map.Length then
        0L // sadly yes, try something else
    else
        // no, we look for the next '.'
        let nextDot =
            match map.IndexOf('.',index) with
            | -1 -> map.Length // dot was not found, consider it's just after the end of the map
            | n -> n
        if nextDot >= end' then
            // the next '.' is after the end of the group, we can continue
            if end' = map.Length  then
                // we reached the end, it's a success if there is no group left
                if List.isEmpty groups then
                    1L
                else
                    0L
            elif map[end'] = '#' then
                // the next char is #... so the group would be actually longer
                0L
            else
                // the caracter just after the group is not a #, we can contine
                loopDot map (end'+1) groups cache
        else
            // there is a dot before the end, try something else
            0L

/// count all possibilites for given input
let count (map,groups) = loopDot map 0 groups (Dictionary())

// do part 1 again to check it works
lines |> Seq.sumBy count

// compute part 2
#time 
lines  
       |> Seq.sumBy (fun (m,c) -> 
            count (m + "?" + m + "?" + m + "?" + m + "?" + m, c@c@c@c@c) )
