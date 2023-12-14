open System

let input = IO.File.ReadAllLines "./input/day12.txt"
// let input = """???.### 1,1,3
// .??..??...?##. 1,1,3
// ?#?#?#?#?#?#?#? 1,3,1,6
// ????.#...#... 4,1,1
// ????.######..#####. 1,6,5
// ?###???????? 3,2,1""".Split('\n')
let lines =
    input |> Array.map(fun l -> 
    match l.Split(' ') with
    | [| map; check |] ->
        map,
            check.Split(',') |> Seq.map int |> Seq.toList 
    | _ -> failwith "Invalid input")



let rec all first total (groups: int list)  =
    match groups with
    | [] -> seq {[ total] }
    | c :: tail -> 
    seq {
        let start =
            if first then
                0
            else
                1
        for i in start .. total-tail.Length do
            let rest = all false (total-i) tail
            for r in rest do
                i :: c :: r

    }

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

let isMatch a b =
    Seq.forall2( fun x y -> x = y || x = '?' ) a b
    
let possibilities (map: string, counts)  =
    let lenMap = map.Length
    let broken = List.sum counts
    all true (lenMap - broken) counts
    |> Seq.map toString
    |> Seq.filter (isMatch map)
    |> Seq.length

lines |> Seq.sumBy possibilities





let possiblySharp = [| '#';'?' |]
let possiblyDot = [| '.';'?' |]
let map,counts = "?###????????", [3;2;1]
let index = 0
let first = true
let groups = counts
let total = map.Length - List.sum groups

let rec findStart' (map: string) index first c =
    if index >= map.Length then
        -1
    else
        let nextGroup = map.IndexOfAny(possiblySharp, index)
        if nextGroup < 0 then
            nextGroup
        else
            let start =
                if first then index else index+1
                |> max nextGroup
            if start+c >= map.Length then
                -1
            else
                let span = map.AsSpan().Slice(start, c)
                match span.IndexOf('.') with
                | -1 -> 
                    if start+c < map.Length && map[start+c] = '#' then
                        findStart' map (start+1) first c
                    else
                        start
                | n -> findStart' map (start+n) first c

let findStart (map: string) index first c =
    try
        findStart' map index first c
    with
    | _ ->
        printfn $"{map} {index} {first} {c}"
        reraise()


let rec count (map: string) index first total (groups: int list)  =
    match groups with
    | [] -> seq { [ total] }
    | c :: tail -> 
        let start = findStart map index first c
        if start < 0 then
            Seq.empty
        else
            seq {
                let dotCount = start-index
                
                for i in dotCount .. min(total-tail.Length) (map.Length-index-c) do
                    if map.AsSpan().Slice(index+i,c).IndexOf('.') < 0 then
                        let rest = count map (index+i+c) false (total-i) tail
                        if Seq.isEmpty rest then
                            ()
                        else
                            for r in rest do
                                i :: c :: r
        }
let possibilities2 (map: string, counts)  =
    let lenMap = map.Length
    let broken = List.sum counts
    count map 0 true (lenMap - broken) counts
    |> Seq.map toString
    |> Seq.filter (isMatch map)
    |> Seq.length

let testAll (map: string) (groups: int list) = all  true (map.Length - List.sum groups) groups |>  Seq.filter (toString >> isMatch map) |> Seq.toList 
let testCount (map: string) (groups: int list) = count map 0 true (map.Length - List.sum groups) groups |>  Seq.filter (toString >> isMatch map)  |> Seq.toList 
testCount "???.###" [1;1;3]
testCount ".??..??...?##." [1;1;3]
testAll "?#?#?#?#?#?#?#?" [1;3;1;6]
testAll "????" [4]
testAll "????.######..#####." [ 1;6;5]
testAll "????" [ 1]
testCount "?###????????" [3;2;1]
lines  //|> Seq.map (fun (m,c) -> List.replicate 5 m |> String.concat "?", List.replicate 5 c |> List.concat )
        |> Seq.sumBy (fun l -> 
            printf "."
            possibilities2 l)


testAll "?#?#?#?#?#?#?#?" [1;3;1;6]

let rec loopDot (map: string) index groups =
    if index + List.sum groups > map.Length then
        0
    elif index >= map.Length then 
        if List.isEmpty groups then
            1
        else
            0
    else
        match groups with
        | [] -> 
            if index = map.Length || map.IndexOf('#', index) < 0 then
                1
            else
                0
        | c :: tail ->
            match map[index] with
            | '.' -> loopDot map (index+1) groups
            | '#' -> 
                loopHash map (index+1) (c-1) tail
            | _ (*?*) ->
                loopHash map (index+1) (c-1) tail
                + loopDot map (index+1) groups
and loopHash (map: string) index len groups =
    if index+len > map.Length then
        0
    else
        let nextDot =
            match map.IndexOf('.',index) with
            | -1 -> map.Length
            | n -> n
        if nextDot >= len+index then
            if index+len = map.Length  then
                if List.isEmpty groups then
                    1
                else
                    0
            elif index+len > map.Length then
                0
            elif map[index+len] <> '#' then
                loopDot map (index+len+1) groups
            else
                0
        else
            0

loopDot "?#?#?#?#?#?#?#?" 0 [1;3;1;6]
loopDot "?###????????" 0 [3;2;1]
loopDot "????.######..#####." 0 [1;6;5]
let map, groups = "?????#???.??.??????????#???.??.??????????#???.??.??????????#???.??.??????????#???.??.????", [6; 1; 1; 1; 2; 6; 1; 1; 1; 2; 6; 1; 1; 1; 2; 6; 1; 1; 1; 2; 6; 1; 1; 1; 2]  
let index = 0

let count (map,groups) = loopDot map 0 groups

let m,c = "???????#???#??????", [1; 1; 7; 1; 1]  

let c1 = count (m,c)
let c2 = count (m + "?" + m, c@c)
let c3 = int64 (c2/c1)
let cv = count (m + "?" + m + "?" + m, c@c@c)
let cv2 = count (m + "?" + m + "?" + m + "?" + m, c@c@c@c)
let cv3 = count (m + "?" + m + "?" + m + "?" + m + "?" + m, c@c@c@c@c)
int64 c1 * pown c3 4
count ("?????#???.??.??????????#???.??.??????????#???.??.??????????#???.??.??????????#???.??.????", [6; 1; 1; 1; 2; 6; 1; 1; 1; 2; 6; 1; 1; 1; 2; 6; 1; 1; 1; 2; 6; 1; 1; 1; 2] )
count ("?????#???.??.??????????#???.??.??????????#???.??.??????????#???.??.??????????#???.??.????", [6; 1; 1; 1; 2; 6; 1; 1; 1; 2; 6; 1; 1; 1; 2; 6; 1; 1; 1; 2; 6; 1; 1; 1; 2] )

lines |> Seq.sumBy count
lines  
       |> Seq.sumBy (fun (m,c) -> 
            printf "."
            let c1 = count(m,c)
            let c2 = count(m + "?"+m, c @ c )
            let c3 = int64 (c2/c1)

            let cv = count (m+"?"+m+"?"+m, c@c@c)
            if int64 cv <> int64 c1 * pown c3 2 then 
                printfn "%d %d %s %A" (int64 cv)  (int64 c1 * pown c3 2) m c
                count (m + "?" + m + "?" + m + "?" + m + "?" + m, c@c@c@c@c) |> int64
            else
                int64 c1 * pown c3 4)

"???????#???#??????"
"#.#....#######.#.#"
"#..#...#######.#.#"
"#...#..#######.#.#"
"#....#.#######.#.#"
".#.#...#######.#.#"
".#..#..#######.#.#"
".#...#.#######.#.#"
"..#.#..#######.#.#"
"..#..#.#######.#.#"
"...#.#.#######.#.#"

"#.#...#######.#.#."
"#..#..#######.#.#."
"#...#.#######.#.#."
".#.#..#######.#.#."
".#..#.#######.#.#."
"..#.#.#######.#.#."

"#.#...#######.#..#"
"#..#..#######.#..#"
"#...#.#######.#..#"
".#.#..#######.#..#"
".#..#.#######.#..#"
"..#.#.#######.#..#"

"#.#...#######..#.#"
"#..#..#######..#.#"
"#...#.#######..#.#"
".#.#..#######..#.#"
".#..#.#######..#.#"
"..#.#.#######..#.#"

"#.#..#######.#.#.."
"#..#.#######.#.#.."
".#.#.#######.#.#.."

"#.#..#######.#..#."
"#..#.#######.#..#."
".#.#.#######.#..#."

"#.#..#######.#...#"
"#..#.#######.#...#"
".#.#.#######.#...#"

"#.#..#######..#.#."
"#..#.#######..#.#."
".#.#.#######..#.#."

"#.#..#######..#..#"
"#..#.#######..#..#"
".#.#.#######..#..#"

"#.#..#######...#.#"
"#..#.#######...#.#"
".#.#.#######...#.#"
