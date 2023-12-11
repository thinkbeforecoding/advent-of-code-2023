open System

// let map = IO.File.ReadAllText("./input/day10.txt")
let map =
    """FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJIF7FJ-
L---JF-JLJIIIIFJLJJ7
|F|F-JF---7IIIL7L|7|
|FFJF7L7F-JF7IIL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L""".Replace("\n", Environment.NewLine)
let width = map.IndexOf(Environment.NewLine)
let lineWidth = width + Environment.NewLine.Length
let height = (map.Length + Environment.NewLine.Length) / lineWidth

let getPos index =
    index%lineWidth, index/lineWidth

let s= map.IndexOf('S') |> getPos

type Dir = Left | Up | Right | Down
let peek (x,y) =
    map[x+lineWidth*y]

let (++) (x1,y1) (x2,y2) = x1+x2, y1+y2
let up = 0,-1
let down = 0,1
let left = -1,0
let right = 1,0

let rec follow s dir len =
    let n =
        match dir with
        | Up -> s++up
        | Down -> s++down
        | Left -> s++left
        | Right -> s++right

    match peek n, dir with
    | '|', Up -> follow n Up (len+1)
    | '|', Down -> follow n Down (len+1)
    | '-', Left -> follow n Left (len+1)
    | '-', Right -> follow n Right (len+1)
    | 'L', Down -> follow n Right (len+1)
    | 'L', Left -> follow n Up (len+1)
    | 'J', Right -> follow n Up (len+1)
    | 'J', Down -> follow n Left (len+1)
    | '7', Up -> follow n Left (len+1)
    | '7', Right -> follow n Down (len+1)
    | 'F', Up -> follow n Right (len+1)
    | 'F', Left -> follow n Down (len+1)
    | 'S',_ -> (len+1) /2
    | _ -> -1

// follow s Up 0
follow s Left 0
// follow s Right 0
// follow s Down 0


let path = Text.RegularExpressions.Regex.Replace(map, @"[^\r\n]", " ") |> Seq.toArray
let plot (x,y) c = path[x+y*lineWidth] <- c
let peekPath (x,y) = path[x+y*lineWidth]
let printPath() = printfn "%s" (String path)
printPath()
let rec draw s dir : bool  =
    match peek s,dir with
    |  '|', Up -> plot s '<'
    |  '|', Down -> plot s '>'
    | 'F', Down -> plot s '>'
    | 'F', Right -> plot s '<' 
    | 'L', Up -> plot s '<'  
    | 'L', Right -> plot s '>'  
    | '7', Down -> plot s '>'
    | '7', Left -> plot s '<'
    | 'J', Up -> plot s '<'
    | 'J', Left -> plot s '>'
    | '-', _ -> plot s '-'
    | 'F', _ -> plot s 'F'
    | 'L', _ -> plot s 'L'
    | '7', _ -> plot s '7'
    | 'J', _ -> plot s 'J'
    | 'S', _ -> plot s 'S'
    | _ -> ()
    let n =
        match dir with
        | Up -> s++up
        | Down -> s++down
        | Left -> s++left
        | Right -> s++right

    match peek n, dir with
    | '|', Up -> draw n Up 
    | '|', Down -> draw n Down
    | '-', Left -> draw n Left
    | '-', Right -> draw n Right
    | 'L', Down -> draw n Right
    | 'L', Left -> draw n Up
    | 'J', Right -> draw n Up
    | 'J', Down -> draw n Left
    | '7', Up -> draw n Left
    | '7', Right -> draw n Down
    | 'F', Up -> draw n Right
    | 'F', Left -> draw n Down
    | 'S',_ -> true
    | _ -> false

// draw s Up 
draw s Left

printPath()

let rec countLine (x,y) inside count =
    if x < width then
        match peekPath (x,y)with
        | '|' -> countLine (x+1,y) (not inside) count
        | '-' -> countLine (x+1,y) inside count
        | _ ->
            if inside then
                countLine (x+1,y) inside (count+1)
            else
                countLine (x+1,y) inside count
    else
        count


[ for i in 0 .. height-1 do
    countLine (0,i) false 0
] |> List.sum


