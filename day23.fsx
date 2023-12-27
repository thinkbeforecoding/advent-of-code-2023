open System
open System.Collections.Generic

let input = IO.File.ReadAllText "./input/day23.txt"

let w = input.IndexOf Environment.NewLine
let lw = w + Environment.NewLine.Length
let h = input.Length / lw

let peek (x,y) = input[x+lw*y]
let position index = index % lw, index / lw
let inMap (x,y) = x >= 0 && x < w && y >= 0 && y < h
let start = position (input.IndexOf '.')
let exit = position (input.LastIndexOf '.')

let (++) (x1,y1) (x2,y2) = x1+x2,y1+y2
let inv (x,y) = -x,-y


let dist (x1,y1) (x2,y2) = abs (x2-x1) + abs(y2-y1)

let N = (0,-1)
let S = (0,1)
let W = (-1, 0)
let E = (1, 0)


let addPos pos dir steps (stack: PriorityQueue<(int*int) * (int*int) * int, int>) =
    let newPos = pos ++ dir
    stack.Enqueue((newPos, dir, steps+1), - (steps+1 + dist newPos exit))

let rec findPath (visited: Dictionary<int* int, int>)  (stack: PriorityQueue<(int*int) * (int*int) * int, int>) result =
    if stack.Count = 0 then
        result
    else
        let pos, dir, steps =  stack.Dequeue()
        if pos = exit then
            findPath visited stack (max steps result)
        elif inMap pos then
            let cont =  
                match visited.TryGetValue(pos) with
                | false, _ -> true
                | true, v -> v < steps


            if cont then
                visited[pos] <- steps

                match peek pos with
                | '.' ->
                    if dir <> S then addPos pos N steps stack
                    if dir <> E then addPos pos W steps stack
                    if dir <> N then addPos pos S steps stack
                    if dir <> W then addPos pos E steps stack
                    findPath visited stack result
                | '#' -> findPath visited stack result
                | '>' ->
                    if dir = E then 
                        addPos pos E steps stack
                    findPath visited stack result
                | '<' ->
                    if dir = W then
                        addPos pos W steps stack
                    findPath visited stack result
                | 'v' ->
                    if dir = S then
                        addPos pos S steps stack
                    findPath visited stack result
                | '^' ->
                    if dir = N then
                        addPos pos N steps stack
                    findPath visited stack result
                | _ -> failwith "Unkown cell" 
            else
                findPath visited stack result
        else
            findPath visited stack result


let q = PriorityQueue()
q.Enqueue((start, S, 0), 0)
findPath (Dictionary()) q 0

let addPos2 pos dir start startDir steps (stack: PriorityQueue<(int*int) * (int*int) * (int*int) * (int*int)* int, int>) =
    let newPos = pos ++ dir
    if peek newPos <> '#' then
        stack.Enqueue((newPos, dir, start, startDir , steps+1), - (steps+1 + dist newPos exit))

let isOpen pos dir =
    if peek (pos ++ dir) <> '#' then 1 else 0

let isIntersection pos = 
    isOpen pos N + isOpen pos S + isOpen pos W + isOpen pos E > 2



let rec findPaths (visited: Set<(int* int) * (int*int)>)  (stack: PriorityQueue< (int*int) * (int*int) *  (int*int) * (int*int) * int, int>) paths =
    if stack.Count = 0 then
        paths
    else
        let pos, dir, start, startDir, steps =  stack.Dequeue()

        

        if pos = exit then
            findPaths visited stack ( (start, (pos, steps)) :: paths)
        elif isIntersection pos then
            // we're at an intersection... 
            let paths =
                (start, (pos, steps))
                :: (pos, (start, steps))
                :: paths
            
            let visited = visited |> Set.add (pos, inv dir)

            if not (Set.contains (pos, N) visited) then addPos2 pos N pos N 0 stack
            if not (Set.contains (pos, S) visited) then addPos2 pos S pos S 0 stack
            if not (Set.contains (pos, W) visited) then addPos2 pos S pos W 0 stack
            if not (Set.contains (pos, E) visited) then addPos2 pos S pos E 0 stack

            findPaths visited stack paths

        else
            if dir <> S && peek (pos ++ N) <> '#' then addPos2 pos N start startDir steps stack
            if dir <> E && peek (pos ++ W) <> '#' then addPos2 pos W start startDir steps stack
            if dir <> N && peek (pos ++ S) <> '#' then addPos2 pos S start startDir steps stack
            if dir <> W && peek (pos ++ E) <> '#' then addPos2 pos E start startDir steps stack
            findPaths visited stack paths



let paths =
    let stack = PriorityQueue()
    addPos2 start S start S 0 stack
    findPaths (Set.empty) stack []
    |> List.distinct
    |> List.groupBy fst
    |> List.map (fun (pos, l) -> pos, List.map snd l)
    |> Map.ofList


let rec runPaths stack length =
    match stack  with
    | [] -> length
    | (pos,paths,steps)  :: tail -> 
        if pos = exit then
            runPaths stack (max length steps)
        else
            match Map.tryFind pos paths with
            | None -> runPaths tail length
            | Some (directions) ->
                let stack = 
                    [for (otherEnd, l ) in  directions do 
                        let removedEnd = List.filter (fun v -> v <> (otherEnd,l)) directions

                        let revDirections = Map.find otherEnd paths
                        let removedStart = List.filter (fun v -> v <> (pos,l)) revDirections
                        let newPaths = paths |> Map.add pos removedEnd |> Map.add otherEnd removedStart

                        otherEnd,  newPaths, steps+l ]
                    @ tail
                runPaths stack length
                
runPaths [ start, paths, 0 ] 0