
open System
open System.Collections.Generic

// This year, there was a lot of path finding algorithms to write !
// and usualy, in path finding, you're looking for the shortest path,
// or the one that minimize a value. Here, we're looking for the longuest path.
let input = IO.File.ReadAllText "./input/day23.txt"

// As usual, width, and height, an lw it the line width including the newline
let w = input.IndexOf Environment.NewLine
let lw = w + Environment.NewLine.Length
let h = input.Length / lw

// peek a char at x,y
let peek (x,y) = input[x+lw*y]

// compute x,y position from array index
let position index = index % lw, index / lw
let inMap (x,y) = x >= 0 && x < w && y >= 0 && y < h
// find the start (this is the first '.' on the first line)
let start = position (input.IndexOf '.')
// find the end, (this is the last '.' on the last line)
let exit = position (input.LastIndexOf '.')

// add vectors, a
let (++) (x1,y1) (x2,y2) = x1+x2,y1+y2

// the manhattan distance between two points
let dist (x1,y1) (x2,y2) = abs (x2-x1) + abs(y2-y1)

// the 4 directions as vector
let N = (0,-1)
let S = (0,1)
let W = (-1, 0)
let E = (1, 0)


// We use a priority queue with negative total distance, so that we process the longuest expected path first.
// the value in the queue contains, the position (to know where we are), the directions (necessary to pass slides and
// useful to speed up corridors traversal), and numbers of steps to compute the result
let addPos pos dir steps (stack: PriorityQueue<(int*int) * (int*int) * int, int>) =
    let newPos = pos ++ dir
    stack.Enqueue((newPos, dir, steps+1), - (steps+1 + dist newPos exit))

// find the longuest path
let rec findLonguestPath (visited: Dictionary<int* int, int>)  (stack: PriorityQueue<(int*int) * (int*int) * int, int>) result =
    if stack.Count = 0 then
        // we examined all paths, we can return the longuest
        result
    else
        let pos, dir, steps =  stack.Dequeue()
        // get the position, direction, and number of steps of the path to process (longuest first)
        if pos = exit then
            // we reached the exit !
            // fine, but this may not be the longuest path...
            // continue other pahts, but the result is the max between 
            // this path and those already visited
            findLonguestPath visited stack (max steps result)
        elif inMap pos then
            // the position is in map check if it has already been visited
            let cont =  
                match visited.TryGetValue(pos) with
                | false, _ -> true // no, continue
                | true, v -> v < steps // yes, continue only is current path is longuer


            if cont then
                // let's continue, update number of steps for this cell
                visited[pos] <- steps

                match peek pos with
                | '.' ->
                    // this is a free cell, add each direction 
                    // (but avoid adding reverse direction, as we don't want to go twice on the 
                    // same cell)
                    if dir <> S then addPos pos N steps stack
                    if dir <> E then addPos pos W steps stack
                    if dir <> N then addPos pos S steps stack
                    if dir <> W then addPos pos E steps stack
                    findLonguestPath visited stack result
                | '#' ->
                    // this is a wall, just try another path
                    findLonguestPath visited stack result
                | '>' ->
                    // it can be traversed only if going East
                    if dir = E then 
                        addPos pos E steps stack
                    findLonguestPath visited stack result
                | '<' ->
                    // it can be traversed only if going West
                    if dir = W then
                        addPos pos W steps stack
                    findLonguestPath visited stack result
                | 'v' ->
                    // only if going South
                    if dir = S then
                        addPos pos S steps stack
                    findLonguestPath visited stack result
                | '^' ->
                    // only if going North
                    if dir = N then
                        addPos pos N steps stack
                    findLonguestPath visited stack result
                | _ -> failwith "Unkown cell" 
            else
                // the cell has already been visited and there's nothing
                // more to do
                findLonguestPath visited stack result
        else
            // the cell is out of map, stop and try another path
            findLonguestPath visited stack result


// start the queue with the starting point
let q = PriorityQueue()
q.Enqueue((start, S, 0), 0)
findLonguestPath (Dictionary()) q 0


// Part 2
// The slides in part 1 where limiting highly the number of possible paths, and
// it made it impossible to have loops, or go twice on a crossroad...
// Now, without the slides, the number of possible path is far higher, and going through
// all possibilities will take ages.
// However, the map is made of long corridors meeting at crossroads. Converting the map
// to a smaller graph will speed things up

// same as above, but check directly if this is a wall to avoid exploring it later
// The queue contains the position, direction and steps (at the end)  like previsously
// but also the start position and direction of the corridor. 
let addPos2 pos dir start startDir steps stack =
    let newPos = pos ++ dir
    if peek newPos <> '#' then
        (newPos, dir, start, startDir , steps+1) :: stack
    else
        stack

// returns 1 if the direction is not a wall, 0 other whise
let isOpen pos dir =
    if peek (pos ++ dir) <> '#' then 1 else 0

// an interesection has more that 2 open ends (with 2, this is a corridor)
let isIntersection pos = 
    isOpen pos N + isOpen pos S + isOpen pos W + isOpen pos E > 2

// orient p1 an p2, from min to max
let orient p1 p2 =
    min p1 p2, max p1 p2


// the algorithm is quite similar, but tries to speedrun corridors
let rec findCorridors (stack:  ((int*int) * (int*int) *  (int*int) * (int*int) * int) list) corridors =

    match stack with
    | [] ->
        // no more path to explore, done
        corridors
    | (pos, dir, start, startDir, steps) :: stack ->
        // explore the next position from the stack

        if pos = exit then
            // this is the exit, (it will not be detected as an intersection)
            // but add it as the end of the current corridor
            findCorridors stack ( (orient start pos, steps) :: corridors)
        elif isIntersection pos then
            // we're at an intersection...
            // add the positions from start and pos and the number of steps to corridors   
            let paths = (orient start pos, steps) :: corridors
            

            // intersections are all surounded by slides, use them to know where to explore
            // and always arrive in the direction of the slide
            let s1 = if peek (pos++N) = '^' then addPos2 pos N pos N 0 stack else stack
            let s2 = if peek (pos++S) = 'v' then addPos2 pos S pos S 0 s1 else s1
            let s3 = if peek (pos++W) = '<' then addPos2 pos W pos W 0 s2 else s2
            let s4  =if peek (pos++E) = '>' then addPos2 pos E pos E 0 s3 else s3

            findCorridors s4 paths

        else
            // this is not an intesection
            // in case this is a slide, continue processing only if in the correct direction
            // This avoids making complicated things to avoid circles
            let cont =
                match peek pos with
                | '^' -> dir = N
                | 'v' -> dir = S
                | '<' -> dir = W
                | '>' -> dir = E
                | _ -> true
            
            if cont then
                // add surounding positions to the stack (if it's not a wall)
                // and never add the position we come from
                let s1 = if dir <> S then addPos2 pos N start startDir steps stack else stack
                let s2  = if dir <> E then addPos2 pos W start startDir steps s1 else s1
                let s3 = if dir <> N then addPos2 pos S start startDir steps s2 else s2
                let s4 = if dir <> W then addPos2 pos E start startDir steps s3 else s3
                findCorridors s4 corridors
            else
                findCorridors stack corridors



// use findCorridors to get all start/end points and length, there is only 60 corridors
let corridors =
    let stack = addPos2 start S start S 0 []
    findCorridors stack []
    |> List.distinct

// make a list of all corridors' ends
let intersections = 
    set (List.map (fst >> fst) corridors
    @ List.map (fst >> snd) corridors)

// for each interesction, mark all other ends where corridors can lead 
let corridors2 = 
    [ for i in intersections do
        i,set [for (s,e),l in corridors do
                if i = s then
                    e,l
                if i = e then
                    s,l ]
    ] |> Map.ofList


// now that we have a graph, find the longuest path
let rec runPaths stack length =
    match stack  with
    | [] -> 
        // all possibilites explored, return the longuest path
        length
    | (pos,visited, paths,steps)  :: tail -> 
        // visit an intersection
        if pos = exit then
            // it is the exit, update max length if the number of steps
            // is higher, continue because we could find a longer path later
            runPaths tail (max length steps)
        else
            // find the other ends of each corridors at this intersection
            match Map.tryFind pos paths with
            | None ->
                // this should not happen, but in case of dead end, continue 
                runPaths tail length
            | Some directions ->
                // prepare all next interesctions to visit
                let stack = 
                    [for (otherEnd, l ) in  directions do
                        // l is the length between current intersections and other end 
                        if not (Set.contains otherEnd visited) then
                            // we maintain a visited set for each path to avoid going twice on the same
                            // intersection in a same path
                            // we also maintain the graph for each path, so that we can remove
                            // taken corridors to avoid turning in circle
                            let removedEnd = Set.remove (otherEnd,l) directions
                            // remove path in both directions
                            let revDirections = Map.find otherEnd paths
                            let removedStart = Set.remove (pos,l) revDirections
                            let newPaths = paths |> Map.add pos removedEnd |> Map.add otherEnd removedStart


                            otherEnd, Set.add otherEnd visited,  newPaths, steps+l 
                            ]
                    @ tail
                runPaths stack length
                
        
// find the longuest walk in the graph
// it's quite long but finishes in a limited time (2min 1s on my machine)
runPaths [ start, Set.singleton start, corridors2, 0 ] 0