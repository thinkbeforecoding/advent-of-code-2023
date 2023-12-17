open System
open System.Collections.Generic

// this one was harder. It started fast, we can use the Dir structure from day16
// and it globaly looks like a A* algorithm... but there are some subtle differences
// that failed me for a few hours...

let input =  IO.File.ReadAllText "./input/day17.txt" 
let w = input.IndexOf(Environment.NewLine)
let lw = w+Environment.NewLine.Length
let h = (input.Length+Environment.NewLine.Length) / lw

let peek(x,y) = int input[x+lw*y] - int '0'
let inMap (x,y) = x >= 0 && x < w && y >= 0 && y < h

// as yesterday, 
[<Struct>]
type Dir = Dir of int
    with
    override this.ToString() =
        match this with
        | Dir 0 -> "N"  
        | Dir 1 -> "E"
        | Dir 2 -> "S"
        | Dir 3 -> "W"
        | _ -> failwith "Invalid value"

fsi.AddPrinter<Dir>(string)
module Dir =
    let turnRight (Dir d) = Dir( (d + 1) &&& 3 )
    let turnLeft (Dir d) = Dir( (d - 1) &&& 3 )

let N = Dir 0
let E = Dir 1
let S = Dir 2
let W = Dir 3

let (|N|E|S|W|) (Dir n) = 
    match n with
    | 0 -> N
    | 1 -> E
    | 2 -> S
    | 3 -> W
    | _ -> failwith "Invalid dir"

let move (x,y) dir =
    match dir with
    | N -> x,y-1
    | E -> x+1,y
    | S -> x,y+1
    | W -> x-1, y

// Along the path we maintain a 4uple that contains:
// * the xy position of the crucible
// * the direction of the crucible
// * the number or moves in the same direction
// * the path so far, with direction (in reverse order)

type Pos = int*int
type Crucible = Pos * Dir * int * (Pos*Dir) list

// turn left from current position and direction
let moveLeft (pos,dir, _, path) =
    let newdir = Dir.turnLeft dir // turn left
    let newPos = move pos newdir  // make one step
    newPos, newdir, 1, ((newPos, newdir) :: path) // reset move count to 1, and add new pos to path

// turn right from current position and direction
let moveRight (pos,dir,_, path) =
    let newdir = Dir.turnRight dir      // turn right
    let newPos = move pos newdir        // make one step
    newPos, newdir, 1, ((newPos, newdir) :: path) // reset move count to 1, and add new pos to path

// check if more move forward can be made
let canMoveForward(_,_,n,_) = n < 3

// move forward
let moveForward (pos, dir, n,path) =
    let newPos = move pos dir
    newPos, dir, n+1, (newPos,dir) :: path // increment count in the same direction

let position (pos,_,_,_) = pos

// compute the remainin distance using the manhattan distance to the bottom right cell
let remaining ((x,y),_,_,_) = abs (w-1-x) + abs (h-1-y) 
// return the position, direction and count
let posdircount (pos,dir,c,_) = pos,dir,c
// return the path
let path (_,_,_,p) = p


// add a new crucible position, if ok
// the difficulty here is that this is a variation on A*
// in classic A*, on each cell, we add accessible neighour cells to a list, and we continue from the shortest path
// a PriorityQueue is used to get the shorted path first, the priority is given by the sum of the current path length
// plus the direct distance to the final point (using the manhattan distance).
// The first arrival in a cell is always the shortest, so any other path that goes through this cell is longer, and can be stopped
//
// in the case where cells are weighted (like here and https://adventofcode.com/2021/day/15),
// the cells may be visited multiple time, because a longer path with lower weights can be more efficient.
// As the short path is explored first, we must keep track of the total cost, and when exploring longer path
// for the same cell, we can continue if the new total cost is lower, otherwise we can stop.
//
// here, this is slightly more complicated, because of the 3 moves limit. Even if locally, a longer path with a higher
// cost should probably stop, it is important to continue  to explore, because the 3 moves limit could lead furthe to a better
// result. If the cell is visited after the 1st move in a direction, we can explore to more cells in the same direction,
// but if the cell is visited after the 2nd move, only 1 more cell can be visited in the same direction, giving different results.
// more over, cells can be visited in different direction.
// for this, the dictionary for track visited cells has a Pos*Dir*count key.
let addNext (paths: PriorityQueue< Crucible * int , int >) (visited: Dictionary<Pos*Dir*int,int>)  crucible total =
    if inMap  (position crucible) then
        // the crucible is in map, compute new total heat
        let heat = peek (position crucible)
        let newTotal = total + heat

        // check if the cell has been visited for this direction and move count        
        match  visited.TryGetValue( posdircount crucible ) with
        | false, _ ->
            // nope, we add it to visited cell with the total cost
            visited.Add(posdircount crucible, newTotal )
            // enqueue the path, the priority is the total cost + the manhattan distance to the exit
            paths.Enqueue((crucible, newTotal), newTotal + remaining crucible )
         | true, v -> 
            // the cell has been visited, check we have a better total cost
            if v > newTotal then
                // yep, update the best cost
                visited[posdircount crucible] <- newTotal
                // enqueue the path, the priority is the total cost + the manhattan distance to the exit
                paths.Enqueue((crucible, newTotal), newTotal + remaining crucible )

// try to find the best path, using A* like algorithm
let rec findPath (paths: PriorityQueue< Crucible * int, int>) (visited: Dictionary<Pos*Dir*int, int>) =
    // take the path with the lowest cost + remaining distance
    let crucible, total = paths.Dequeue() 
    // check if we reached the end
    if position crucible = (w-1,h-1) then
        // yes, return total cost, and the path
        total, path crucible 
    else
        // add the cell on the right and the left (if accessible, and interesting to expore further)
        addNext paths visited (moveRight crucible) total
        addNext paths visited (moveLeft crucible) total

        // if the crucible can still move forward, add the next cell 
        if canMoveForward crucible then
            addNext paths visited (moveForward crucible) total

        // continue exploring the shorted path first
        findPath paths visited

// print the path
let printPath p =
    let map = input.ToCharArray()
    for (x,y), dir in p do
        map[x+y*lw] <-
            match dir with
            | N -> '^'
            | S -> 'v'
            | E -> '>'
            | W -> '<'  
    String map |> printfn "%s"

// compute part 1 result
do
    let paths = PriorityQueue< Crucible*int , int>()
    let cs = ((0,0),S,0,[])
    let ce = ((0,0),E,0,[])
    paths.Enqueue((ce,0), remaining ce)
    paths.Enqueue((cs,0), remaining cs)
    let visited = Dictionary<Pos*Dir*int, int>()
    let l, p = findPath paths visited

    printPath p

    printfn "%d" l

// Part 2
// It is quite similar to part 1, but with new limits on the moves

// check if the crucible can turn or Stop (more than 4 in the same direction)
let canTrunOrStop (_,_,c,_) = c >= 4
// check if the crucible can continue forward (less than 10 in the same direction)
let canContinue(_,_,c,_) = c < 10

// same as findPath, but with the new rules
let rec findPath2 (paths: PriorityQueue< Crucible * int, int>) (visited: Dictionary<Pos*Dir*int, int>) =
    // take the path with the lowest cost + remaining distance
    let crucible, total = paths.Dequeue() 
    if position crucible = (w-1,h-1) then
        // the crucible is on the last cell, but we must check if it can stop
        if canTrunOrStop crucible then
            // ok we're done
            total, path crucible 
        else
            // abandon this path, and try the next shorted one
            findPath2 paths visited
    else

        // check if the crucible can continue forward
        if canContinue crucible then
            addNext paths visited (moveForward crucible) total
        
        // check if the crucible can turn
        if canTrunOrStop (crucible) then
            addNext paths visited (moveRight crucible) total
            addNext paths visited (moveLeft crucible) total

        findPath2 paths visited

// compute solution for part 2
do
    let paths = PriorityQueue< Crucible*int , int>()
    let cs = ((0,0),S,0,[])
    let ce = ((0,0),E,0,[])
    paths.Enqueue((cs,0), remaining ce)
    paths.Enqueue((ce,0), remaining ce)
    let visited = Dictionary<Pos*Dir*int, int>()
    let l, p = findPath2 paths visited

    printPath p
    printfn "%d" l
