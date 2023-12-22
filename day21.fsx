open System
open System.Collections.Generic

let input = IO.File.ReadAllText "./input/day21.txt"
//     """...........
// .....###.#.
// .###.##..#.
// ..#.#...#..
// ....#.#....
// .##..S####.
// .##..#...#.
// .......##..
// .##.#.####.
// .##..##.##.
// ...........
// """        .ReplaceLineEndings()

// this one is once again a map filling algorithm
let w = input.IndexOf(Environment.NewLine)
let lw = w+Environment.NewLine.Length
let h = input.Length/lw

let position index = index%lw, index/lw
let start = position (input.IndexOf 'S')

let inMap (x,y) = x >= 0 && x < w && y >= 0 && y < h

let peek(x,y) = input[x+y*lw]

let (++) (x1,y1) (x2,y2) = x1+x2, y1+y2
let N = (0,-1)
let S = (0,1)
let W = (-1, 0)
let E = (1,0)

peek (start ++ E)

// make a step
let rec step (visited: HashSet<int*(int*int)>) stack result =
        match stack with
        | [] ->
            // done, return the result
            result
        | (remaining,pos) :: tail ->
            // we're at a position, with a remaining move count

            if visited.Add(remaining,pos) then
                // this position has not been visited yet for this count
                if not (inMap pos) then
                    // the position is out of map, move on
                    step visited tail result
                else
                    match peek pos with
                    | '#' ->
                        // the position is blocked, move on
                        step visited tail result
                    | '.' | 'S' ->
                        // this position is free
                        if remaining > 0 then 
                            // we must continue in all directions
                            step visited ((remaining-1, pos++N)
                                            :: (remaining-1, pos++S)
                                            :: (remaining-1, pos++W)
                                            :: (remaining-1, pos++E)
                                            :: tail) result
                        else
                            // we can end here, add 1 to result
                            step visited tail (result + 1)
                    | _ -> failwith "Invalid char"
            else
                // already visited, move on
                step visited tail result

// do 64 steps
step (HashSet()) [ 64, start ] 0


// part 2

// I had the proper intution, looking at the graph that the covered area was
// growing as the square of the distance, of course this is not a strict progression
// as all obstacles modify it slightly, but, considering the shape of the input, and
// the folding of space, there should be some repetition
// I read an hint, that the repetition is at (width/2) + n * width. At each of theses
// distance, the same pattern occured, only bigger with a quadratic progression 
// of course,  26501365 is of the form (width/2) + n * width.

// do a proper positive modulo
let (%%) x y = 
    let r = x%y
    if r >= 0 then r else r+y

// apply modulo on x,y to stay in the map for folding
let modPos (x,y) = x%%w,y%%h



// this time it will be big, continue only when remaining count is higher
// apply steps from lowest distance first to be sure to fill the surface (hence the Priority queue)
let rec step2  (visited: Dictionary<(int*int),int>) (stack: PriorityQueue<int*int * (int*int), int>) count =
        if stack.Count = 0 then
            // done
            count
        else
            let depth,remaining, pos = stack.Dequeue()
            // the queue, contains the depth (number of steps already made)
            // the remaining distance, and the position
            // check if we must continue
            let cont =
                match visited.TryGetValue(pos) with
                | true, v -> remaining < v  // already visited but for smaller distance
                | false, _ -> true  // not visited

            if cont then
                // continue, and mark as visited for remaining distance
                visited[pos] <- remaining
                // peek in folded space
                match peek (modPos pos) with
                | '#' -> step2 visited stack count // blocked, move on 
                | '.' | 'S' ->
                    // free cell
                    if remaining > 0 then 
                        // there is still steps to do,
                        // move in each direction
                        stack.Enqueue((depth+1, remaining-1, pos++N), depth+1 )
                        stack.Enqueue((depth+1,remaining-1, pos++S), depth+1 )
                        stack.Enqueue((depth+1,remaining-1, pos++W), depth+1 )
                        stack.Enqueue((depth+1,remaining-1, pos++E), depth+1 )
                        step2 visited stack count
                    else
                        // we're a the end of the path, count it
                        step2 visited stack (count+1)
                | _ -> failwith "Invalid char"
            else
                // already seen, move on
                step2 visited stack count

// run step2 for a given number of steps
let run2 n = 
    let q = PriorityQueue()
    q.Enqueue((0,n,start),0)
    let r = step2 (Dictionary()) q 0
    r
    
// sample values at specific number of steps
// get area for w/2 + w
let x1' = w/2 + w |> int64
let y1' = run2 (int x1') |> int64
// get area for w/2 + w*2
let x2' = w/2 + w*2 |> int64
let y2' = run2 (int x2') |> int64
// get area for w/2 + w*3
let x3' = w/2 + w*3 |> int64
let y3' = run2 (int x3') |> int64

// we are looking for a quadratic equation of the form:
// y = a.x² + b.x + c
// and we have 3 samples where x1,y1 x2,y2 x3,y3 are values and a,b,c the variables
// y1 = a.x1² + b.x1 + c
// y2 = a.x2² + b.x2 + c
// y3 = a.x3² + b.x3 + c
// solve the linar equation system:

let x1,y1 = decimal x1', decimal y1'
let x2,y2 = decimal x2', decimal y2'
let x3,y3 = decimal x3', decimal y3'

let a = -(x1*y2-x1*y3-x2*y1+x2*y3+x3*y1-x3*y2) / ((x1-x2)*(x1-x3)*(x2-x3))
let b = (x1*x1*y2-x1*x1*y3-x2*x2*y1+x2*x2*y3+x3*x3*y1-x3*x3*y2)/((x1-x2)*(x1-x3)*(x2-x3))
let c = (x1*x1*x2*y3-x1*x1*x3*y2-x1*x2*x2*y3+x1*x3*x3*y2+x2*x2*x3*y1-x2*x3*x3*y1)/((x1-x2)*(x1-x3)*(x2-x3))

// now we have a,b,c, we can compute the value for any x of the form (width/2) + n * width
let v x= x*x*a + x*b + c

Decimal.Ceiling (v 26501365M)