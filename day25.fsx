

open System
open System.Collections.Generic

// This one was also a bit tricky,
// if there was only one bridge, there is an algorithm to find cut bridges
// but here, there are three, and we must use the max flow / min cut algorithm
// https://en.wikipedia.org/wiki/Max-flow_min-cut_theorem
let input = IO.File.ReadAllLines "./input/day25.txt"

let parse (l: string) =
    let parts = l.Split ' '
    let source = parts[0].TrimEnd(':')
    [ for p in Seq.skip 1 parts do 
        source,p ]

// read all wires definitions and flaten the list
let wires = input |> Seq.collect parse |> Seq.toList 

// make a list of all connections in both directions
let connections =
    [ for s,e in wires do 
        s,e
        e,s ]
    |> List.groupBy fst
    |> List.map (fun (s,l) -> s, List.map snd l)
    |> Map.ofList
    
// reorder ends min -> max 
let order s e = min s e, max s e
// all edges in the graph
let edges = 
    [ for s,e in wires do
        order s e ]
    |> set

// make a set of all components (vertices)
let components = connections.Keys |> set


// find a path to target in graph g, visited is used to avoid going twice on the same vertex
let rec findPath target g (visited: _ HashSet ) (queue: _ Queue)  =
    match queue.TryDequeue() with
    | false,_ -> None // no more path to process, there is no path to target
    | true, (v, path)  ->
        if v = target then
            // we found the path
            // add the last point and reverse the list as it has been constructed in
            // other direction
            Some ( List.rev (v :: path) )
        elif visited.Contains v then
            // the vertex has been visited, try something else
            findPath target g visited queue
        else
            // add vertex to visited
            visited.Add(v) |> ignore
            // add all connected vertices to the queue to explore
            for n in connections[v] do
                if Set.contains (order n v) g then
                    queue.Enqueue(n, v :: path)
               
            findPath target g visited queue 

// use find path to actually compute a path from source to sink in graph g
let path source sink g = 
    let queue = Queue()
    queue.Enqueue(source,[])
    match findPath sink g (HashSet()) queue with
    | None -> []
    | Some p ->
        p |> List.pairwise

// add n to the flow of edge e
let addFlow e n f =
    match Map.tryFind e f with
    | None -> Map.add e n f
    | Some v -> Map.add e (v+n) f

// compute the max flow.
// if source and sink are in the two subgraphs, the flow should be 3 as there are 3 bridges
let rec maxFlow source sink g (flow: Map<string*string, int>) paths mf =
    // find a path from source to sink in the residual graph g (the graph with saturated edges removed)
    match path source sink g  with
    | [] -> 
        // no more path from source to sink, we have the max flow
        flow, paths, mf, g
    | p ->
        // compute the new flow on the path (add 1 to each edge on the path) 
        let newFlow = 
            (flow,p) ||> List.fold (fun f (s,t) ->
                f
                |> addFlow (s,t) 1
                |> addFlow (t,s) -1
            )

        // prepare the new residual graph by removing edges on the path
        let newg = g - (p |> List.map (fun (s,e) -> order s e) |> set)
        // add the path to paths, and increase maxflow
        maxFlow source sink newg newFlow (p :: paths) (mf + 1)

let componentsl = Set.toList components
// take a random source and sink/target
let s = componentsl[Random.Shared.Next(componentsl.Length)]
let t = componentsl[Random.Shared.Next(componentsl.Length)]
let source = s
let sink = t
// compute the flow (if it's not 3, the source and target are on the same side of the bridges, try again)
let f, ps, mf, rg = maxFlow source sink edges Map.empty [] 0


// make a list of all vertices reachable from source in the residual graph
let rec reachable' g (visited: _ HashSet ) (queue: _ Queue)  =
    match queue.TryDequeue() with
    | false,_ -> visited
    | true, v  ->
        if visited.Contains v then
            reachable' g visited queue
        else
            visited.Add(v) |> ignore
            for n in connections[v] do
                if Set.contains (order n v) g then
                    queue.Enqueue(n)
               
            reachable' g visited queue 

let reachable g source =
    let q = Queue()
    q.Enqueue(source)
    reachable' g (HashSet()) q

// rs is the set of reachable vertices
let rs = reachable rg source

// the cut is on the edges where one end is reachable (in rs) and the other is not
let cut =
    [ for s,e in edges do
        if rs.Contains s && not (rs.Contains e) || rs.Contains e && not (rs.Contains s) then
            order s e ] |> set

// remove cut edges from the full graph
let split = edges - cut



// now, the set of reachable points from source contains all the vertices from one side
let countSource = reachable split source |> Seq.length
// the set of reachable points from from sink contains all vertices from the other side
let countSink = reachable split sink |> Seq.length

// multiply both counts
countSource * countSink





    



