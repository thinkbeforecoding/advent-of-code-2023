

open System
open System.Collections.Generic

let input = IO.File.ReadAllLines "./input/day25.txt"
//     """jqt: rhn xhk nvd
// rsh: frs pzl lsr
// xhk: hfx
// cmg: qnr nvd lhk bvb
// rhn: xhk bvb hfx
// bvb: xhk hfx
// pzl: lsr hfx nvd
// qnr: nvd
// ntq: jqt hfx bvb xhk
// nvd: lhk
// lsr: lhk
// rzs: qnr cmg lsr rsh
// frs: qnr lhk lsr""".Split '\n'

let parse (l: string) =
    let parts = l.Split ' '
    let source = parts[0].TrimEnd(':')
    [ for p in Seq.skip 1 parts do 
        source,p ]

let wires = input |> Seq.collect parse |> Seq.toList 
wires |> List.map(fun (s,e) -> $"{s}-{e}") |> fun l -> IO.File.WriteAllLines("out25.txt", l)


let connections =
    [ for s,e in wires do 
        s,e
        e,s ]
    |> List.groupBy fst
    |> List.map (fun (s,l) -> s, List.map snd l)
    |> Map.ofList
    
let order s e = min s e, max s e
let edges = 
    [ for s,e in wires do
        order s e ]
    |> set

let components = connections.Keys |> set
components.Count


let source = components.MinimumElement
let sink = components.MaximumElement


let rec findPath target g (visited: _ HashSet ) (queue: _ Queue)  =
    match queue.TryDequeue() with
    | false,_ -> None
    | true, (v, path)  ->
        if v = target then
            Some ( List.rev (v :: path) )
        elif visited.Contains v then
            findPath target g visited queue
        else
            visited.Add(v) |> ignore
            for n in connections[v] do
                if Set.contains (order n v) g then
                    queue.Enqueue(n, v :: path)
               
            findPath target g visited queue 



let path source sink g = 
    let queue = Queue()
    queue.Enqueue(source,[])
    match findPath sink g (HashSet()) queue with
    | None -> []
    | Some p ->
        p |> List.pairwise

path source sink edges

edges.Count

let addFlow e n f =
    match Map.tryFind e f with
    | None -> Map.add e n f
    | Some v -> Map.add e (v+n) f

let rec maxFlow source sink g (flow: Map<string*string, int>) paths mf =

    match path source sink g  with
    | [] -> flow, paths, mf, g
    | p ->
        let newFlow = 
            (flow,p) ||> List.fold (fun f (s,t) ->
                f
                |> addFlow (s,t) 1
                |> addFlow (t,s) -1
            )

        let newg = g - (p |> List.map (fun (s,e) -> order s e) |> set)
        maxFlow source sink newg newFlow (p :: paths) (mf + 1)

let componentsl = Set.toList components


let s = componentsl[Random.Shared.Next(componentsl.Length)]
let t = componentsl[Random.Shared.Next(componentsl.Length)]
let source = s
let sink = t
let f, ps, mf, rg = maxFlow source sink edges Map.empty [] 0

let vs = [ for s,e in rg do
            s
            e ] |> set

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


let rs = reachable rg source

rs

let cut =
    [ for s,e in edges do
        if rs.Contains s && not (rs.Contains e) || rs.Contains e && not (rs.Contains s) then
            order s e ] |> set
let split = edges - cut




let countSource = reachable split source |> Seq.length
let countSink = reachable split sink |> Seq.length


countSource * countSink





    



