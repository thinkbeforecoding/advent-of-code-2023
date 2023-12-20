
open System
open System.Collections.Generic
    
module FlipFlop =
    let run cmd s =
        match cmd with
        | _,true -> [], s
        | _,false -> [ not s ], not s

    let create name inputs outputs =
        let mutable state = false
        // printfn "%% %s %A %A" name inputs outputs
        fun (input: string, pulse ) ->
            let newpulses, newstate = run (input,pulse) state
            state <- newstate
            [ for pulse in newpulses do
                for output in outputs do
                    output, name, pulse  ]
        ,fun () -> Some state



module Conjunction =

    let run (input,pulse) s =
        let news = Map.add input pulse s
        [ news |> Map.forall (fun _ m -> m) |> not ], news

    let create name inputs outputs =
        let mutable state = Map.ofList [ for input in inputs -> input, false]
        // printfn "& %s %A %A" name inputs outputs
        fun (input, pulse ) ->
            let newpulses, newstate = run (input,pulse) state
            state <- newstate
            [ for pulse in newpulses do
                for output in outputs do
                    output, name, pulse  ]
        ,fun () -> Some (Map.forall (fun _ m -> m) state)
module Broadcaster =
    
    let run (input,pulse) () = [pulse],()
    let create name inputs outputs =
        // printfn "broadcast %A %A" inputs outputs 
        fun (input: string, pulse: bool ) ->
            let newpulses, _ = run (input,pulse) ()
            [ for pulse in newpulses do
                for output in outputs do
                    output, name, pulse  ]
        , fun _ -> None

let ff,_ = FlipFlop.create "x" ["a"] ["b"]
ff ("a",false)

let conj,_ = Conjunction.create "x" ["a";"b"] ["c"]
conj ("a", false)
conj ("a", true)
conj ("b", false)
conj ("b", true)

let bc,_ = Broadcaster.create "x" ["button"] ["a";"b";"c"] 
bc ("button", false)
bc ("button", true)

let lines = IO.File.ReadAllLines "./input/day20.txt"
    // """broadcaster -> a, b, c
    // %a -> b
    // %b -> c
    // %c -> inv
    // &inv -> a""".Split('\n')
//     """broadcaster -> a
// %a -> inv, con
// &inv -> b
// %b -> con
// &con -> output""".Split('\n')

let parseLine (l:string) =
    match l.Split("->",StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries) with
    | [| def; outputs |] ->
        let typ, name =
            if def.StartsWith("%") then
                "%", def.Substring(1)
            elif def.StartsWith "&" then
                "&", def.Substring(1)
            else
                "b", def
        typ, name, outputs.Split(',', StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
    | _ -> failwith "Invalid lines"

let modulesDef = lines |> Array.map parseLine

let inputs =
    [ for _,name, outputs in modulesDef do
        for output in outputs do
            output, name
    ] |> List.groupBy fst
    |> List.map (fun (k,l) -> k, List.map snd l)
    |> Map.ofList

let createModules() = 
    [ for typ, name, outputs in modulesDef do
        let inputs = Map.tryFind name inputs |> Option.defaultValue []
        name,
        match typ with
        | "%" -> FlipFlop.create name inputs outputs
        | "&" -> Conjunction.create name inputs outputs
        | _ -> Broadcaster.create name inputs outputs
    ] |> Map.ofList

let modules1 = createModules()
let rec run (queue: Queue<string*string*bool>) low high =
    if queue.Count = 0 then
        low, high
    else
        let name, input, pulse = queue.Dequeue()
        let newLow, newHigh =
            match pulse with
            | true -> low, high + 1L
            | false -> low+1L, high
        match Map.tryFind name modules1 with
        | Some(md,_) -> 
            for (next, m, p) in md(input,pulse) do
                // printfn $"{name} -> {p} -> {next}"   
                queue.Enqueue(next,m,p)
        | None -> ()//printfn $"?{name} -> ..."

        
        
        run queue newLow newHigh

let runOnce() =
    let queue = Queue<string*string*bool>()
    queue.Enqueue("broadcaster","button",false)
    run queue 0L 0L

let rec runN n low high =
    if n = 0 then
        low*high
    else
        let l,h = runOnce()
        runN (n-1) (l+low) (h+high)


runN 1000 0L 0L
 
let rec run2 modules2 (queue: Queue<string*string*bool>) output result =
    if queue.Count = 0 then
        result
    else
        let name, input, pulse = queue.Dequeue()
        let result =
            if input = output && pulse = true then
                Some true
            else
                result
        match Map.tryFind name modules2 with
        | Some(md,_) -> 
            for (next, m, p) in md(input,pulse) do
                // printfn $"{name} -> {p} -> {next}"   
                queue.Enqueue(next,m,p)
        | None -> ()

        
        run2 modules2 queue output result

let run2Once modules2 output state =
    let queue = Queue<string*string*bool>()
    queue.Enqueue("broadcaster","button",false)

    run2 modules2 queue output state


let rec run2N modules2 n output =
    let changes = run2Once modules2 output None
    match changes with
    | Some true -> int64 n
    | _ -> run2N modules2 (n+1) output 

let source = inputs["rx"] |> List.exactlyOne
let sources = inputs[source]

[ for s in sources do
    run2N (createModules()) 1 s ]
|> List.fold ( * ) 1L
