
open System
open System.Collections.Generic
    
// Modules are implemented as two functions 
// one that takes an input module and the pulse, and returns a list out output pulses
// the other to check the state
module FlipFlop =
    let run cmd s =
        match cmd with
        | _,true -> [], s               // high pulse, no output pulse, no change
        | _,false -> [ not s ], not s   // low pulse, output pulse, and state change

    
    let create name inputs outputs =
        let mutable state = false
        fun (input: string, pulse ) ->
            let newpulses, newstate = run (input,pulse) state
            state <- newstate
            // propagate pulse to all outputs
            [ for pulse in newpulses do
                for output in outputs do
                    output, name, pulse  ] 
        ,fun () -> Some state



module Conjunction =
    let run (input,pulse) s =
        // store input pulse to remember its value
        let news = Map.add input pulse s
        // output low pulse if all inputs are low, otherwise output high
        [ news |> Map.forall (fun _ m -> m) |> not ], news

    let create name inputs outputs =
        // initialize all inputs to low
        let mutable state = Map.ofList [ for input in inputs -> input, false]
        // printfn "& %s %A %A" name inputs outputs
        fun (input, pulse ) ->
            let newpulses, newstate = run (input,pulse) state
            state <- newstate
            // propagate pulse to all outputs
            [ for pulse in newpulses do
                for output in outputs do
                    output, name, pulse  ]
        ,fun () -> Some (Map.forall (fun _ m -> m) state)
    
module Broadcaster =
    let run (input,pulse) () = [pulse],()  // just output input pulse, no state
    let create name inputs outputs =
        // printfn "broadcast %A %A" inputs outputs 
        fun (input: string, pulse: bool ) ->
            let newpulses, _ = run (input,pulse) ()
            // propagate pulse to all outputs
            [ for pulse in newpulses do
                for output in outputs do
                    output, name, pulse  ]
        , fun _ -> None

// some checks
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

// pase lines
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

// parse a line as a module definition (symbol, name, outputs)
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

// read all modules definitions
let modulesDef = lines |> Array.map parseLine

// creates a map of inputs from the definitions
let inputs =
    [ for _,name, outputs in modulesDef do
        // yield that output has name as input
        for output in outputs do
            output, name
    ] |> List.groupBy fst
    |> List.map (fun (k,l) -> k, List.map snd l)
    |> Map.ofList

// prepare module functions
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

// run pulses through modules, conting low and high pulses
let rec run (queue: Queue<string*string*bool>) low high =
    if queue.Count = 0 then
        // no more pulses, return the result
        low, high
    else
        // there is a pulse for module name, comming from input
        let name, input, pulse = queue.Dequeue()
        // update counters
        let newLow, newHigh =
            match pulse with
            | true -> low, high + 1L
            | false -> low+1L, high
        // find the module
        match Map.tryFind name modules1 with
        | Some(md,_) -> 
            // pas the pulse to the module and add
            // output pulse to the queue
            for (next, m, p) in md(input,pulse) do
                // printfn $"{name} -> {p} -> {next}"   
                queue.Enqueue(next,m,p)
        | None -> () // no module, this is the output
        
        // move on to next pulse
        run queue newLow newHigh

// run the modules once
let runOnce() =
    // create a pulse queue with a low pulse from button to broadcaster
    let queue = Queue<string*string*bool>()
    queue.Enqueue("broadcaster","button",false)
    run queue 0L 0L

// run n times
let rec runN n low high =
    if n = 0 then
        low*high
    else
        let l,h = runOnce()
        runN (n-1) (l+low) (h+high)

// run 1000 times
runN 1000 0L 0L
 
// Part 2

// the module definitions shows that rx relyes on a conjunctions
// whose input are independent.
// Each of the input trigger a high pulse at regular intervals
// Find each interval length then multiply them (they are mutually primes, otherwise a lowest common denominator should do)

// find the first high pulse on a specific output module, result is an option
// if indicates if a high pulse was triggered, and which 

let rec run2 modules2 (queue: Queue<string*string*bool>) output result =
    if queue.Count = 0 then
        // done, was there a pulse ?
        result
    else
        // module name, recieves a pulse from input
        let name, input, pulse = queue.Dequeue()
        
        let result =
            if input = output && pulse = true then
                // this is a high pulse, and this is the module we're looking for
                // found
                Some true
            else
                // nope not yet
                result
        // run the pulse through the module
        match Map.tryFind name modules2 with
        | Some(md,_) -> 
            for (next, m, p) in md(input,pulse) do
                // printfn $"{name} -> {p} -> {next}"   
                queue.Enqueue(next,m,p)
        | None -> ()

        // continue
        run2 modules2 queue output result

// run onces on modules2 expecting a high pulse on output
let run2Once modules2 output state =
    let queue = Queue<string*string*bool>()
    queue.Enqueue("broadcaster","button",false)

    run2 modules2 queue output state

// push button as long as no input pulse is triggered
let rec run2N modules2 n output =
    // push once
    let changes = run2Once modules2 output None
    match changes with
    | Some true ->
        // we had an ight pulse, great
        int64 n
    | _ ->
        // no high pulse, push again
        run2N modules2 (n+1) output 

// get the output
let source = inputs["rx"] |> List.exactlyOne
// find  conjunction sources
let sources = inputs[source]

// find cycle length for each conjuction input and multiply them
[ for s in sources do
    run2N (createModules()) 1 s ]
|> List.fold ( * ) 1L
