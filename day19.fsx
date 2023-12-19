#r "nuget: FSharp.Text.RegexProvider"
open System
open FSharp.Text.RegexProvider


let input = IO.File.ReadAllLines "./input/day19.txt"


// split rules and parts
let rulesLines = Array.takeWhile (String.IsNullOrWhiteSpace >> not) input
let partsLines = Array.skipWhile (String.IsNullOrWhiteSpace >> not) input |> Array.skip 1

// parsing parts is easy
type PartRx = Regex< @"\{x=(?<x>\d+),m=(?<m>\d+),a=(?<a>\d+),s=(?<s>\d+)\}">
let parts =
    partsLines |> Array.map (fun l ->
        let m =  PartRx().TypedMatch(l)
        int m.x.Value, int m.m.Value, int m.a.Value, int m.s.Value )

// parsing rules is just a bit more complicated
type RuleRx = Regex<  @"(?<name>\w+)\{(?<rule>[^,]+,)*(?<def>\w+)\}">
type ConditionRx = Regex< @"^(?<part>\w+)(?<condition>[<>])(?<value>\d+):(?<next>\w+),">

// this has been modified because part 2 require a different interpretation
// this will return a Map of rule name -> function
let parseRules ruleParser rulesLines =
    rulesLines |> Array.map (fun l ->
        let m = RuleRx().TypedMatch(l)
        let name = m.name.Value
        let def = m.def.Value

        let conditions =
            [ for r in m.rule.Captures do
                let m = ConditionRx().TypedMatch(r.Value)
                m.part.Value, m.condition.Value , int m.value.Value , m.next.Value ]

        name, ruleParser conditions def
    ) |> Map.ofArray

// prepare directly a function for each rule
// take a list of functions check conditions, apply them, and exists when one is 

let ruleParser conditions def  =
    let conditionsFunc =
        [ for part, condition, value, next in conditions do
                // extract the category
                let category =
                    match part with
                    | "x" -> fun (x,_,_,_) -> x
                    | "m" -> fun (_,m,_,_) -> m
                    | "a" -> fun (_,_,a,_) -> a
                    | "s" -> fun (_,_,_,s) -> s
                    | p -> failwith $"Unknown part '{p}'"
                // a function the check if the condition holds, and return the next rule otherwise none
                let cond = 
                    match condition with
                    | "<" -> fun x -> if x < value then Some next else None
                    | ">" -> fun x -> if x > value then Some next else None
                    | _ -> failwith "Unknown operation"

                // apply the condition to the category value
                category>>cond ]

    // Try to find the first condition than matches, or return default
    (fun part -> 
        conditionsFunc
        |> List.tryPick (fun c -> c part)
        |> Option.defaultValue def)

// parse all rules
let rules = parseRules ruleParser rulesLines

// run applying rules recursively
let applyRules part =
    let rec loop rule part = 
        // apply the rule an check whant is the next one
        match rules[rule] part with
        | "A" -> Some part // accepted, we keep it
        | "R" -> None   // rejected, it should not be included
        | newRule -> loop newRule part // new rule, apply it 
    loop "in" part // start with rule "in"

// sum the categories
let sum (x,m,a,s) = x+m+a+s

// filter all accepted parts and sum them
parts |> Array.choose applyRules |> Array.sumBy sum


// Part 2
// This one rely on ranges of values (low,high) with both ends included
// each rule with restrict ranges

// this is the initial range for each category
let initrange = (1,4000)
// check if a range is empty (when low = high, there is 1 item, hence the > )
let isEmpty (l,h) = l > h
// intersect two ranges
let intersect (la,ha) (lb, hb) = max la lb, min ha hb

// keep lower than n range, the 2nd part of the tuple is the rest, what did not pass the condition
let lt n  = intersect (1,n-1) , intersect (n,4000) 
// keep greater than n range, the 2nd part of the tuple is the rest, what did not pass the condition
let gt n  = intersect (n+1,4000) , intersect (1,n) 

// apply a function on a cateogry x m a or s
let mapx f (x,m,a,s) = (f x,m,a,s)
let mapm f (x,m,a,s) = (x,f m,a,s)
let mapa f (x,m,a,s) = (x,m,f a,s)
let maps f (x,m,a,s) = (x,m,a,f s)

// apply the functions f and g to a category (as the map function)
let mapboth map (f,g) p = map f p, map g p 

// the initial value, with each category to max range
let init = initrange, initrange, initrange, initrange
// we can stop when any category is empty
let stop (x,m,a,s) = isEmpty x || isEmpty m || isEmpty a || isEmpty s 

// apply rules to a part p
// returning the new ranges for each next rule, and the accepted range
// rejected parts are discareded
let rec applyRules2 p def rules =
    let rec loop p def rules result accepted =
        match rules with
        | [] -> 
            // there is no other rule, apply the default one
            if stop p then 
                // p doesn't contain any element, nothing to do
                result, accepted
            else
                // check the default rule
                match def with
                | "A" -> result, p :: accepted // the rest of the range is accepted
                | "R" -> result, accepted   // the rest of the range is rejected, forget it
                | _ -> 
                    // default is a new rule, add the remaining range to next rules to check
                    (p,def) :: result, accepted 

        | (rule,next) :: tail ->
            // apply the rul, we get the range that pass this rule
            // and the rest that does not pass this rule
            let range, rest = rule p

            if stop rest then
                // there is no rest, so no need to apply next conditions
                let r =
                    if stop range then
                        // nothing passed the condition, the result rest the same 
                        result
                    else
                        // the range that passed the condition should go through next rule
                        (range, next) :: result
                // exit with ranges to apply and accepted values
                r, accepted
            else
                // there is a rest, we will have to go through next conditions
                match next with
                | "A" ->
                    // the range that passed is accepted, add it to accepted values
                    loop rest def tail result (range :: accepted)
                | "R" ->
                    // the range that passed is rejected, forget it
                    loop rest def tail result accepted
                | _ ->
                    // the range will have to go trough the next rule,
                    // check if it's empty first
                    let r =
                        if stop range then
                            result
                        else
                            (range, next) :: result 
                
                    loop rest def tail r accepted
    loop p def rules [] [] 


// build a function that apply all conditions
let ruleParser2 conditions def =
    let rules =
        [ for part, condition, value, next in conditions do


            let mapp =
                match part with
                | "x" -> mapx 
                | "m" -> mapm
                | "a" -> mapa
                | "s" -> maps
                | _ -> failwith "Invalid part"

            let cond = 
                match condition with
                | "<" -> lt value
                | ">" -> gt value
                | _ -> failwith "Invalid condition"
            mapboth mapp cond, next
        ] 
    fun p -> applyRules2 p def rules

// parse the rules and build all functions
let rules2 = parseRules ruleParser2 rulesLines


// apply rules recursively
// input is a list of ranges with the rules to pass through
// the result is a list of accepted ranges 
let rec run input result =
    match input with
    | [] -> result // no more rules to apply, return the result
    | (part, rule) :: tail ->
        // apply the rule, we get the parts that should go to next rules
        // and the accepted ranges
        let newParts, accepted = rules2[rule] part 
        // call recursively with both new rules to apply and new accepted results
        run (newParts @ tail) (accepted @ result)
    
// count possibilities in a category range (the +1 is because end is included)
let countp (l,h) = int64(h - l + 1)
// count the possibilities for a part
let count (x,m,a,s) = countp x * countp m * countp a * countp s

// apply all rules starting from in with the maximum ranges, and count the results
run [init, "in"] [] |> List.sumBy count