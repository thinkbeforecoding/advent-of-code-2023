let lines = System.IO.File.ReadAllLines("./input/day07.txt")
// let lines = 
//     """32T3K 765
// T55J5 684
// KK677 28
// KTJJT 220
// QQQJA 483""".Split("\n")


type Card = 
    | A
    | K
    | Q
    | J
    | T
    | Nine
    | Height 
    | Seven
    | Six 
    | Five
    | Four 
    | Three 
    | Two

module Card =
    let parse = function
        | 'A' -> A
        | 'K' -> K
        | 'Q' -> Q
        | 'J' -> J
        | 'T' -> T
        | '9' -> Nine
        | '8' -> Height 
        | '7' -> Seven
        | '6' -> Six 
        | '5' -> Five
        | '4' -> Four 
        | '3' -> Three 
        | '2' -> Two
        | _ -> failwith "Unknown card"
        
let parseCards cards = cards |> Seq.map Card.parse |> Seq.toList


let parseLine (line: string)=
    match line.Split(' ') with
    | [| cards; bid  |] ->
        parseCards cards, System.Int64.Parse bid
    | _ -> failwith "Nope"

type Hand =
    | FiveOfAKind 
    | FourOfAKind
    | FullHouse
    | ThreeOfKind
    | TwoPairs
    | OnePair
    | HighCard 

let matchHand (cards: Card list) =
    match cards |> List.countBy id |> List.sortByDescending snd with
    | [ _,5 ] -> FiveOfAKind
    | [ _,4; _, 1] -> FourOfAKind
    | [ _, 3; _, 2] -> FullHouse
    | [ _,3; _,1; _,1 ] -> ThreeOfKind
    | [ _,2; _,2; _, 1] -> TwoPairs
    | [_,2;_,1;_,1;_,1]  -> OnePair
    | [_,1;_,1;_,1;_,1;_,1] ->  HighCard
    | _ -> failwith "invalid hand"
    , cards

lines |> Array.map parseLine  
|> Array.map (fun (cards, bid) -> matchHand cards, bid )
|> Array.sortByDescending fst
|> Array.mapi (fun i (_, bid) ->  (bid*(int64 (i+1))))
|> Array.sum

let mapCard = function
    | A -> 0
    | K -> 1
    | Q -> 2
    | T -> 3
    | Nine -> 4
    | Height  -> 5
    | Seven -> 6
    | Six  -> 7
    | Five -> 8
    | Four  -> 9
    | Three  -> 10
    | Two -> 11
    | J -> 12
        

let matchHand2 (cards: Card list) =
    let counts =  cards |> List.countBy id 
    let fixedCount = 
        let map = Map.ofList counts
        let jockers = Map.tryFind J map |> Option.defaultValue 0
        match Map.remove J map |> Map.toList |> List.sortByDescending snd with
        | (card, count) :: tail -> (card, count+jockers) :: tail
        | [] -> [A, 5]
    match fixedCount with
    | [ _,5 ] -> FiveOfAKind
    | [ _,4; _, 1] -> FourOfAKind
    | [ _, 3; _, 2] -> FullHouse
    | [ _,3; _,1; _,1 ] -> ThreeOfKind
    | [ _,2; _,2; _, 1] -> TwoPairs
    | [_,2;_,1;_,1;_,1]  -> OnePair
    | [_,1;_,1;_,1;_,1;_,1] ->  HighCard
    | _ -> failwith "invalid hand"
    , (cards |> Seq.map mapCard |> Seq.toArray)    
        

        
lines |> Array.map parseLine  
|> Array.map (fun (cards, bid) -> matchHand2 cards, bid )
|> Array.sortByDescending fst
|> Array.mapi (fun i (_, bid) ->  (bid*(int64 (i+1))))
|> Array.sum      

    
    
let cards = parseCards "KTJJT"

