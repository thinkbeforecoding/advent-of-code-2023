
open System

let input = IO.File.ReadAllLines "./input/day04.txt"

let parse (line: string) =
    match line.Split(':') with
    | [| gamestr; nums|] ->
        let id = int (gamestr.Split(' ', StringSplitOptions.RemoveEmptyEntries)[1])
        match nums.Split('|') with
        | [| win; actual |] ->
            let wins = 
                win.Split(' ', StringSplitOptions.RemoveEmptyEntries)
                |> Array.map int
                |> set
            let actuals =
                actual.Split(' ', StringSplitOptions.RemoveEmptyEntries)
                |> Array.map int
                |> set
            id, (wins, actuals)
        | _ -> failwith ""
    | _ -> failwith ""


let score (_,(win: int Set, actual: int Set)) =
    let count = Set.intersect win actual |> Set.count
    if count >= 1 then
        1 <<< (count-1)
    else
        0
input |> Array.map parse |> Array.sumBy score
    



(Map.ofList [for i in 1 .. input.Length -> i,1L], Array.map parse input)
||> Array.fold (fun map (id,(win, actual)) -> 
        let count = map[id]
        let winCards = Set.intersect win actual |> Set.count 
        if winCards > 0 then
            let newCards = 
                (map,[ for i in id+1 .. id+winCards -> i])
                ||> List.fold (fun m  k -> 
                    match Map.tryFind k m with
                    | Some v -> Map.add k (v+count) m
                    | None -> Map.add k count m
                    ) 
            newCards
        else
            map

        )
|> Map.values |> Seq.sum 