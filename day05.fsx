open System

let input = IO.File.ReadAllLines("./input/day05.txt") |> Array.toList

module Part1 =
    /// extract seeds from first line text
    let seeds (line: string) =
        line.Substring(7).Split(' ', StringSplitOptions.RemoveEmptyEntries)
        |> Array.map(int64)
        |> set

    /// transforms a seed or not
    let transformSeed (dst,src,len) seed =
        let offset = seed - src // the offset to source
        if offset >= 0L && offset <  len then
            // the seed is in range
            // transform it to destination
            Some (dst+offset)
        else
            // the seed cannot be transformed yet
            None

    /// apply tranformation on seeds.
    /// seeds that have been transformed are moved to a new set
    let transform range (seeds, transformed) =
        ((seeds, transformed), seeds)
        ||> Set.fold (fun (ss,t) s ->
            match transformSeed range s with
            | Some d -> (Set.remove s ss, Set.add d t) // remove from src, add to dst
            | None -> ss,t )
        
    /// parse the input sequentially using mutually recursive functions
    let rec run input =
        match input with
        | s :: "" :: tail ->
            // get seeds from first line
            let seeds = seeds s
            // apply maps
            map seeds tail 

        | _ -> failwith "nope"
    and map seeds input =
        match input with
        | [] -> seeds
        | h :: tail ->
            printfn "%s" h
            // apply all mapping ranges, starting with
            // seeds, and an empty mapped set
            range (seeds, Set.empty) tail 
    and range (seeds, dests) input =
        match input with
        | [] ->
            // done, combine seeds that have not been mapped
            // with mapped seeds
            seeds+dests
        | "" :: tail ->
            // current map is done, combine seeds that have not
            // been mapped with mapped seeds
            // see if there is a next map
            map (seeds+ dests) tail
        | r :: tail ->
            // extract mapping range
            let rn = 
                match r.Split(' ') |> Array.map int64 with
                | [| dst; src; len |] -> dst, src, len
                | _ -> failwith "nope range"
            // apply mapping range
            let newSeeds = transform rn (seeds, dests)
            // move to next range
            range newSeeds tail


module Part2 =
    /// seeds are now defined using ranges with start index, and end index exclusive
    /// The total number of seeds is far too high to put seeds individually in a set
    let seeds (line: string) =
        line.Substring(7).Split(' ', StringSplitOptions.RemoveEmptyEntries)
        |> Array.map(int64)
        |> Array.chunkBySize 2
        |> Array.map (function [|s;l|] ->s,s+l | _ -> failwith "nope seeds" )
        |> set

    /// this is an empty seed range
    let empty = 0L,0L
    /// check whether a seed range is empty
    let isEmpty (s,e) = s>=e


    /// compute the intersection between two ranges
    let intersection (sx,ex) (sy,ey) =  
        let i = max sx sy, min ex ey
        if isEmpty i then empty else i

    /// returns the part of the second range that is before first range
    /// if second range starts after the first, the result is empty
    let before (sx,ex) (sy,ey) =
        intersection (0L,sx) (sy,ey)
    
    /// returns the part of the second range that is after first range
    /// if second range ends before the first, the result is empty
    let after (sx,ex) (sy,ey) =
        intersection (ex, Int64.MaxValue) (sy,ey)    



    let transformRange (dst,src,len) (s,e) =
        // the mapping source range
        let srcrn = (src,src+len)
        // split seed range in 3 parts, before, intersection and after
        let bf = before srcrn (s,e)
        let inter = intersection srcrn (s,e)
        let af = after srcrn (s,e)

        // the result is a list of before and after (parts out of range)
        // if they're not empty
        // and a list containing intersection if ranges overlap
        [ if not(isEmpty bf) then bf
          if not(isEmpty af) then af], 
        [ if not(isEmpty inter) then 
            let is, ie = inter
            let len = ie - is
            let offset = is-src
            (dst+offset,dst+offset+len)
        ]

    /// apply transformation on all ranges 
    let transform range (seeds, transformed) =
        ((seeds, transformed), seeds)
        ||> Set.fold (fun (ss,t) s ->
            // try to transform current seed range, and get
            // sub ranges that are kept as is, and part that change
            let keep, change = transformRange range s
            let kept =
                (Set.remove s ss, keep) // always remove previous range, it will be added back if there is no intersection
                ||> List.fold (fun ss s -> Set.add s ss) // add sub ranges that did not change
            let changed =
                (t, change)
                ||> List.fold (fun ss s -> Set.add s ss) // add range that changed if any

            kept, changed)

    let rec run input =
        match input with
        | s :: "" :: tail ->
            // extract seed ranges from first line
            let seeds = seeds s
            map seeds tail 

        | _ -> failwith "nope"
    and map seeds input =
        match input with
        | [] -> seeds
        | h :: tail ->
            // start of a map, all seeds are not transformed yet
            printfn "%s" h
            range (seeds, Set.empty) tail 
    and range (seeds, dests) input =
        match input with
        | [] -> 
            // combine ranges that did not changed with those that changed
            seeds+dests
        | "" :: tail ->
            // current map is done
            // combine ranges that did not changed with those that changed
            // and try to find a new map
            map (seeds+ dests) tail
        | r :: tail ->
            // get mapping from line text
            let rn = 
                match r.Split(' ') |> Array.map int64 with
                | [| dst; src; len |] -> dst, src, len
                | _ -> failwith "nope range"
            // apply transformation on all seeds
            let newSeeds = transform rn (seeds, dests)
            range newSeeds tail

    run input
    |> Set.map fst
    |> Set.minElement




