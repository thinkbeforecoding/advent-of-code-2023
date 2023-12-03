open System
open FSharp.NativeInterop
open FSharp.Core.CompilerServices
#nowarn "9"

// this version is intensive on parsing
// this refactoring is doing the parsing without string instanciation during the parsing
// it uses spans to avoid substring creations 

// this is a span based stackalloc
let inline stackAlloc<'t when 't: unmanaged>(n) =
    Span<'t>(NativePtr.toVoidPtr( NativePtr.stackalloc<'t>(n)), n)

// creates a sub span from a range
let rangeSpan (s: ReadOnlySpan<'t>, r: Range) =
    let struct(o,l) = r.GetOffsetAndLength(s.Length)
    s.Slice(o,l)

// provide it as an extension method on ReadOnlySpan
type ReadOnlySpan<'t> with
    member this.Item with get(r: Range) = rangeSpan(this, r)
    

/// does an Split on the input ReadOnlySpan, and returns a slice containing only found ranges
let inline split (s: ReadOnlySpan<char>, max: int, separator: char) =
    let buffer = stackAlloc 20
    let n = s.Split(buffer, separator, StringSplitOptions.RemoveEmptyEntries)
    buffer.Slice(0,n)


type Cube =  Blue | Red | Green

/// a span containing a color text
let parseColor (s: ReadOnlySpan<char>) =
        if s.Equals("blue".AsSpan(), StringComparison.InvariantCultureIgnoreCase) then
            Blue
        elif s.Equals("red".AsSpan(), StringComparison.InvariantCultureIgnoreCase) then
            Red
        elif s.Equals("green".AsSpan(), StringComparison.InvariantCultureIgnoreCase) then
            Green
        else
            failwith "Unknown color"

/// parse a cube like "15 blue" from a span
let parseCube (s: ReadOnlySpan<char>) =
    let parts = split(s, 2, ' ')
    if parts.Length = 2 then
        parseColor (s[parts[1]]), Int32.Parse (s[parts[0]])
    else
        failwith "Invalid cube"


/// parse cubes separated by ','
let parseSubset (s: ReadOnlySpan<char>) =
    let cubes = split(s, 20, ',')
    let mutable map = Map.empty
    for cube in cubes  do
        let color, count = parseCube(s[cube])
        map <- Map.add color count map
    map

/// parse subsets separated by ';', max 20 results are expected
let parseSubsets (s: ReadOnlySpan<char>) =
    let subsets = split(s, 20, ';')
    let  a = Array.zeroCreate subsets.Length
    let mutable i = 0
    for subset in subsets do
        a[i] <- parseSubset(s[subset])
        i <- i+1
    a

/// parse a game line
let parseGame (s: string) =
    let span = s.AsSpan()
    let parts = split(span, 2, ':')
    if parts.Length = 2 then
        let idSpan = span[parts[0]]

        let idParts = split(idSpan, 2, ' ')
        let gameId = Int32.Parse (idSpan[idParts[1]])
        let subsets = parseSubsets (span[parts[1]])
        gameId, subsets
    else
        failwith "Invalid game"


/// For part 1, checks that all colors have less that expected count  
let isPossible (total: Map<Cube,int>) (id, subsets) =
    subsets
    |> Array.forall (fun subset ->
        subset |> Map.forall(fun color n -> n <= total[color] )
    )

/// For part 2, compute the max of count for each color, this will be the minimum required to make the game possible
let minCubes (id, (subsets: Map<Cube,int>[])) =
    id,
    subsets
    |> Array.reduce (fun (acc: Map<Cube,int>) (subset: Map<Cube,int>) ->
        subset
        |> Map.fold (fun (acc: Map<Cube, int>) cube n ->
            match Map.tryFind cube acc with
            | Some m -> Map.add cube (max n m) acc 
            | None -> Map.add cube n acc) acc
            )  
    
/// Compute the power of a subset
let power (_,subset: Map<Cube,int>) =
    subset |> Map.fold(fun acc _ n -> acc * n ) 1

let input = IO.File.ReadAllLines("./input/day02.txt")

// Part 1
input
|> Array.map parseGame
|> Array.filter (isPossible (Map.ofList [ Red, 12; Green, 13 ; Blue, 14 ]))
|> Array.sumBy fst

// Part 2
input
|> Array.map parseGame
|> Array.map minCubes
|> Array.sumBy power