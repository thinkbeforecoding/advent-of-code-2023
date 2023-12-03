open System
open System.Text.RegularExpressions

// keep the input as a single string
let input = IO.File.ReadAllText("./input/day03.txt")
// this is the width of the table 
let width = input.IndexOf(Environment.NewLine)
// this is the offset to go to next line, including newline chars
let nextLineOffset = width + Environment.NewLine.Length
let height = (input.Length + Environment.NewLine.Length) / nextLineOffset

[<Struct>]
type Pos = { X: int; Y: int }
    with 
    static member (+) (a,b) =
        { X = a.X+b.X; Y = a.Y+b.Y} 

/// returns x,y position from index in string
let pos index =
    let struct(y,x) = Int32.DivRem( index,nextLineOffset)
    { X = x; Y = y}

/// peek an char by position, returns '.' when outside of table
let peek pos =
    if pos.X < 0 || pos.X >= width || pos.Y < 0 || pos.Y >= height then
        '.'
    else
        input[pos.Y * nextLineOffset + pos.X]

/// any char not . or digit
let isSymbol c =
    not (c = '.' || Char.IsAsciiDigit c)

/// peek at position plus offset, and check whether it's a symbol
let isSymbolAt p offset = isSymbol (peek (p+offset))

/// vector offsets for the 8 positions around a point
let offsets =
    [| for y in -1 .. 1 do
            for x in -1 .. 1 do
                if x <> 0 || y <> 0 then
                    { X = x; Y = y}
    |]


/// check if any char arround p is a symbol
let isPartDigit p =
    offsets 
    |> Array.exists (isSymbolAt p)


// all numbers in the table. Since we kepts newlines, there is no risk
// if a line ends with a number, and the next one starts also with a number
let numbers = Regex.Matches(input, @"\d+", RegexOptions.Multiline)

// Part 1
numbers
|> Seq.choose (fun m -> 
    let isPart =
        Seq.init m.Length (fun i -> pos (i+m.Index)) // positions of each digit in number
        |> Seq.exists isPartDigit                    // check if could be a part
    if isPart then
        Some (Int32.Parse m.ValueSpan)               // this is a part, return its number
    else
        None                                         // not a part
     )
|> Seq.toList
|> Seq.sum
     

// For part 2, we maintain a map of parts associated with a gears,
// this is a Map<Pos,Set<Pos * int>>
// The key is the position of the gear, making it easy to gather parts for the same gear
// the Set contains Position of the first digit of the part, and the value of the part.
// Using only the value in the set is risky, because a gear could be next to two parts
// with the same value at different positions.

/// check chars around p to find positions of all gears
/// returns a list in case there could be several
let findGears p =
    offsets
    |> Array.filter (fun o -> peek (p+o) = '*')
    |> Array.map (fun o -> p + o)

/// adds a part to a gear. As we use Map and Set, the same part
/// can be added multiple time without problem
let addGearPart gear part gears =
    match Map.tryFind gear gears with
    | None -> Map.add gear (Set.singleton part) gears
    | Some parts -> Map.add gear (Set.add part parts) gears


(Map.empty, numbers)
||> Seq.fold (fun gears m -> 
    let isPart =
        Seq.init m.Length (fun i -> pos (i+m.Index))
        |> Seq.exists isPartDigit

    if isPart then
        // collect all possible gears for the part
        let newGears =
            Seq.init m.Length (fun i -> pos (i+m.Index))
            |> Seq.collect findGears
            |> Seq.distinct

        // add the part for each found gear
        (gears,newGears) ||> Seq.fold (fun gs g -> addGearPart g (pos(m.Index), int m.Value) gs)
    else
        gears
     )
|> Map.toSeq 
|> Seq.choose (fun (gear, parts) -> 
    // filter gears that have exactly two parts, an compute power
    if Set.count parts = 2 then
        let power = parts |> Seq.fold (fun acc (_,v) -> acc * v) 1
        Some power
    else
        None
)
|> Seq.sum
     
    