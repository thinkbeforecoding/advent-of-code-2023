open System

let input = IO.File.ReadAllLines("./input/day01.txt")

// part 1 is easy, use IndexOfAny/LastIndexOfAny to spot numbers
module PartOne =
    let digits = [| '0' .. '9' |]

    let findDigits (s: string) =
        let first = s.IndexOfAny(digits)
        let last = s.LastIndexOfAny(digits)
        string s[first] + string s[last]
        |> int

    input |> Array.sumBy findDigits

// part 2 is slightly more difficult as numbers can appear as text
module PartTwo =
    // all text that must be looked up
    let digits = 
        [| for i in 1 .. 9 do string i,i
           "one", 1
           "two", 2
           "three", 3
           "four", 4
           "five", 5
           "six", 6
           "seven", 7
           "eight", 8
           "nine", 9
         |]

    /// find position and value of each digit text
    /// take the one at min position, and return value
    let findFirstDigit (s: string) =
        [ for d,v in digits do
            match s.IndexOf(d) with
            | i when i >= 0 -> i, v
            | _ -> () 
        ] |> List.minBy fst
        |> snd

    /// find last position and value of each digit text
    /// take the one at max position, and return value
    let findLastDigit (s: string) =
        [ for d,v in digits do
            match s.LastIndexOf(d) with
            | i when i >= 0 -> i, v
            | _ -> () 
        ] |> List.maxBy fst
        |> snd

    /// compute the value for first / last number 
    let findDigits s =
        findFirstDigit s * 10 + findLastDigit s

    // examples
    findDigits "two1nine"
    findDigits "eightwothree"
    findDigits "7pqrstsixteen"

    input |> Array.sumBy findDigits