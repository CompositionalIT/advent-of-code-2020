#r "nuget:FsToolkit.ErrorHandling"

open System
open System.IO
open FsToolkit.ErrorHandling

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

[<AutoOpen>]
module Common = 
    /// This reusable function takes in rows of text and groups up based on whenever an empty line occurs.
    let groupByLines (data:string) =
        data.Split([| Environment.NewLine + Environment.NewLine|], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map(fun group -> group.Split([|Environment.NewLine|], StringSplitOptions.RemoveEmptyEntries) |> Array.toList)
        |> Array.toList            

module DayOne =
    let values = File.ReadLines "DayOne.txt" |> Seq.map int |> Seq.toArray
    let findAndProduct year items =
        items
        |> Seq.tryFind(List.sum >> (=) year)
        |> Option.map(List.reduce (*))
    
    // Part one
    seq {
      for a in values do
      for b in values do
        [ a; b ] }
    |> findAndProduct 2020

    // Part two
    seq {
      for a in values do
      for b in values do
      for c in values do
        [ a; b; c ] }
    |> findAndProduct 2020

module DayTwo =
    let data =
        File.ReadAllLines "DayTwo.txt"
        |> Array.map(fun line ->
            let items = line.Split ' '
            let range = items.[0].Split '-'
            int range.[0], int range.[1], items.[1].[0], items.[2])

    // Part 1
    let isValid (min, max, letter, password:string) =
        let occurrences = password |> Seq.filter ((=) letter) |> Seq.length
        occurrences >= min && occurrences <= max

    data |> Array.filter isValid |> Array.length 

    // Part 2
    let isValidAlt (first, second, letter, password:string) =
        match password.[first - 1] = letter, password.[second - 1] = letter with
        | true, false
        | false, true ->
            true
        | _ ->
            false

    data |> Array.filter isValidAlt |> Array.length

module DayThree =
    let parsed =
        File.ReadAllLines "DayThree.txt"
        |> Array.map(fun line -> seq {
            while true do
                yield! line
        })

    let rideSlope (right, down) =
        let rows = List.indexed [ 0 .. down .. (parsed.Length - 1) ]
        [ for (rowNumber, row) in rows do
            let xCoordinate = rowNumber * right
            (rowNumber, row, xCoordinate), parsed.[row] |> Seq.item xCoordinate ]
        |> List.filter(snd >> (=) '#')
        |> List.length
        |> int64

    // Part 1
    rideSlope (3, 1)

    // Part 2
    [ 1, 1
      3, 1
      5, 1
      7, 1
      1, 2 ]
    |> List.map rideSlope
    |> List.reduce (*)

module DayFour =
    type PassportType = Normal | NorthPole
    let (|Digits|_|) length (text:string) =
        if text.Length = length && text |> Seq.forall Char.IsDigit then Some Digits
        else None
    let (|AtLeast|_|) min (text:string) = if int text >= min then Some AtLeast else None
    let (|AtMost|_|) max (text:string) = if int text <= max then Some AtMost else None
    let (|Between|_|) min max = function AtLeast min & AtMost max -> Some Between | _ -> None
    let (|NumberOrAf|_|) c =
        if Char.IsDigit c || [| 'a' .. 'f' |] |> Array.contains c then Some NumberOrAf
        else None
    let (|HasHeight|) (text:string) =
        let splitPosition = text |> Seq.findIndex (Char.IsDigit >> not)
        let numbers = text.[..splitPosition - 1]
        let measure = text.[splitPosition..]
        HasHeight (numbers, measure)
    let (|Chars|) (text:string) = Chars (Seq.toList text)
    type PassportField =
        | BirthYear
        | IssueYear
        | ExpirationYear
        | Height
        | HairColour
        | EyeColour
        | PassportId
        | CountryId
        static member ParseSimple (text:string) =
            match List.ofArray (text.Split ':') with
            | [ "byr"; _ ] -> Ok BirthYear
            | [ "iyr"; _ ] -> Ok IssueYear
            | [ "eyr"; _ ] -> Ok ExpirationYear
            | [ "hgt"; _ ] -> Ok Height
            | [ "hcl"; _ ] -> Ok HairColour
            | [ "ecl"; _ ] -> Ok EyeColour
            | [ "pid"; _ ] -> Ok PassportId
            | [ "cid"; _ ] -> Ok CountryId
            | header -> Error $"Invalid {header}"
        static member Parse (text:string) =
            match List.ofArray (text.Split ':') with
            | [ "byr"; Digits 4 & Between 1920 2002 ] -> Ok BirthYear
            | [ "iyr"; Digits 4 & Between 2010 2020 ] -> Ok IssueYear
            | [ "eyr"; Digits 4 & Between 2020 2030 ] -> Ok ExpirationYear
            | [ "hgt"; HasHeight (Between 150 193, "cm" | Between 59 76, "in")  ] -> Ok Height
            | [ "hcl"; Chars [ '#'; NumberOrAf; NumberOrAf; NumberOrAf; NumberOrAf; NumberOrAf; NumberOrAf ] ] -> Ok HairColour
            | [ "ecl"; ("amb" | "blu" | "brn" | "gry" | "grn" | "hzl" | "oth") ] -> Ok EyeColour
            | [ "pid"; Digits 9 ] -> Ok PassportId
            | [ "cid"; _ ] -> Ok CountryId
            | header -> Error $"Invalid {header}"

    let data = File.ReadAllText "DayFour.txt"

    let parseFile parser lines =
        lines
        |> List.collect (fun (line:string) -> line.Split ' ' |> Array.toList)
        |> List.traverseResultA parser

    let validate (file:PassportField list) = 
        let file = Set file
        if file.Count = 8 then Ok Normal
        elif file.Count = 7 && not (file.Contains CountryId) then Ok NorthPole
        else Error [ "Bad fields" ]

    let parseFiles parser =
        data
        |> groupByLines
        |> List.map (parseFile parser)
        |> List.map (Result.bind validate)
        |> List.filter (function Ok _ -> true | Error _ -> false)
        |> List.length

    // Part one
    parseFiles PassportField.ParseSimple

    // Part two
    parseFiles PassportField.Parse

module DayFive =
    let (|LowerHalf|UpperHalf|) = function
        | 'F' | 'L' -> LowerHalf
        | 'B' | 'R' -> UpperHalf
        | x -> failwith $"Bad indicator {x}!"
    let bsp max indicators =
        ({| Min = 0; Max = max |}, indicators)
        ||> Seq.fold(fun row indicator ->
            let diff = (row.Max - row.Min) / 2 + 1
            match indicator with
            | LowerHalf -> {| row with Max = row.Max - diff |}
            | UpperHalf -> {| row with Min = row.Min + diff |})
        |> fun row -> row.Min

    let findSeat (seatData:string) =
        let row = bsp 127 seatData.[..6]
        let column = bsp 7 seatData.[7..]
        {| Id = row * 8 + column; Row = row; Column = column |}
        
    let data = File.ReadAllLines "DayFive.txt"
    let allSeats = data |> Array.map findSeat

    // Part one
    allSeats
    |> Array.maxBy(fun seat -> seat.Id)

    // Part two
    allSeats
    |> Array.sortBy(fun r -> r.Id)
    |> Array.pairwise
    |> Array.find(fun (seatA, seatB) -> seatB.Id - seatA.Id > 1)
    |> fun (seatA, _) -> seatA.Id + 1

module DaySix =
    let data = File.ReadAllText "DaySix.txt"

    let partOneData =
        data
        |> groupByLines
        |> List.map(fun group ->
            group
            |> List.collect(fun line -> line.ToCharArray() |> List.ofArray)
            |> Set)
        |> List.sumBy Set.count

    // Part Two
    let partTwoData =
        data
        |> groupByLines
        |> List.map(fun group ->
            group
            |> List.map(fun s -> s.ToCharArray())
            |> Array.concat
            |> Array.countBy id
            |> Array.filter (snd >> (=) group.Length)
            |> Array.length)
        |> List.sum