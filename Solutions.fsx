#r "nuget:FsToolkit.ErrorHandling"
#r "nuget:FsCheck"
#r "nuget:Deedle"

open System
open System.IO
open FsToolkit.ErrorHandling
open FsCheck

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

[<AutoOpen>]
module Common = 
    /// This reusable function takes a multiline string and groups up based on whenever an empty line occurs.
    let groupByLines (data:string) =
        data.Split([| Environment.NewLine + Environment.NewLine |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map(fun group -> group.Split([| Environment.NewLine |], StringSplitOptions.RemoveEmptyEntries) |> Array.toList)
        |> Array.toList            
    type Files () =
        member _.Item
            with get file = $"data/{file}.txt"
    /// Provides access to data files using an indexer e.g. Files.[1] gets the path to the Day One data file.
    let Files = Files()

module DayOne =
    let values = File.ReadLines Files.[1] |> Seq.map int |> Seq.toArray
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
        File.ReadAllLines Files.[2]
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
    let data =
        File.ReadAllLines Files.[3]
        |> Array.map(fun line -> seq {
            while true do
                yield! line
        })

    let rideSlope (right, down) =
        let rows = List.indexed [ 0 .. down .. (data.Length - 1) ]
        [ for (rowNumber, row) in rows do
            let xCoordinate = rowNumber * right
            (rowNumber, row, xCoordinate), data.[row] |> Seq.item xCoordinate ]
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
        match text |> Seq.tryFindIndex (Char.IsDigit >> not) with
        | Some splitPosition ->
            let numbers = text.[..splitPosition - 1]
            let measure = text.[splitPosition..]
            HasHeight (numbers, measure)
        | None ->
            HasHeight (text, "")
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
        |> List.map (fun group ->
            group
            |> parseFile parser
            |> Result.bind validate)

    let countOk data = data |> List.filter Result.isOk |> List.length

    // Part one
    parseFiles PassportField.ParseSimple |> countOk

    // Part two
    parseFiles PassportField.Parse |> countOk

module DayFive =
    let data = File.ReadAllLines Files.[5]
    let (|LowerHalf|UpperHalf|) = function
        | 'F' | 'L' -> LowerHalf
        | 'B' | 'R' -> UpperHalf
        | x -> failwith $"Bad indicator {x}!"

    let bsp max indicators =
        let row =
            ({| Min = 0; Max = max |}, indicators)
            ||> Seq.fold(fun row indicator ->
                let diff = (row.Max - row.Min) / 2 + 1
                match indicator with
                | LowerHalf -> {| row with Max = row.Max - diff |}
                | UpperHalf -> {| row with Min = row.Min + diff |})
        row.Min

    let findSeat (seatData:string) =
        let row = bsp 127 seatData.[..6]
        let column = bsp 7 seatData.[7..]
        {| Id = row * 8 + column; Row = row; Column = column |}
        
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
    let data = File.ReadAllText Files.[6]

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

module DaySeven =
    let (|Words|) (text:string) =
        Words(List.ofArray (text.Trim().Split([|' '|], StringSplitOptions.RemoveEmptyEntries)))
    let (|Number|_|) =
        Int32.TryParse >> function true, n -> Some (Number n) | false, _ -> None
    let parse (line:string) =
        match List.ofArray (line.Split([| "contain" |], StringSplitOptions.RemoveEmptyEntries)) with
        | [ Words [ description; colour; "bags" ] ; children ] ->
            let children =
                children.Split ','
                |> Array.map (fun parts -> parts.Trim())
                |> Array.toList
                |> List.choose(function
                    | Words [ Number count; description; colour; _ ] -> Some ($"{description} {colour}", count)
                    | "no other bags." -> None
                    | p -> failwithf "Unknown pattern %s" p)
                |> Map
            $"{description} {colour}", children
        | _ ->
            failwith "oops"

    let lookup = File.ReadAllLines Files.[7] |> Array.map parse |> Map

    // Part One
    let calculate target (lookup:Map<_,_>) = 
        let rec walk items =
            if items |> Map.containsKey target then true
            elif Map.isEmpty items then false
            else
                items
                |> Seq.map(fun child -> walk lookup.[child.Key])
                |> Seq.exists ((=) true)
        walk

    lookup
    |> Map.filter (fun _ -> calculate "shiny gold" lookup)
    |> Map.count

    // Part Two
    let calculate (lookup:Map<_,_>) (name, count) = 
        let rec walk numberOfBags children =
            if Map.isEmpty children then numberOfBags
            else
                let children =
                    children
                    |> Map.toSeq
                    |> Seq.sumBy(fun (name, numberOfChildren) ->
                        walk (numberOfBags * numberOfChildren) lookup.[name])
                numberOfBags + children
        walk count lookup.[name]

    lookup.["shiny gold"]
    |> Map.toSeq
    |> Seq.sumBy (calculate lookup)

module DayEight =
    let (|Number|_|) (text:string) = Int32.TryParse text |> function true, n -> Some (Number n) | false, _ -> None
    type Command =
        | Acc of int | Jmp of int | Nop of int
        static member Parse (text:string) =
            match Array.toList (text.Split ' ') with
            | [ "acc"; Number number ] -> Acc number
            | [ "jmp"; Number number ] -> Jmp number
            | [ "nop"; Number number ] -> Nop number
            | x -> failwithf "Bad input %A" x

    let commands = File.ReadAllLines Files.[8] |> Array.map Command.Parse |> Array.toList
    
    let rec run (acc, cmdIndex, executedCommands:int Set) (program:Command list) =
        if executedCommands.Contains cmdIndex then Error acc
        elif cmdIndex = program.Length then Ok acc
        else
            let acc, cmdIndex =
                match program.[cmdIndex] with
                | Acc value -> acc + value, cmdIndex + 1
                | Nop _ -> acc, cmdIndex + 1
                | Jmp value -> acc, cmdIndex + value
            program |> run (acc, cmdIndex, executedCommands.Add cmdIndex)

    // Part One
    commands |> run (0, 0, Set.empty)

    // Part Two
    let replace = function Jmp x -> Nop x | Nop x -> Jmp x | other -> other
    let modifyList commands index = commands |> List.map(fun (i, cmd) -> if i = index then replace cmd else cmd)

    let findValidProgram commands =
        let commands = commands |> List.indexed

        commands
        |> Seq.filter(snd >> function Jmp _ | Nop _ -> true | _ -> false)
        |> Seq.map (fst >> modifyList commands >> run (0, 0, Set.empty))
        |> Seq.skipWhile Result.isError
        |> Seq.tryHead

    findValidProgram commands

module DayNine =
    let data =
        Files.[9]
        |> File.ReadAllLines
        |> Array.map int64

    // Part One
    let windowSize = 25
    let result =
        data
        |> Array.windowed (windowSize + 1)
        |> Array.map(fun window ->
            let head = window.[windowSize]
            let tail = window.[..windowSize - 1]
            let combinations = [
                for a in tail do
                for b in tail do
                    if a + b = head then a, b
            ]
            match combinations with
            | [] -> Error head
            | combinations -> Ok (head, combinations))
        |> Array.choose (Result.fold (fun _ -> None) (fun e -> Some e))
        |> Array.head

    // Part Two
    let maxWindow = data.Length - 1
    seq {
        for windowSize in 2 .. maxWindow do
            yield!
                data
                |> Array.windowed windowSize
                |> Array.tryFind (fun window -> Array.sum window = result)
                |> Option.toList
    }
    |> Seq.tryHead
    |> Option.map(fun answer -> Array.min answer + Array.max answer)