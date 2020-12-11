#load "Bootstrapper.fsx"

open System
open System.IO

let test =
    "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL"

type SeatState =
    | Occupied
    | Unoccupied
    | Floorspace
    static member Parse = function
        | 'L' -> Unoccupied
        | '.' -> Floorspace
        | '#' -> Occupied
        | x -> failwith $"unknown space type '{x}'"
    override this.ToString() =
        match this with
        | Occupied -> "#" | Floorspace -> "." | Unoccupied -> "L"
fsi.AddPrinter<SeatState> string
fsi.AddPrinter<SeatState [][]>(fun s ->
    [ for row in s do
        [ for seat in row do string seat ]
        |> String.concat "" 
    ] |> String.concat "\n")

let data = Files.[11] |> File.ReadAllLines |> Array.map (fun s -> s |> Seq.map SeatState.Parse |> Seq.toArray)
let width = data.[0].Length
let length = data.Length

let (|AtLeast|_|) x y = if y >= x then Some AtLeast else None
let (|Adjacent|) (seatingPlan:_ [][]) (targetSeat, targetRow) =
    let occupied =
        [ for rowNumber in [ targetRow - 1; targetRow; targetRow + 1 ] do
          for seatNumber in [ targetSeat - 1; targetSeat; targetSeat + 1 ] do
            rowNumber, seatNumber ]
        |> List.filter ((<>) (targetRow, targetSeat))
        |> List.filter (fun (_, seatNumber) -> seatNumber >= 0 && seatNumber < width)
        |> List.filter (fun (rowNumber, _) -> rowNumber >= 0 && rowNumber < length)
        |> List.sumBy(fun (rowNumber, seatNumber) ->
            match seatingPlan.[rowNumber].[seatNumber] with
            | Unoccupied | Floorspace -> 0
            | Occupied -> 1)
    Adjacent occupied

let evolve (seats:SeatState[][]) = [|
    for rowNumber, row in Array.indexed seats do [|
        for seatNumber, seat in Array.indexed row do
            match seat, (seatNumber, rowNumber) with
            | Unoccupied, Adjacent seats 0 -> Occupied
            | Occupied, Adjacent seats (AtLeast 4) -> Unoccupied
            | state, _ -> state |]
    |]

data
|> Seq.unfold (fun state ->
    let nextState = evolve state
    if nextState = state then None
    else Some (nextState, nextState))
|> Seq.last
|> Array.collect id
|> Array.filter ((=) Occupied)
|> Array.length

