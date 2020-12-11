#load "Bootstrapper.fsx"

open System

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

let (|Occupied|Empty|Floorspace|) = function
    | 'L' -> Empty
    | '.' -> Floorspace
    | '#' -> Occupied
    | x -> failwith $"unknown space type '{x}'"

let raw = test.Split '\n' |> Array.map Seq.toArray
let width = raw.[0].Length
let length = raw.Length
let seats = Array2D.init width length (fun x y -> raw.[x].[y])

let (|AtLeast|_|) x y = if y >= x then Some AtLeast else None
let (|Adjacent|) (seatingPlan:_ [,]) (x,y) =
    let occupied =
        [ for x in [ x - 1; x; x + 1 ] do
          for y in [ y - 1; y; y + 1 ] do
            x,y ]
        |> List.filter (fun (x, _) -> x >= 0 && x < width)
        |> List.filter (fun (_, y) -> y >= 0 && y < width)
        |> List.sumBy(fun (x,y) ->
            match seatingPlan.[int x, int y] with
            | Empty | Floorspace -> 0
            | Occupied -> 1)
    Adjacent occupied

let y = [
    for x in [ 0 .. width - 1 ] do
    for y in [ 0 .. length - 1 ] do
        let newState =
            match seats.[x, y], (x, y) with
            | Empty, Adjacent seats 0 -> '#'
            | Occupied, Adjacent seats (AtLeast 4) -> 'L'
            | state, _ -> state
        (x, y), newState
]

let yy = Map y
Array2D.init width length (fun x y -> yy.[x, y])
