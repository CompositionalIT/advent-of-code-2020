[<AutoOpen>]
module Common

#r "nuget:FsToolkit.ErrorHandling"
#r "nuget:FsCheck"
#r "nuget:Deedle"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

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
