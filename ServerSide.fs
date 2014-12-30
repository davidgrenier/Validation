namespace ServerSide

open Validation

module Validate =
    let inline parse input: Result<'a, _> =
        match input with
        | null -> fail (sprintf "Cannot parse null into %s" typeof<'a>.Name)
        | _ ->
            let mutable value = Unchecked.defaultof<_>
            match (^a: (static member TryParse: string * byref< ^a> -> bool) (input, &value)) with
            | true -> succeed value
            | _ -> fail (sprintf "%s is not a %s" input typeof<'a>.Name)

module Result =
    let inline parse result = Result.bind Validate.parse result

module Failure =
    let concatLines (result: Result<unit list, _>) =
        result
        |> Success.ignore
        |> Failure.map (String.concat System.Environment.NewLine)