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

    let matches pattern error input =
        match input with
        | null -> fail error
        | _ ->
            match System.Text.RegularExpressions.Regex.Match(input, pattern) with
            | m when m.Success -> succeed m.Value
            | _ -> fail error

module Result =
    let inline parse result = Result.bind Validate.parse result
    let matches pattern error result = Result.bind (Validate.matches pattern error) result

module Failure =
    let concatLines (result: Result<unit list, _>) =
        result
        |> Success.ignore
        |> Failure.map (String.concat System.Environment.NewLine)

[<AutoOpen>]
module TopLevel =
    let inline Zero x = x = LanguagePrimitives.GenericZero
    let inline One x = x = LanguagePrimitives.GenericOne
    let inline negative x = x < LanguagePrimitives.GenericZero