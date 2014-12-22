#r @"bin\Validation.dll"

open Validation

let q : Result<string, _> =
    result {
        let! x =
            [Result.fail "test"; Result.fail "toto"]
            |> Result.concat
        return! Result.fail "test"
    }