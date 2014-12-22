#r @"bin\Validation.dll"

let q : Result<string, _> =
    result {
        let! x =
            [Result.fail "test"; Result.fail "toto"]
            |> Result.concat
        return! Result.fail "test"
    }