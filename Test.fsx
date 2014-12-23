#r @"bin\Validation.dll"

let q : Result<string, _> =
    result {
        let! x =
            [fail "test"; fail "toto"]
            |> Result.concat
        return! fail "test"
    }