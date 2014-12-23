#r @"bin\Validation.dll"

let q : Result<string, _> =
    result {
        let! x =
            [fail "test"; fail "toto"]
            |> Result.concat
        return! fail "test"
    }

let d : Choice<int,_> =
    Validate.parse "33"
    |> (|Success|Failure|)