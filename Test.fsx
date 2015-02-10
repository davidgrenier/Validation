#r @"bin\Validation.dll"

let q =
    result {
        for x in [1;2] do
            do! x |> Validate.ensure (fun x -> x < 2) (sprintf "Oops %d is >= 2" x)

        printfn "test"

        return "test"
    }

let d : Choice<int,_> =
    Validate.parse "33"
    |> (|Success|Failure|)