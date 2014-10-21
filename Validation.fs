namespace Validation

module private Seq =
    let partitioned f xs =
        Array.foldBack (fun v (first, second) ->
            match f v with
            | Choice1Of2 x -> x :: first, second
            | Choice2Of2 x -> first, x :: second
        ) (Seq.toArray xs) ([], [])

module Result =
    let succeed value = S value
    let fail error = F (error, [])

    let map f = function
        | S x -> succeed (f x)
        | F (error, rest) -> F (error, rest)

    let mapError f = function
        | S x -> succeed x
        | F (error, rest) -> F (f error, List.map f rest)

    let sequence results =
        let successes, failures = Seq.partitioned (|Success|Failure|) results
        match List.concat failures with
        | [] -> succeed successes
        | err :: errors -> F (err, errors)