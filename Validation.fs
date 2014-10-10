module Validation.Validation

type Result<'a, 'b> =
    private
    | S of 'a
    | F of 'b * 'b list

let (|Success|Failure|) = function
    | S a -> Success a
    | F (err, rest) -> Failure (err :: rest)

module private Seq =
    let partitioned f xs =
        Array.foldBack (fun v (first, second) ->
            match f v with
            | Choice1Of2 x -> x :: first, second
            | Choice2Of2 x -> first, x :: second
        ) (Seq.toArray xs) ([], [])

module R =
    let succeed value = S value
    let fail error = F (error, [])

    let map f = function
        | S x -> succeed (f x)
        | F (error, rest) -> F (error, rest)

    let mapError f = function
        | S x -> succeed x
        | F (error, rest) -> F (f error, List.map f rest)

    let bind f = function
        | S x -> f x
        | F (error, rest) -> F (error, rest)

    let sequence results =
        let successes, failures = Seq.partitioned (|Success|Failure|) results
        match List.concat failures with
        | [] -> succeed successes
        | err :: errors -> F (err, errors)

    type Builder() =
        member __.Bind(result, f) = bind f result
        member __.Return value = succeed value
        member __.ReturnFrom result = result

let result = R.Builder()

let test = Result.fail "Shit"

