namespace Validation

type Result<'a, 'b> =
    private
    | S of 'a
    | F of 'b * 'b list

[<AutoOpen>]
module TopLevel =
    let (|Success|Failure|) = function
        | S a -> Success a
        | F (err, rest) -> Failure (err :: rest)
    
    let (>>=) result f =
        match result with
        | S x -> f x
        | F (error, rest) -> F (error, rest)

    type ResultBuilder internal () =
        member __.Return value = S value
        member __.ReturnFrom result = result
        member __.Bind(result, f) = result >>= f

    let result = ResultBuilder()