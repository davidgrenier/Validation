namespace Validation

type Result<'a, 'b> =
    private
    | S of 'a
    | F of 'b * 'b list

module Validate =
    let Is predicate error value = if predicate value then S value else F (error, [])
    let ensure predicate error value = if predicate value then S () else F (error, [])

    let isnt predicate = Is (predicate >> not)
    let prevent predicate = ensure (predicate >> not)

module Result =
    let succeed value = S value
    let fail error = F (error, [])

    let map f = function
        | S x -> succeed (f x)
        | F (error, rest) -> F (error, rest)

    let mapError f = function
        | S x -> succeed x
        | F (error, rest) -> F (f error, List.map f rest)

    let concat results =
        (Seq.toArray results, S [])
        ||> Array.foldBack (fun result results ->
            match result, results with
            | S x, S xs -> S (x :: xs)
            | S _, F (e, es) | F (e, es), S _ -> F (e, es)
            | F (x, xs), F (o, os) -> F (x, xs @ o :: os)
        )

    let bind f = function
        | S x -> f x
        | F (error, rest) -> F (error, rest)

    let private (=>) predicate f error = bind (f predicate error)

    let Is predicate error = bind (Validate.Is predicate error)
    let isnt predicate error = bind (Validate.isnt predicate error)
    let ensure predicate error = bind (Validate.ensure predicate error)
    let prevent predicate error = bind (Validate.prevent predicate error)

[<AutoOpen>]
module TopLevel =
    let (|Success|Failure|) = function
        | S a -> Success a
        | F (err, rest) -> Failure (err :: rest)
    
    let (>>=) result f = Result.bind f result
    let equalTo = (=)
    let True x : bool = x
    let False = not
    let inline Zero x = x = LanguagePrimitives.GenericZero
    let inline One x = x = LanguagePrimitives.GenericOne

    type ResultBuilder internal () =
        member __.Return value = S value
        member __.ReturnFrom result = result
        member __.Bind(result, f) = result >>= f

    let result = ResultBuilder()