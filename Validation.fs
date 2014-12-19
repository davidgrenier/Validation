namespace Validation

type Result<'a, 'b> =
    private
    | Success of 'a
    | Failure of 'b list

module Validate =
    let Is predicate error = function
        | value when predicate value -> Success value
        | _ -> Failure [error]

    let ensure predicate error = function
        | value when predicate value -> Success ()
        | _ -> Failure [error]

    let isnt predicate = Is (predicate >> not)
    let prevent predicate = ensure (predicate >> not)

module Result =
    let succeed value = Success value
    let fail error = Failure [error]

    let unwrap s f = function
        | Success x -> s x
        | Failure x -> f x

    let map s f = unwrap (s >> succeed) (List.map f >> Failure)
    let bind f = unwrap f Failure

    let concat results =
        (Seq.toArray results, Success [])
        ||> Array.foldBack (fun result results ->
            match result, results with
            | Success x, Success xs -> Success (x :: xs)
            | Success _, Failure errors | Failure errors, Success _ -> Failure errors
            | Failure xs, Failure es -> Failure (xs @ es)
        )

    let private (=>) predicate f error = bind (f predicate error)

    let Is predicate error = bind (Validate.Is predicate error)
    let isnt predicate error = bind (Validate.isnt predicate error)
    let ensure predicate error = bind (Validate.ensure predicate error)
    let prevent predicate error = bind (Validate.prevent predicate error)

module Success =
    let map f = Result.map f id

module Failure =
    let map f = Result.map id f

module Aliased =
    module V = Validate
    module R = Result

[<AutoOpen>]
module TopLevel =
    let (|Success|Failure|) = function
        | Success a -> Success a
        | Failure errors -> Failure errors
    
    let equalTo = (=)
    let True x : bool = x
    let False = not
    let inline Zero x = x = LanguagePrimitives.GenericZero
    let inline One x = x = LanguagePrimitives.GenericOne

    type ResultBuilder internal () =
        member __.Return value = Result.succeed value
        member __.ReturnFrom (result: Result<_,_>) = result
        member __.Bind (result, f) = Result.bind f result

    let result = ResultBuilder()