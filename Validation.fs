﻿namespace Validation

type Result<'a, 'b> =
    private
    | Success of 'a
    | Failure of 'b

[<ReflectedDefinition>]
module Validate =
    let Is predicate error value =
        if predicate value then Success value
        else Failure error

    let isnt predicate error value = Is (predicate >> not) error value

    let ensure predicate error value =
        if predicate value then Success ()
        else Failure error

    let prevent predicate error value = ensure (predicate >> not) error value

    let notEmpty (error: 'b) list =
        match list with
        | [] -> Failure error
        | x::xs -> Success (x, xs)

    let single (error: 'b) list =
        match list with
        | [x] -> Success x
        | _ -> Failure error

    let singleton error (x, xs: 'a list) : Result<'a, 'b> =
        match xs with
        | [] -> Success x
        | _ -> Failure error

    let matches pattern error input =
        match input with
        | null -> Failure error
        | _ ->
            match System.Text.RegularExpressions.Regex.Match(input, pattern) with
            | m when m.Success -> Success m.Value
            | _ -> Failure error

    let all predicate error xs = Seq.forall predicate xs |> ensure id error
        
[<ReflectedDefinition>]
module Result =
    let unwrap (s: 'a -> 'c) (f: 'b -> 'c) result =
        match result with
        | Success x -> s x
        | Failure x -> f x

    let recover (f: 'b -> 'a) result = unwrap id f result
    let tryRecover (f: 'b -> Result<'a, 'c>) result = unwrap Success f result

    let map s f result = unwrap (s >> Success) (f >> Failure) result
    let bind f result = unwrap f Failure result

    let concat results =
        Seq.toArray results
        |> Array.foldBack (fun result results ->
            match result, results with
            | Success x, Success xs -> Success (x :: xs)
            | Success _, Failure errors -> Failure errors
            | Failure error, Success _ -> Failure [error]
            | Failure error, Failure errors -> Failure (error :: errors)
        ) <| Success []

    let ofOption error value =
        match value with
        | None -> Failure error
        | Some x -> Success x

    let toOption result = unwrap Some (fun _ -> None) result

    let get result = unwrap id failwith result

    let isSuccess result = unwrap (fun _ -> true) (fun _ -> false) result
    let isFailure result = not (isSuccess result)

    open Validate

    let filter predicate error result = bind (Is predicate error) result
    let isnt predicate error result = bind (isnt predicate error) result
    let ensure predicate error result = bind (ensure predicate error) result
    let prevent predicate error result = bind (prevent predicate error) result
    let notEmpty error result = bind (notEmpty error) result
    let single error result = bind (single error) result
    let singleton error result = bind (singleton error) result
    let matches pattern error result = bind (matches pattern error) result
    let all predicate error xs = bind (all predicate error) xs
    
[<ReflectedDefinition>]
module Success =
    let map f result = Result.map f id result
    let ignore result = map ignore result
    
[<ReflectedDefinition>]
module Failure =
    let map f result = Result.map id f result

[<AutoOpen; ReflectedDefinition>]
module TopLevel =
    let succeed value = Success value
    let fail error = Failure error

    let (|Success|Failure|) result =
        match result with
        | Success x -> Success x
        | Failure x -> Failure x
    
    let inline equalTo x y = x = y
    let inline True x : bool = x
    let inline False x = not x
    let inline Zero x = x = LanguagePrimitives.GenericZero
    let inline One x = x = LanguagePrimitives.GenericOne
    let inline negative x = x < LanguagePrimitives.GenericZero
    let inline empty xs = Seq.isEmpty xs
    let inline tryCatch f arg =
        try f arg |> succeed
        with ex -> fail (ex.Message, ex)

    type ResultBuilder internal () =
        member __.Return value = succeed value
        member __.ReturnFrom result : Result<'a,'b> = result
        member __.Bind (result, f) = Result.bind f result
        member __.Bind (result, f) = Result.bind (f >> Failure.map (fun x -> [x])) result

    type ReaderBuilder internal () =
        member __.ReturnFrom reader : 'context -> Result<'a, 'b> = reader
        member x.ReturnFrom result : 'context -> Result<'a, 'b> = fun _ -> result
        member __.Return v : 'context -> _ = fun _ -> succeed v
        member __.Bind (reader, f: 'a -> _) : 'context -> Result<'c, 'b> =
            fun context -> reader context |> Result.bind (fun s -> f s context)
        member x.Bind (result: Result<_, _>, f) = x.Bind((fun _ -> result), f)

    let result = ResultBuilder()
    let reader = ReaderBuilder()