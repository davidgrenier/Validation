namespace Validation

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

    let singleton (error: 'b) (x, xs) =
        match xs with
        | [] -> Success x
        | _ -> Failure error

    let matches pattern error input =
        match System.Text.RegularExpressions.Regex.Match(input, pattern) with
        | m when m.Success -> Success m.Value
        | _ -> Failure error
        
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
        (Seq.toArray results, Success [])
        ||> Array.foldBack (fun result results ->
            match result, results with
            | Success x, Success xs -> Success (x :: xs)
            | Success _, Failure errors -> Failure errors
            | Failure error, Success _ -> Failure [error]
            | Failure error, Failure errors -> Failure (error :: errors)
        )

    let ofOption error value =
        match value with
        | None -> Failure error
        | Some x -> Success x

    let toOption result = unwrap Some (fun _ -> None) result

    let get result = unwrap id failwith result

    let isSuccess result = unwrap (fun _ -> true) (fun _ -> false) result
    let isFailure result = not (isSuccess result)

    let filter predicate error result = bind (Validate.Is predicate error) result
    let isnt predicate error result = bind (Validate.isnt predicate error) result
    let ensure predicate error result = bind (Validate.ensure predicate error) result
    let prevent predicate error result = bind (Validate.prevent predicate error) result
    let notEmpty error result = bind (Validate.notEmpty error) result
    let single error result = bind (Validate.single error) result
    let singleton error result = bind (Validate.singleton error) result
    let matches pattern error result = bind (Validate.matches pattern error) result
    
[<ReflectedDefinition>]
module Success =
    let map f = Result.map f id
    let ignore result = map ignore result
    
[<ReflectedDefinition>]
module Failure =
    let map f = Result.map id f

[<AutoOpen; ReflectedDefinition>]
module TopLevel =
    let succeed value = Success value
    let fail error = Failure error

    let (|Success|Failure|) result =
        match result with
        | Success x -> Success x
        | Failure x -> Failure x
    
    let inline equalTo x y = x = y
    let True x : bool = x
    let False = not
    let inline Zero x = x = LanguagePrimitives.GenericZero
    let inline One x = x = LanguagePrimitives.GenericOne
    let empty = Seq.isEmpty
    let tryCatch f arg =
        try f arg |> succeed
        with ex -> fail (ex.Message, ex)

    type ResultBuilder internal () =
        member __.Return value = succeed value
        member __.ReturnFrom (result: Result<_,_>) = result
        member __.Bind (result, f) = Result.bind f result
        member __.Bind (result, f) = Result.bind (f >> Failure.map (fun x -> [x])) result
        member __.Bind (result, f) =
            result
            |> Failure.map (fun x -> [x])
            |> Result.bind f

    type ReaderBuilder internal () =
        member x.ReturnFrom (reader: _ -> Result<_, _>) = reader
        member x.ReturnFrom (result: Result<_, _>) = fun _ -> result
        member x.Return v = fun _ -> succeed v
        member x.Bind (result: Result<_, _>, f) = x.Bind((fun _ -> result), f)
        member x.Bind (reader: 'context -> Result<'a, 'b>, f: 'a -> 'context -> Result<'c, 'b>) =
            fun context ->
                reader context
                |> Result.bind (fun s -> f s context)

    let result = ResultBuilder()
    let reader = ReaderBuilder()