module FastResult =
    type T<'a, 'b, 'c> = ('a -> 'c) -> ('b ->'c) -> 'c

    let (|Success|Failure|) result = result Choice1Of2 Choice2Of2

    let fail error (successF: 'a -> 'c) (failureF: 'b -> 'c) = failureF error
    let succeed value (successF: 'a -> 'c) (failureF: 'b -> 'c) = successF value

    let mapResult (f: 'a -> 'd) result (successF: 'd -> 'c) (failureF: 'b -> 'c) = result (f >> successF) failureF
    let mapError f result (successF: 'd -> 'c) (failureF: 'b -> 'c) = result successF (f >> failureF)

    let bind (f: 'a -> T<'d, 'b, 'c>) (result: T<'a, 'b, 'c>) (successF: 'd -> 'c) (failureF: 'b -> 'c) =
        result (fun v ->
            f v successF failureF
        ) failureF

module SResult =
    [<Struct>]
    type Result<'a, 'b, 'c> =
        val mutable internal Apply: ('a -> 'c) -> ('b -> 'c) -> 'c
        internal new(result) = { Apply = result }

    let inline (|Success|Failure|) (result: Result<_,_,_>) = result.Apply Choice1Of2 Choice2Of2

    let inline fail error = Result(fun _ f -> f error)
    let inline succeed value = Result(fun f _ -> f value)

    let inline mapResult f (result: Result<_,_,_>) = Result(fun s -> result.Apply (f >> s))
    let inline mapError f (result: Result<_,_,_>) = Result(fun s ff -> result.Apply s (f >> ff))

    let inline bind (f: _ -> Result<_,_,_>) (result: Result<_,_,_>) =
        Result(fun s ff -> result.Apply (fun v -> (f v).Apply s ff) ff)

module Result =
    type T<'a, 'b, 'c> = private Result of (('a -> 'c) -> ('b ->'c) -> 'c)

    let inline (|Success|Failure|) (Result result) = result Choice1Of2 Choice2Of2
    
    let inline succeed value = Result (fun s _ -> s value)
    let inline fail error = Result (fun _ f -> f error)

    let inline mapResult f (Result result) = Result (fun s ff -> result (f >> s) ff)
    let inline mapError f (Result result) = Result (fun s ff -> result s (f >> ff))

    let inline bind (f: 'a -> T<'d, 'b, 'c>) (Result result) =
        Result (fun s ff ->
            result (fun v ->
                let (Result result) = f v
                result s ff
            ) ff
        )

module Validate =
    let inline (|Success|Failure|) x = x

    let inline succeed x = Choice1Of2 x
    let inline fail x = Choice2Of2 x

    let inline mapResult f = function
        | Success x -> succeed (f x)
        | Failure x -> fail x

    let inline mapError f = function
        | Success x -> succeed x
        | Failure x -> fail (f x)

    let inline bind f = function
        | Success x -> f x
        | Failure x -> fail x

let time f =
    let ts = System.Diagnostics.Stopwatch.StartNew()
    for x = 0 to 1000 do
        f ()
    ts.Elapsed.TotalSeconds
    |> printfn "%.3f"

time (fun () ->
    for x = 0 to 10000 do
        let result =
            Validate.succeed 2
            |> Validate.mapResult (fun y -> y + 3)
            |> Validate.mapResult (fun y -> y + 3)
            |> Validate.mapResult (fun y -> y + 3)
            |> Validate.mapResult (fun y -> y + 3)
            |> Validate.mapResult (fun y -> y + 3)
            |> Validate.bind (fun x -> Validate.fail "test")
            |> Validate.mapResult (fun y -> y + 3)
            |> Validate.bind (fun x -> Validate.fail "test")
            |> Validate.mapResult (fun y -> y + 3)
            |> Validate.bind (fun x -> Validate.fail "test")
            |> Validate.mapResult (fun y -> y + 3)
            |> Validate.bind (fun x -> Validate.fail "test")
            |> Validate.mapResult (fun y -> y + 3)
            |> Validate.bind (fun x -> Validate.fail "test")
            |> Validate.mapError (fun err -> ())
        match result with
        | Validate.Success x -> printfn "%A" x
        | _ -> ()
)

time (fun () ->
    for x = 0 to 10000 do
        let result =
            Result.succeed 2
            |> Result.mapResult (fun y -> y + 3)
            |> Result.mapResult (fun y -> y + 3)
            |> Result.mapResult (fun y -> y + 3)
            |> Result.mapResult (fun y -> y + 3)
            |> Result.mapResult (fun y -> y + 3)
            |> Result.bind (fun x -> Result.fail "test")
            |> Result.mapResult (fun y -> y + 3)
            |> Result.bind (fun x -> Result.fail "test")
            |> Result.mapResult (fun y -> y + 3)
            |> Result.bind (fun x -> Result.fail "test")
            |> Result.mapResult (fun y -> y + 3)
            |> Result.bind (fun x -> Result.fail "test")
            |> Result.mapResult (fun y -> y + 3)
            |> Result.bind (fun x -> Result.fail "test")
            |> Result.mapError (fun err -> ())
        match result with
        | Result.Success x -> printfn "%A" x
        | _ -> ()
)

time (fun () ->
    for x = 0 to 10000 do
        let result =
            SResult.succeed 2
            |> SResult.mapResult (fun y -> y + 3)
            |> SResult.mapResult (fun y -> y + 3)
            |> SResult.mapResult (fun y -> y + 3)
            |> SResult.mapResult (fun y -> y + 3)
            |> SResult.mapResult (fun y -> y + 3)
            |> SResult.bind (fun x -> SResult.fail "test")
            |> SResult.mapResult (fun y -> y + 3)
            |> SResult.bind (fun x -> SResult.fail "test")
            |> SResult.mapResult (fun y -> y + 3)
            |> SResult.bind (fun x -> SResult.fail "test")
            |> SResult.mapResult (fun y -> y + 3)
            |> SResult.bind (fun x -> SResult.fail "test")
            |> SResult.mapResult (fun y -> y + 3)
            |> SResult.bind (fun x -> SResult.fail "test")
            |> SResult.mapError (fun err -> ())
        match result with
        | SResult.Success x -> printfn "%A" x
        | _ -> ()
)

time (fun () ->
    for x = 0 to 10000 do
        let result =
            FastResult.succeed 2
            |> FastResult.mapResult (fun y -> y + 3)
            |> FastResult.mapResult (fun y -> y + 3)
            |> FastResult.mapResult (fun y -> y + 3)
            |> FastResult.mapResult (fun y -> y + 3)
            |> FastResult.mapResult (fun y -> y + 3)
            |> FastResult.bind (fun x -> FastResult.fail "test")
            |> FastResult.mapResult (fun y -> y + 3)
            |> FastResult.bind (fun x -> FastResult.fail "test")
            |> FastResult.mapResult (fun y -> y + 3)
            |> FastResult.bind (fun x -> FastResult.fail "test")
            |> FastResult.mapResult (fun y -> y + 3)
            |> FastResult.bind (fun x -> FastResult.fail "test")
            |> FastResult.mapResult (fun y -> y + 3)
            |> FastResult.bind (fun x -> FastResult.fail "test")
            |> FastResult.mapError (fun err -> ())
        match result with
        | FastResult.Success x -> printfn "%A" x
        | _ -> ()
)