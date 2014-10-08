#r @"bin\Release\Validation.dll"

open Validation

module OResult =
    [<Struct>]
    type Result<'a, 'b, 'c> =
        val mutable private Result: ('a -> 'c) -> ('b -> 'c) -> 'c
        private new(result) = { Result = result }

    type Result<'a, 'b, 'c>(result: ('a -> 'c) -> ('b -> 'c) -> 'c) =
        member x.Eval (successF: 'a -> 'c) (failureF: 'b -> 'c) = result successF failureF

    type Result<'a, 'b>(result) =
        inherit Result<'a, 'b, unit>(result)

module FastResult =
    type T<'a, 'b, 'c> = ('a -> 'c) -> ('b ->'c) -> 'c

    let (|Success|Failure|) result = result Choice1Of2 Choice2Of2

    let fail error (successF: 'a -> 'c) (failureF: 'b -> 'c) = failureF error
    let succeed value (successF: 'a -> 'c) (failureF: 'b -> 'c) = successF value

    let mapResult (f: 'a -> 'd) result (successF: 'd -> 'c) (failureF: 'b -> 'c) = result (f >> successF) failureF
    let mapError f result (successF: 'd -> 'c) (failureF: 'b -> 'c) = result successF (f >> failureF)

module Result =
    type T<'a, 'b, 'c> = private Result of (('a -> 'c) -> ('b ->'c) -> 'c)

    let (|Success|Failure|) (Result result) = result Choice1Of2 Choice2Of2
    
    let succeed value = Result (fun s _ -> s value)
    let fail error = Result (fun _ f -> f error)

    let mapResult f (Result result) = Result (fun s ff -> result (f >> s) ff)
    let mapError f (Result result) = Result (fun s ff -> result s (f >> ff))

module Old =
    type Result<'a, 'b> =
        | Success of 'a
        |

match fail 3 with
| Success x -> printfn "%A" x
| Failure y -> printfn "%A" y

let time f =
    let ts = System.Diagnostics.Stopwatch.StartNew()
    for x = 0 to 100 do
        f ()
    ts.Elapsed.TotalSeconds
    |> printfn "%.3f"