namespace Validation

module OResult =
    [<Struct>]
    type Result<'a, 'b, 'c> =
        val private Result: ('a -> 'c) -> ('b -> 'c) -> 'c
        private new(result) = { Result = result }