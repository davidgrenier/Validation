namespace Validation

module OResult =
    [<NoEquality; NoComparison>]
    type Result<'a, 'b, 'c> =
        struct
            val private Result: ('a -> 'c) -> ('b -> 'c) -> 'c
        end