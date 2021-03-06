namespace RedBlackTree.Test

/// A pretty naive implementation of "find all permutations of the given list".
[<RequireQualifiedAccess>]
module Permutations =
    let rec private zipper (acc : 'a list) (results) (xs : 'a list) : ('a list * 'a list) seq =
        match xs with
        | [] -> seq { yield (acc, []) ; yield! results }
        | x :: xs ->
           zipper (x :: acc) ((acc, x :: xs) :: results) xs

    let private insertions (y : 'a) (xs : 'a list) =
        zipper [] [] xs
        |> Seq.map (fun (pre, post) -> List.rev pre @ (y :: post))

    let rec all (xs : 'a list) : 'a list seq =
        match xs with
        | [] -> Seq.singleton []
        | x1 :: xs ->
            all xs |> Seq.collect (insertions x1)

    let rec choose (xs : 'a list) (n : int) : ('a list * 'a list) seq =
        if n = 0 then Seq.singleton ([], xs) else
        match xs with
        | [] -> Seq.empty
        | x :: xs ->
            seq {
                yield! choose xs (n - 1) |> Seq.map (fun (i, rest) -> (x :: i, rest))
                yield! choose xs n |> Seq.map (fun (i, rest) -> i, x :: rest)
            }