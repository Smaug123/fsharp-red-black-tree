namespace RedBlackTree

[<Struct>]
type RSet<'a when 'a : comparison> = | RSet of RedBlackTree<'a, unit>

[<RequireQualifiedAccess>]
module RSet =
    let empty<'a when 'a : comparison> : RSet<'a> = RSet (RedBlackTree.empty)

    let add<'a when 'a : comparison> (a : 'a) (RSet s : RSet<'a>) : RSet<'a> =
        RedBlackTree.add a () s
        |> RSet

    let contains<'a when 'a : comparison> (a : 'a) (RSet s : RSet<'a>) : bool =
        match RedBlackTree.tryFind a s with
        | Some _ -> true
        | None -> false

    let toListRev<'a when 'a : comparison> (RSet s : RSet<'a>) : 'a list =
        RedBlackTree.toListRev s
        |> List.map fst

    let toSeq<'a when 'a : comparison> (RSet s : RSet<'a>) : 'a seq =
        RedBlackTree.toSeq s
        |> Seq.map fst
