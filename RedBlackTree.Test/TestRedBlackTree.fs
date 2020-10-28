namespace RedBlackTree.Test

open RedBlackTree
open NUnit.Framework
open FsUnitTyped
open FsCheck

[<TestFixture>]
module TestRedBlackTree =

    /// Because why not.
    let distinctSorted (s : 'a seq) : 'a seq =
        seq {
            use e = s.GetEnumerator ()
            let mutable prev = None
            while e.MoveNext () do
                if (Some e.Current) <> prev then
                    prev <- Some e.Current
                    yield e.Current
        }

    [<Test>]
    let ``Test distinctSorted`` () =
        let property (l : int list) =
            let l = l |> List.sort
            distinctSorted l
            |> List.ofSeq
            |> shouldEqual (List.distinct l)
        Check.QuickThrowOnFailure property

    let propertyCases =
        [
            [0 ; 1]
            [2 ; -1 ; 1 ; 0]
            [3 ; 0 ; -1 ; 2 ; 1]
        ]
        |> List.map TestCaseData

    [<TestCaseSource "propertyCases">]
    let ``Examples found by property-based testing`` (l : int list) =
        l
        |> List.fold (fun tree i -> RedBlackTree.add i () tree) RedBlackTree.empty
        |> RedBlackTree.toListRev
        |> List.map fst
        |> List.rev
        |> shouldEqual (Set.ofList l |> Set.toList)

    [<TestCase 10>]
    let ``Exhaustive test`` (n : int) =
        let expected = [1..n] |> List.rev
        let collected =
            Permutations.choose [1..n] 5
            |> Seq.collect (fun (s, rest) -> Permutations.all s |> Seq.map (fun r -> r, rest))
        for perm, rest in collected do
            let baseRbt =
                perm
                |> List.fold (fun tree i -> RedBlackTree.add i () tree) RedBlackTree.empty
            for perm2 in Permutations.all rest do
                let rbt =
                    perm2
                    |> List.fold (fun tree i -> RedBlackTree.add i () tree) baseRbt
                if rbt |> RedBlackTree.toListRev |> List.map fst <> expected then failwithf "Correctness error: %+A produced %+A" perm rbt
                let balance = RedBlackTree.balanceFactor rbt
                if balance.Longest >= balance.Shortest * 2 then
                    failwithf "Unbalanced! %+A produced %+A (balance: %+A)"  perm rbt balance

    [<Test>]
    let ``Property-based test`` () =
        let property (list : int list) =
            list
            |> List.fold (fun tree i -> RedBlackTree.add i () tree) RedBlackTree.empty
            |> RedBlackTree.toListRev
            |> List.map fst
            |> List.rev
            |> shouldEqual (Set.ofList list |> Set.toList)

        let config = { Config.Default with MaxTest = 10000 }
        Check.One(config, property)

    [<Test>]
    let ``Property-based test for equivalence with sorting`` () =
        let property (list : int list) =
            list
            |> List.fold (fun tree i -> RSet.add i tree) RSet.empty
            |> RSet.toSeq
            |> Seq.zip (distinctSorted (List.sort list))
            |> Seq.iter (fun (i, j) -> if i <> j then failwith "oh no!")
        Check.QuickThrowOnFailure property

    [<Test>]
    let ``Property-based test for insertion when already present`` () =
        let property (i : int) (list : int list) =
            // We've already checked equivalence with sorting so this is general enough
            let list = i :: list
            list
            |> List.fold (fun tree i -> RedBlackTree.add i () tree) RedBlackTree.empty
            |> RedBlackTree.toSeq
            |> Seq.zip (distinctSorted (List.sort list))
            |> Seq.iter (fun (i, (j, ())) -> if i <> j then failwith "oh no!")
        Check.QuickThrowOnFailure property

    [<Test>]
    let ``toSeq vs toList`` () =
        let property (list : int list) =
            let rbt =
                list
                |> List.fold (fun tree i -> RedBlackTree.add i () tree) RedBlackTree.empty
            rbt
            |> RedBlackTree.toSeq
            |> Seq.toList
            |> shouldEqual (RedBlackTree.toListRev rbt |> List.rev)

        let config = { Config.Default with MaxTest = 10000 }
        Check.One(config, property)
