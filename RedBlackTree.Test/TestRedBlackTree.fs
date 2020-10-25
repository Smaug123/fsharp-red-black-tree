namespace RedBlackTree.Test

open System.Collections.Generic
open RedBlackTree
open NUnit.Framework
open FsUnitTyped
open FsCheck

[<TestFixture>]
module TestRedBlackTree =

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
