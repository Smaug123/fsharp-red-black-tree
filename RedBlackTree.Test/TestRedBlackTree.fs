namespace RedBlackTree.Test

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
        |> List.fold RedBlackTree.add RedBlackTree.empty
        |> RedBlackTree.toList
        |> shouldEqual (Set.ofList l |> Set.toList)

    [<TestCase 11>]
    let ``Exhaustive test`` (n : int) =
        for perm in Permutations.all [1..n] do
            let rbt =
                perm
                |> List.fold RedBlackTree.add RedBlackTree.empty
            if rbt |> RedBlackTree.toList <> [1..n] then failwithf "Correctness error: %+A produced %+A" perm rbt
            let balance = RedBlackTree.balanceFactor rbt
            if balance.Longest >= balance.Shortest * 2 then
                failwithf "Unbalanced! %+A produced %+A (balance: %+A)"  perm rbt balance

    [<Test>]
    let ``Property-based test`` () =
        let property (list : int list) =
            list
            |> List.fold RedBlackTree.add RedBlackTree.empty
            |> RedBlackTree.toList
            |> shouldEqual (Set.ofList list |> Set.toList)

        let config = { Config.Default with MaxTest = 10000 }
        Check.One(config, property)