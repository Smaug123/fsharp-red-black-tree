namespace RedBlackTree.Test

open RedBlackTree
open NUnit.Framework
open FsUnitTyped
open FsCheck

[<TestFixture>]
module TestRedBlackTree =

    [<Test>]
    let ``First`` () =
        [0 ; 1]
        |> List.fold RedBlackTree.add RedBlackTree.empty
        |> RedBlackTree.toList
        |> shouldEqual (Set.ofList [0 ; 1] |> Set.toList)

    [<Test>]
    let ``Second`` () =
        [2 ; -1 ; 1 ; 0]
        |> List.fold RedBlackTree.add RedBlackTree.empty
        |> RedBlackTree.toList
        |> shouldEqual (Set.ofList [2 ; -1 ; 1 ; 0] |> Set.toList)

    [<Test>]
    let ``Third`` () =
        [3 ; 0 ; -1 ; 2 ; 1]
        |> List.fold (RedBlackTree.add) RedBlackTree.empty
        |> RedBlackTree.toList
        |> shouldEqual (Set.ofList [-1 ; 0 ; 1 ; 2 ; 3] |> Set.toList)

    [<Test>]
    let ``Property-based test`` () =
        let property (list : int list) =
            list
            |> List.fold RedBlackTree.add RedBlackTree.empty
            |> RedBlackTree.toList
            |> shouldEqual (Set.ofList list |> Set.toList)

        let config = { Config.Default with MaxTest = 10000 }
        Check.One(config, property)