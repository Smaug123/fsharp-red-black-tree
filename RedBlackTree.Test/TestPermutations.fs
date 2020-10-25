namespace RedBlackTree.Test

open NUnit.Framework
open FsUnitTyped

[<TestFixture>]
module TestPermutations =

    let private factorial (n : int) =
        let rec go acc i = if i <= 1 then acc else go (acc * i) (i - 1)
        go 1 n

    [<Test>]
    let ``Test factorial`` () =
        [1..5]
        |> List.map factorial
        |> shouldEqual [1 ; 2 ; 6 ; 24 ; 120]

    [<TestCase 5>]
    let ``Test permutations`` (n : int) =
        let allPerms =
            Permutations.all [1..n]
            |> Seq.toList
        let fact = factorial n
        Set.ofList allPerms |> Set.count |> shouldEqual fact
        allPerms |> List.length |> shouldEqual fact
        allPerms |> List.iter (fun i -> Set.ofList i |> shouldEqual (Set.ofList [1..n]))

