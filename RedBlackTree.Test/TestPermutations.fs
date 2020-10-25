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

    [<TestCase 6>]
    let ``Test choose`` (n : int) =
        let choices = Permutations.choose [1..n] 3 |> Seq.toList
        let len = factorial n / (factorial (n - 3) * factorial 3)
        Set.ofList choices |> Set.count |> shouldEqual len
        List.length choices |> shouldEqual len
        choices
        |> List.iter (fun (perm, rest) ->
            perm |> shouldHaveLength 3
            rest |> shouldHaveLength (n - 3)
        )
        choices
        |> List.iter (fun (perm, rest) ->
            Set.ofList perm |> shouldHaveLength (List.length perm)
            Set.ofList rest |> shouldHaveLength (List.length rest)
        )