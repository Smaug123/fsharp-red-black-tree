namespace RedBlackTree

type Zero = private | Zero
type 'a Succ = private | Succ

[<Struct>]
type ValueAtDepth<'a, 'depth> = ValueAtDepth of 'a

[<RequireQualifiedAccess>]
module internal ValueAtDepth =
    let inline value (ValueAtDepth a) = a

    let inline elevate<'a, 'depth> (ValueAtDepth a : ValueAtDepth<'a, 'depth>) : ValueAtDepth<'a, Succ<'depth>> =
        ValueAtDepth a

    let inline collapse<'a, 'depth> (ValueAtDepth a : ValueAtDepth<'a, Succ<'depth>>) : ValueAtDepth<'a, 'depth> =
        ValueAtDepth a

type private RedNode<'a, 'v, 'depth when 'a : comparison> = RedNode of BlackNode<'a, 'v, 'depth> * BlackNode<'a, 'v, 'depth> * ValueAtDepth<'a * 'v, 'depth>

and [<RequireQualifiedAccess>] private BlackNode<'a, 'v, 'depth when 'a : comparison> =
    | Leaf
    | RedRedNode of RedNode<'a, 'v, Succ<'depth>> * RedNode<'a, 'v, Succ<'depth>> * ValueAtDepth<'a * 'v, 'depth>
    | RedBlackNode of RedNode<'a, 'v, Succ<'depth>> * BlackNode<'a, 'v, Succ<'depth>> * ValueAtDepth<'a * 'v, 'depth>
    | BlackRedNode of BlackNode<'a, 'v, Succ<'depth>> * RedNode<'a, 'v, Succ<'depth>> * ValueAtDepth<'a * 'v, 'depth>
    | BlackBlackNode of BlackNode<'a, 'v, Succ<'depth>> * BlackNode<'a, 'v, Succ<'depth>> * ValueAtDepth<'a * 'v, 'depth>

[<RequireQualifiedAccess>]
type RedBlackTree<'a, 'v when 'a : comparison> =
    private
    | BlackRoot of BlackNode<'a, 'v, Zero>
    | RedRoot of RedNode<'a, 'v, Zero>

[<RequireQualifiedAccess>]
module RedBlackTree =

    /// A red-black tree holding no data.
    let empty<'a, 'v when 'a : comparison> : RedBlackTree<'a, 'v> = RedBlackTree.BlackRoot (BlackNode.Leaf)

    let rec private elevateBlack<'a, 'v, 'depth when 'a : comparison>
        (node : BlackNode<'a, 'v, 'depth>)
        : BlackNode<'a, 'v, Succ<'depth>>
        =
        match node with
        | BlackNode.Leaf -> BlackNode.Leaf
        | BlackNode.BlackBlackNode (left, right, value) ->
            BlackNode.BlackBlackNode (elevateBlack left, elevateBlack right, ValueAtDepth.elevate value)
        | BlackNode.RedBlackNode (left, right, value) ->
            BlackNode.RedBlackNode (elevateRed left, elevateBlack right, ValueAtDepth.elevate value)
        | BlackNode.RedRedNode (left, right, value) ->
            BlackNode.RedRedNode (elevateRed left, elevateRed right, ValueAtDepth.elevate value)
        | BlackNode.BlackRedNode (left, right, value) ->
            BlackNode.BlackRedNode (elevateBlack left, elevateRed right, ValueAtDepth.elevate value)

    and private elevateRed<'a, 'v, 'depth when 'a : comparison>
        (node : RedNode<'a, 'v, 'depth>)
        : RedNode<'a, 'v, Succ<'depth>>
        =
        match node with
        | RedNode.RedNode (left, right, value) ->
            RedNode.RedNode (elevateBlack left, elevateBlack right, ValueAtDepth.elevate value)

    type private AdditionRedResult<'a, 'v, 'depth when 'a : comparison> =
        | AlreadyPresent
        | Red of RedNode<'a, 'v, 'depth>
        | NeedsRebalance of {| Upper : ValueAtDepth<'a * 'v, 'depth>
                               Lower : ValueAtDepth<'a * 'v, 'depth>
                               Left : BlackNode<'a, 'v, 'depth>
                               Middle : BlackNode<'a, 'v, 'depth>
                               Right : BlackNode<'a, 'v, 'depth> |}

    type private AdditionBlackResult<'a, 'v, 'depth when 'a : comparison> =
        | AlreadyPresent
        | Red of RedNode<'a, 'v, 'depth>
        | Black of BlackNode<'a, 'v, 'depth>

    let rec private addBlack<'a, 'v, 'depth when 'a : comparison>
        (parent : BlackNode<'a, 'v, 'depth>)
        (elt : 'a)
        (value : 'v)
        : AdditionBlackResult<'a, 'v, 'depth>
        =
        match parent with
        | BlackNode.Leaf ->
            AdditionBlackResult.Red (RedNode (BlackNode.Leaf, BlackNode.Leaf, ValueAtDepth (elt, value)))
        | BlackNode.RedRedNode (leftTop, rightTop, valueTop) ->
            if elt = fst (ValueAtDepth.value valueTop) then
                AdditionBlackResult.AlreadyPresent
            elif elt < fst (ValueAtDepth.value valueTop) then
                match addRed<'a, 'v, Succ<'depth>> leftTop elt value with
                | AdditionRedResult.AlreadyPresent -> AdditionBlackResult.AlreadyPresent
                | AdditionRedResult.Red (RedNode (left, right, value)) ->
                    AdditionBlackResult.Black (BlackNode.RedRedNode (RedNode (left, right, value), rightTop, valueTop))
                | AdditionRedResult.NeedsRebalance info ->
                    RedNode
                        (BlackNode.BlackBlackNode (info.Left, info.Middle, ValueAtDepth.collapse info.Lower),
                         BlackNode.BlackRedNode (info.Right, rightTop, valueTop),
                         ValueAtDepth.collapse info.Upper)
                    |> AdditionBlackResult.Red
            else
                match addRed rightTop elt value with
                | AdditionRedResult.AlreadyPresent -> AdditionBlackResult.AlreadyPresent
                | AdditionRedResult.Red (RedNode (left, right, value)) ->
                    AdditionBlackResult.Black (BlackNode.RedRedNode (leftTop, RedNode (left, right, value), valueTop))
                | AdditionRedResult.NeedsRebalance info ->
                    RedNode
                        (BlackNode.RedBlackNode (leftTop, info.Left, valueTop),
                         BlackNode.BlackBlackNode (info.Middle, info.Right, ValueAtDepth.collapse info.Upper),
                         ValueAtDepth.collapse info.Lower)
                    |> AdditionBlackResult.Red
        | BlackNode.BlackRedNode (leftTop, rightTop, valueTop) ->
            if elt = fst (ValueAtDepth.value valueTop) then
                AdditionBlackResult.AlreadyPresent
            elif elt < fst (ValueAtDepth.value valueTop) then
                match addBlack leftTop elt value with
                | AdditionBlackResult.AlreadyPresent -> AdditionBlackResult.AlreadyPresent
                | AdditionBlackResult.Red redNode ->
                    AdditionBlackResult.Black (BlackNode.RedRedNode (redNode, rightTop, valueTop))
                | AdditionBlackResult.Black blackNode ->
                    AdditionBlackResult.Black (BlackNode.BlackRedNode (blackNode, rightTop, valueTop))
            else
                match addRed rightTop elt value with
                | AdditionRedResult.AlreadyPresent -> AdditionBlackResult.AlreadyPresent
                | AdditionRedResult.Red redNode ->
                    AdditionBlackResult.Black (BlackNode.BlackRedNode (leftTop, redNode, valueTop))
                | AdditionRedResult.NeedsRebalance info ->
                    RedNode
                        (BlackNode.BlackBlackNode (leftTop, info.Left, valueTop),
                         BlackNode.BlackBlackNode (info.Middle, info.Right, ValueAtDepth.collapse info.Upper),
                         ValueAtDepth.collapse info.Lower)
                    |> AdditionBlackResult.Red
        | BlackNode.RedBlackNode (leftTop, rightTop, valueTop) ->
            if elt = fst (ValueAtDepth.value valueTop) then
                AdditionBlackResult.AlreadyPresent
            elif elt < fst (ValueAtDepth.value valueTop) then
                match addRed leftTop elt value with
                | AdditionRedResult.AlreadyPresent -> AdditionBlackResult.AlreadyPresent
                | AdditionRedResult.Red redNode ->
                    AdditionBlackResult.Black (BlackNode.RedBlackNode (redNode, rightTop, valueTop))
                | AdditionRedResult.NeedsRebalance info ->
                    RedNode
                        (BlackNode.BlackBlackNode (info.Left, info.Middle, ValueAtDepth.collapse info.Lower),
                         BlackNode.BlackBlackNode (info.Right, rightTop, valueTop),
                         ValueAtDepth.collapse info.Upper)
                    |> AdditionBlackResult.Red
            else
                match addBlack rightTop elt value with
                | AdditionBlackResult.AlreadyPresent -> AdditionBlackResult.AlreadyPresent
                | AdditionBlackResult.Red redNode ->
                    AdditionBlackResult.Black (BlackNode.RedRedNode (leftTop, redNode, valueTop))
                | AdditionBlackResult.Black blackNode ->
                    AdditionBlackResult.Black (BlackNode.RedBlackNode (leftTop, blackNode, valueTop))
        | BlackNode.BlackBlackNode (leftTop, rightTop, valueTop) ->
            if elt = fst (ValueAtDepth.value valueTop) then
                AdditionBlackResult.AlreadyPresent
            elif elt < fst (ValueAtDepth.value valueTop) then
                match addBlack leftTop elt value with
                | AdditionBlackResult.AlreadyPresent -> AdditionBlackResult.AlreadyPresent
                | AdditionBlackResult.Red redNode ->
                    AdditionBlackResult.Black (BlackNode.RedBlackNode (redNode, rightTop, valueTop))
                | AdditionBlackResult.Black blackNode ->
                    AdditionBlackResult.Black (BlackNode.BlackBlackNode (blackNode, rightTop, valueTop))
            else
                match addBlack rightTop elt value with
                | AdditionBlackResult.AlreadyPresent -> AdditionBlackResult.AlreadyPresent
                | AdditionBlackResult.Red redNode ->
                    AdditionBlackResult.Black (BlackNode.BlackRedNode (leftTop, redNode, valueTop))
                | AdditionBlackResult.Black blackNode ->
                    AdditionBlackResult.Black (BlackNode.BlackBlackNode (leftTop, blackNode, valueTop))

    and private addRed<'a, 'v, 'depth when 'a : comparison>
        (parent : RedNode<'a, 'v, 'depth>)
        (elt : 'a)
        (value : 'v)
        : AdditionRedResult<'a, 'v, 'depth>
        =
        match parent with
        | RedNode (leftTop, rightTop, valueTop) ->
            if elt < fst (ValueAtDepth.value valueTop) then
                match addBlack leftTop elt value with
                | AdditionBlackResult.AlreadyPresent -> AdditionRedResult.AlreadyPresent
                | AdditionBlackResult.Red (RedNode (left, right, value)) ->
                    {|
                        Upper = valueTop
                        Lower = value
                        Left = left
                        Middle = right
                        Right = rightTop
                    |}
                    |> NeedsRebalance
                | AdditionBlackResult.Black blackNode -> AdditionRedResult.Red (RedNode (blackNode, rightTop, valueTop))
            elif elt > fst (ValueAtDepth.value valueTop) then
                match addBlack rightTop elt value with
                | AdditionBlackResult.AlreadyPresent -> AdditionRedResult.AlreadyPresent
                | AdditionBlackResult.Red (RedNode (left, right, value)) ->
                    {|
                        Upper = value
                        Lower = valueTop
                        Left = leftTop
                        Middle = left
                        Right = right
                    |}
                    |> NeedsRebalance
                | AdditionBlackResult.Black blackNode -> AdditionRedResult.Red (RedNode (leftTop, blackNode, valueTop))
            else
                AdditionRedResult.AlreadyPresent

    let add<'a, 'v when 'a : comparison> (elt : 'a) (value : 'v) (tree : RedBlackTree<'a, 'v>) : RedBlackTree<'a, 'v> =
        match tree with
        | RedBlackTree.RedRoot node ->
            addRed node elt value
            |> function
            | AdditionRedResult.AlreadyPresent -> tree
            | AdditionRedResult.Red node -> RedBlackTree.RedRoot node
            | AdditionRedResult.NeedsRebalance info ->
                BlackNode.RedBlackNode
                    (RedNode.RedNode (elevateBlack info.Left, elevateBlack info.Middle, ValueAtDepth.elevate info.Lower),
                     elevateBlack info.Right,
                     info.Upper)
                |> RedBlackTree.BlackRoot
        | RedBlackTree.BlackRoot node ->
            addBlack node elt value
            |> function
            | AdditionBlackResult.AlreadyPresent -> tree
            | AdditionBlackResult.Black node -> RedBlackTree.BlackRoot node
            | AdditionBlackResult.Red node -> RedBlackTree.RedRoot node

    let rec private tryFindBlack<'a, 'v, 'depth when 'a : comparison>
        (tree : BlackNode<'a, 'v, 'depth>)
        (elt : 'a)
        : 'v option
        =
        match tree with
        | BlackNode.Leaf -> None
        | BlackNode.RedRedNode (left, right, value) ->
            if elt = fst (ValueAtDepth.value value) then Some (snd (ValueAtDepth.value value))
            elif elt < fst (ValueAtDepth.value value) then tryFindRed left elt
            else tryFindRed right elt
        | BlackNode.RedBlackNode (left, right, value) ->
            if elt = fst (ValueAtDepth.value value) then Some (snd (ValueAtDepth.value value))
            elif elt < fst (ValueAtDepth.value value) then tryFindRed left elt
            else tryFindBlack right elt
        | BlackNode.BlackRedNode (left, right, value) ->
            if elt = fst (ValueAtDepth.value value) then Some (snd (ValueAtDepth.value value))
            elif elt < fst (ValueAtDepth.value value) then tryFindBlack left elt
            else tryFindRed right elt
        | BlackNode.BlackBlackNode (left, right, value) ->
            if elt = fst (ValueAtDepth.value value) then Some (snd (ValueAtDepth.value value))
            elif elt < fst (ValueAtDepth.value value) then tryFindBlack left elt
            else tryFindBlack right elt

    and private tryFindRed<'a, 'v, 'depth when 'a : comparison> (tree : RedNode<'a, 'v, 'depth>) (elt : 'a) : 'v option =
        match tree with
        | RedNode (left, right, value) ->
            if elt = fst (ValueAtDepth.value value) then Some (snd (ValueAtDepth.value value))
            elif elt < fst (ValueAtDepth.value value) then tryFindBlack left elt
            else tryFindBlack right elt

    let tryFind<'a, 'v when 'a : comparison> (elt : 'a) (t : RedBlackTree<'a, 'v>) : 'v option =
        match t with
        | RedBlackTree.BlackRoot root -> tryFindBlack root elt
        | RedBlackTree.RedRoot root -> tryFindRed root elt

    let rec private foldBlack<'a, 'v, 'state, 'depth when 'a : comparison>
        (folder : 'state -> ('a * 'v) -> 'state)
        (state : 'state)
        (tree : BlackNode<'a, 'v, 'depth>)
        : 'state
        =
        match tree with
        | BlackNode.Leaf -> state
        | BlackNode.RedRedNode (left, right, value) ->
            let state = foldRed folder state left
            let state = folder state (ValueAtDepth.value value)
            foldRed folder state right
        | BlackNode.BlackRedNode (left, right, value) ->
            let state = foldBlack folder state left
            let state = folder state (ValueAtDepth.value value)
            foldRed folder state right
        | BlackNode.BlackBlackNode (left, right, value) ->
            let state = foldBlack folder state left
            let state = folder state (ValueAtDepth.value value)
            foldBlack folder state right
        | BlackNode.RedBlackNode (left, right, value) ->
            let state = foldRed folder state left
            let state = folder state (ValueAtDepth.value value)
            foldBlack folder state right

    and private foldRed<'a, 'v, 'state, 'depth when 'a : comparison>
        (folder : 'state -> ('a * 'v) -> 'state)
        (state : 'state)
        (tree : RedNode<'a, 'v, 'depth>)
        : 'state
        =
        match tree with
        | RedNode (left, right, value) ->
            let state = foldBlack folder state left
            let state = folder state (ValueAtDepth.value value)
            foldBlack folder state right

    let fold<'a, 'v, 'state when 'a : comparison>
        (folder : 'state -> ('a * 'v) -> 'state)
        (state : 'state)
        (tree : RedBlackTree<'a, 'v>)
        : 'state
        =
        match tree with
        | RedBlackTree.RedRoot root -> foldRed folder state root
        | RedBlackTree.BlackRoot root -> foldBlack folder state root

    /// Convert the tree to a list, in reverse sorted order.
    let toListRev<'a, 'v when 'a : comparison> (tree : RedBlackTree<'a, 'v>) : ('a * 'v) list =
        fold (fun ls a -> a :: ls) [] tree

    let rec private toSeqBlack<'a, 'v, 'depth when 'a : comparison> (tree : BlackNode<'a, 'v, 'depth>) : ('a * 'v) seq =
        seq {
            match tree with
            | BlackNode.Leaf -> ()
            | BlackNode.BlackBlackNode (left, right, value) ->
                yield! toSeqBlack left
                yield (ValueAtDepth.value value)
                yield! toSeqBlack right
            | BlackNode.BlackRedNode (left, right, value) ->
                yield! toSeqBlack left
                yield (ValueAtDepth.value value)
                yield! toSeqRed right
            | BlackNode.RedBlackNode (left, right, value) ->
                yield! toSeqRed left
                yield (ValueAtDepth.value value)
                yield! toSeqBlack right
            | BlackNode.RedRedNode (left, right, value) ->
                yield! toSeqRed left
                yield (ValueAtDepth.value value)
                yield! toSeqRed right
        }

    and private toSeqRed<'a, 'v, 'depth when 'a : comparison> (tree : RedNode<'a, 'v, 'depth>) : ('a * 'v) seq =
        seq {
            match tree with
            | RedNode.RedNode (left, right, value) ->
                yield! toSeqBlack left
                yield (ValueAtDepth.value value)
                yield! toSeqBlack right
        }

    let toSeq<'a, 'v when 'a : comparison> (tree : RedBlackTree<'a, 'v>) : ('a * 'v) seq =
        match tree with
        | RedBlackTree.RedRoot root -> toSeqRed root
        | RedBlackTree.BlackRoot root -> toSeqBlack root

    let rec private balanceFactorBlack<'a, 'v, 'depth when 'a : comparison> (node : BlackNode<'a, 'v, 'depth>) : int * int =
        match node with
        | BlackNode.Leaf -> 0, 0
        | BlackNode.BlackBlackNode (left, right, _) ->
            let (min1, max1) = balanceFactorBlack left
            let (min2, max2) = balanceFactorBlack right
            (min min1 min2, max max1 max2)
        | BlackNode.BlackRedNode (left, right, _) ->
            let (min1, max1) = balanceFactorBlack left
            let (min2, max2) = balanceFactorRed right
            (min min1 min2, max max1 max2)
        | BlackNode.RedBlackNode (left, right, _) ->
            let (min1, max1) = balanceFactorRed left
            let (min2, max2) = balanceFactorBlack right
            (min min1 min2, max max1 max2)
        | BlackNode.RedRedNode (left, right, _) ->
            let (min1, max1) = balanceFactorRed left
            let (min2, max2) = balanceFactorRed right
            (min min1 min2, max max1 max2)
        |> fun (a, b) -> (a + 1, b + 1)

    and private balanceFactorRed<'a, 'v, 'depth when 'a : comparison> (node : RedNode<'a, 'v, 'depth>) : int * int =
        match node with
        | RedNode (left, right, _) ->
            let (min1, max1) = balanceFactorBlack left
            let (min2, max2) = balanceFactorBlack right
            (min min1 min2, max max1 max2)
        |> fun (a, b) -> (a + 1, b + 1)

    /// Answer the question: how long is the longest path through the graph, and the shortest?
    let balanceFactor<'a, 'v when 'a : comparison> (tree : RedBlackTree<'a, 'v>) : {| Longest : int ; Shortest : int |} =
        match tree with
        | RedBlackTree.RedRoot red -> balanceFactorRed red
        | RedBlackTree.BlackRoot black -> balanceFactorBlack black
        |> fun (short, long) -> {| Shortest = short ; Longest = long |}
