namespace RedBlackTree

type Zero = private | Zero
type 'a Succ = private | Succ

[<Struct>]
type ValueAtDepth<'a, 'depth> = ValueAtDepth of 'a

[<RequireQualifiedAccess>]
module ValueAtDepth =
    let inline value (ValueAtDepth a) = a

    let inline elevate<'a, 'depth> (ValueAtDepth a : ValueAtDepth<'a, 'depth>) : ValueAtDepth<'a, Succ<'depth>> =
        ValueAtDepth a

    let inline collapse<'a, 'depth> (ValueAtDepth a : ValueAtDepth<'a, Succ<'depth>>) : ValueAtDepth<'a, 'depth> =
        ValueAtDepth a

type RedNode<'a, 'depth when 'a : comparison> = RedNode of BlackNode<'a, 'depth> * BlackNode<'a, 'depth> * ValueAtDepth<'a, 'depth>

and [<RequireQualifiedAccess>] BlackNode<'a, 'depth when 'a : comparison> =
    | Leaf
    | RedRedNode of RedNode<'a, Succ<'depth>> * RedNode<'a, Succ<'depth>> * ValueAtDepth<'a, 'depth>
    | RedBlackNode of RedNode<'a, Succ<'depth>> * BlackNode<'a, Succ<'depth>> * ValueAtDepth<'a, 'depth>
    | BlackRedNode of BlackNode<'a, Succ<'depth>> * RedNode<'a, Succ<'depth>> * ValueAtDepth<'a, 'depth>
    | BlackBlackNode of BlackNode<'a, Succ<'depth>> * BlackNode<'a, Succ<'depth>> * ValueAtDepth<'a, 'depth>

[<RequireQualifiedAccess>]
type RedBlackTree<'a when 'a : comparison> =
    private
    | BlackRoot of BlackNode<'a, Zero>
    | RedRoot of RedNode<'a, Zero>

type RedNodeEvaluator<'a, 'ret when 'a : comparison> =
    abstract Eval<'depth> : RedNode<'a, 'depth> -> 'ret

type RedNodeCrate<'a when 'a : comparison> =
    abstract Apply<'ret> : RedNodeEvaluator<'a, 'ret> -> 'ret

type BlackNodeEvaluator<'a, 'ret when 'a : comparison> =
    abstract Eval<'depth> : BlackNode<'a, 'depth> -> 'ret

type BlackNodeCrate<'a when 'a : comparison> =
    abstract Apply<'ret> : BlackNodeEvaluator<'a, 'ret> -> 'ret

[<RequireQualifiedAccess>]
module RedNodeCrate =
    let make<'a, 'depth when 'a : comparison> (node : RedNode<'a, 'depth>) =
        { new RedNodeCrate<'a> with
            member __.Apply e = e.Eval node
        }

[<RequireQualifiedAccess>]
module BlackNodeCrate =
    let make<'a, 'depth when 'a : comparison> (node : BlackNode<'a, 'depth>) =
        { new BlackNodeCrate<'a> with
            member __.Apply e = e.Eval node
        }

[<RequireQualifiedAccess>]
module RedBlackTree =

    let empty<'a when 'a : comparison> : RedBlackTree<'a> = RedBlackTree.BlackRoot (BlackNode.Leaf)

    let rec private elevateBlack<'a, 'depth when 'a : comparison>
        (node : BlackNode<'a, 'depth>)
        : BlackNode<'a, Succ<'depth>>
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

    and private elevateRed<'a, 'depth when 'a : comparison> (node : RedNode<'a, 'depth>) : RedNode<'a, Succ<'depth>> =
        match node with
        | RedNode.RedNode (left, right, value) ->
            RedNode.RedNode (elevateBlack left, elevateBlack right, ValueAtDepth.elevate value)

    type private AdditionResult<'a, 'depth when 'a : comparison> =
        | AlreadyPresent
        | Red of RedNode<'a, 'depth>
        | Black of BlackNode<'a, 'depth>
        | NeedsRebalance of {| Upper : ValueAtDepth<'a, 'depth>
                               Lower : ValueAtDepth<'a, 'depth>
                               Left : BlackNode<'a, 'depth>
                               Middle : BlackNode<'a, 'depth>
                               Right : BlackNode<'a, 'depth> |}

    let rec private addBlack<'a, 'depth when 'a : comparison>
        (parent : BlackNode<'a, 'depth>)
        (elt : 'a)
        : AdditionResult<'a, 'depth>
        =
        match parent with
        | BlackNode.Leaf -> AdditionResult.Red (RedNode (BlackNode.Leaf, BlackNode.Leaf, ValueAtDepth elt))
        | BlackNode.RedRedNode (leftTop, rightTop, valueTop) ->
            if elt = ValueAtDepth.value valueTop then
                AdditionResult.AlreadyPresent
            elif elt < ValueAtDepth.value valueTop then
                match addRed<'a, Succ<'depth>> leftTop elt with
                | AdditionResult.AlreadyPresent -> AdditionResult.AlreadyPresent
                | AdditionResult.Red (RedNode (left, right, value)) ->
                    AdditionResult.Black (BlackNode.RedRedNode (RedNode (left, right, value), rightTop, valueTop))
                | AdditionResult.Black blackNode ->
                    AdditionResult.Black (BlackNode.BlackRedNode (blackNode, rightTop, valueTop))
                | AdditionResult.NeedsRebalance info ->
                    RedNode
                        (BlackNode.BlackBlackNode (info.Left, info.Middle, ValueAtDepth.collapse info.Lower),
                         BlackNode.BlackRedNode (info.Right, rightTop, valueTop),
                         ValueAtDepth.collapse info.Upper)
                    |> AdditionResult.Red
            else
                match addRed<'a, Succ<'depth>> rightTop elt with
                | AdditionResult.AlreadyPresent -> AdditionResult.AlreadyPresent
                | AdditionResult.Red (RedNode (left, right, value)) ->
                    AdditionResult.Black (BlackNode.RedRedNode (leftTop, RedNode (left, right, value), valueTop))
                | AdditionResult.Black blackNode ->
                    AdditionResult.Black (BlackNode.BlackRedNode (blackNode, rightTop, valueTop))
                | AdditionResult.NeedsRebalance info ->
                    RedNode
                        (BlackNode.RedBlackNode (leftTop, info.Left, valueTop),
                         BlackNode.BlackBlackNode (info.Middle, info.Right, ValueAtDepth.collapse info.Upper),
                         ValueAtDepth.collapse info.Lower)
                    |> AdditionResult.Red
        | BlackNode.BlackRedNode (leftTop, rightTop, valueTop) ->
            if elt = ValueAtDepth.value valueTop then
                AdditionResult.AlreadyPresent
            elif elt < ValueAtDepth.value valueTop then
                match addBlack<'a, Succ<'depth>> leftTop elt with
                | AdditionResult.AlreadyPresent -> AdditionResult.AlreadyPresent
                | AdditionResult.Red redNode ->
                    AdditionResult.Black (BlackNode.RedRedNode (redNode, rightTop, valueTop))
                | AdditionResult.Black blackNode ->
                    AdditionResult.Black (BlackNode.BlackRedNode (blackNode, rightTop, valueTop))
                | AdditionResult.NeedsRebalance info ->
                    RedNode
                        (BlackNode.BlackBlackNode (info.Left, info.Middle, ValueAtDepth.collapse info.Lower),
                         BlackNode.BlackRedNode (info.Right, rightTop, valueTop),
                         ValueAtDepth.collapse info.Upper)
                    |> AdditionResult.Red
            else
                match addRed<'a, Succ<'depth>> rightTop elt with
                | AdditionResult.AlreadyPresent -> AdditionResult.AlreadyPresent
                | AdditionResult.Red redNode ->
                    AdditionResult.Black (BlackNode.BlackRedNode (leftTop, redNode, valueTop))
                | AdditionResult.Black blackNode ->
                    // TODO - we could do this as a red node if we bumped the depth
                    AdditionResult.Black (BlackNode.BlackBlackNode (leftTop, blackNode, valueTop))
                | AdditionResult.NeedsRebalance info ->
                    RedNode
                        (BlackNode.BlackBlackNode (leftTop, info.Left, valueTop),
                         BlackNode.BlackBlackNode (info.Middle, info.Right, ValueAtDepth.collapse info.Upper),
                         ValueAtDepth.collapse info.Lower)
                    |> AdditionResult.Red
        | BlackNode.RedBlackNode (leftTop, rightTop, valueTop) ->
            if elt = ValueAtDepth.value valueTop then
                AdditionResult.AlreadyPresent
            elif elt < ValueAtDepth.value valueTop then
                match addRed leftTop elt with
                | AdditionResult.AlreadyPresent -> AdditionResult.AlreadyPresent
                | AdditionResult.Red redNode ->
                    AdditionResult.Black (BlackNode.RedBlackNode (redNode, rightTop, valueTop))
                | AdditionResult.Black blackNode ->
                    AdditionResult.Black (BlackNode.BlackBlackNode (blackNode, rightTop, valueTop))
                | AdditionResult.NeedsRebalance info ->
                    RedNode
                        (BlackNode.BlackBlackNode (info.Left, info.Middle, ValueAtDepth.collapse info.Lower),
                         BlackNode.BlackBlackNode (info.Right, rightTop, valueTop),
                         ValueAtDepth.collapse info.Upper)
                    |> AdditionResult.Red
            else
                match addBlack rightTop elt with
                | AdditionResult.AlreadyPresent -> AdditionResult.AlreadyPresent
                | AdditionResult.Red redNode -> AdditionResult.Black (BlackNode.RedRedNode (leftTop, redNode, valueTop))
                | AdditionResult.Black blackNode ->
                    AdditionResult.Black (BlackNode.RedBlackNode (leftTop, blackNode, valueTop))
                | AdditionResult.NeedsRebalance info ->
                    RedNode
                        (BlackNode.RedBlackNode (leftTop, info.Left, valueTop),
                         BlackNode.BlackBlackNode (info.Middle, info.Right, ValueAtDepth.collapse info.Upper),
                         ValueAtDepth.collapse info.Lower)
                    |> AdditionResult.Red
        | BlackNode.BlackBlackNode (leftTop, rightTop, valueTop) ->
            if elt = ValueAtDepth.value valueTop then
                AdditionResult.AlreadyPresent
            elif elt < ValueAtDepth.value valueTop then
                match addBlack leftTop elt with
                | AdditionResult.AlreadyPresent -> AdditionResult.AlreadyPresent
                | AdditionResult.Red redNode ->
                    AdditionResult.Black (BlackNode.RedBlackNode (redNode, rightTop, valueTop))
                | AdditionResult.Black blackNode ->
                    AdditionResult.Black (BlackNode.BlackBlackNode (blackNode, rightTop, valueTop))
                | AdditionResult.NeedsRebalance info -> failwith ""
            else
                match addBlack rightTop elt with
                | AdditionResult.AlreadyPresent -> AdditionResult.AlreadyPresent
                | AdditionResult.Red redNode ->
                    AdditionResult.Black (BlackNode.BlackRedNode (leftTop, redNode, valueTop))
                | AdditionResult.Black blackNode ->
                    AdditionResult.Black (BlackNode.BlackBlackNode (leftTop, blackNode, valueTop))
                | AdditionResult.NeedsRebalance info -> failwith ""

    and private addRed<'a, 'depth when 'a : comparison>
        (parent : RedNode<'a, 'depth>)
        (elt : 'a)
        : AdditionResult<'a, 'depth>
        =
        match parent with
        | RedNode (leftTop, rightTop, valueTop) ->
            if elt < ValueAtDepth.value valueTop then
                match addBlack<'a, 'depth> leftTop elt with
                | AdditionResult.AlreadyPresent -> AdditionResult.AlreadyPresent
                | AdditionResult.Red (RedNode (left, right, value)) ->
                    {|
                        Upper = valueTop
                        Lower = value
                        Left = left
                        Middle = right
                        Right = rightTop
                    |}
                    |> NeedsRebalance
                | AdditionResult.Black blackNode -> AdditionResult.Red (RedNode (blackNode, rightTop, valueTop))
                | AdditionResult.NeedsRebalance info -> failwith ""
            elif elt > ValueAtDepth.value valueTop then
                match addBlack<'a, 'depth> rightTop elt with
                | AdditionResult.AlreadyPresent -> AdditionResult.AlreadyPresent
                | AdditionResult.Red (RedNode (left, right, value)) ->
                    {|
                        Upper = value
                        Lower = valueTop
                        Left = leftTop
                        Middle = left
                        Right = right
                    |}
                    |> NeedsRebalance
                | AdditionResult.Black blackNode -> AdditionResult.Red (RedNode (leftTop, blackNode, valueTop))
                | AdditionResult.NeedsRebalance info -> failwith ""
            else
                AdditionResult.AlreadyPresent

    let add<'a when 'a : comparison> (tree : RedBlackTree<'a>) (elt : 'a) : RedBlackTree<'a> =
        match tree with
        | RedBlackTree.RedRoot node -> addRed node elt
        | RedBlackTree.BlackRoot node -> addBlack node elt
        |> function
        | AdditionResult.AlreadyPresent -> tree
        | AdditionResult.Black node -> RedBlackTree.BlackRoot node
        | AdditionResult.Red node -> RedBlackTree.RedRoot node
        | AdditionResult.NeedsRebalance info ->
            RedNode
                (BlackNode.BlackBlackNode (elevateBlack info.Left, elevateBlack info.Middle, info.Lower),
                 info.Right,
                 info.Upper)
            |> RedBlackTree.RedRoot

    let rec private findBlack<'a, 'depth when 'a : comparison>
        (tree : BlackNode<'a, 'depth>)
        (elt : 'a)
        : Choice<RedNodeCrate<'a>, BlackNodeCrate<'a>> option
        =
        match tree with
        | BlackNode.Leaf -> None
        | BlackNode.RedRedNode (left, right, value) ->
            if elt = ValueAtDepth.value value then
                Some (Choice2Of2 (BlackNodeCrate.make tree))
            elif elt < ValueAtDepth.value value then
                findRed left elt
            else
                findRed right elt
        | BlackNode.RedBlackNode (left, right, value) ->
            if elt = ValueAtDepth.value value then
                Some (Choice2Of2 (BlackNodeCrate.make tree))
            elif elt < ValueAtDepth.value value then
                findRed left elt
            else
                findBlack right elt
        | BlackNode.BlackRedNode (left, right, value) ->
            if elt = ValueAtDepth.value value then
                Some (Choice2Of2 (BlackNodeCrate.make tree))
            elif elt < ValueAtDepth.value value then
                findBlack left elt
            else
                findRed right elt
        | BlackNode.BlackBlackNode (left, right, value) ->
            if elt = ValueAtDepth.value value then
                Some (Choice2Of2 (BlackNodeCrate.make tree))
            elif elt < ValueAtDepth.value value then
                findBlack left elt
            else
                findBlack right elt

    and private findRed<'a, 'depth when 'a : comparison>
        (tree : RedNode<'a, 'depth>)
        (elt : 'a)
        : Choice<RedNodeCrate<'a>, BlackNodeCrate<'a>> option
        =
        match tree with
        | RedNode (left, right, value) ->
            if elt = ValueAtDepth.value value then
                Some (Choice1Of2 (RedNodeCrate.make tree))
            elif elt < ValueAtDepth.value value then
                findBlack left elt
            else
                findBlack right elt

    let find<'a when 'a : comparison> (t : RedBlackTree<'a>) (elt : 'a) : bool =
        match t with
        | RedBlackTree.BlackRoot root -> Option.isSome (findBlack root elt)
        | RedBlackTree.RedRoot root -> Option.isSome (findRed root elt)

    let rec private foldBlack<'a, 'state, 'depth when 'a : comparison>
        (folder : 'state -> 'a -> 'state)
        (state : 'state)
        (tree : BlackNode<'a, 'depth>)
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

    and private foldRed<'a, 'state, 'depth when 'a : comparison>
        (folder : 'state -> 'a -> 'state)
        (state : 'state)
        (tree : RedNode<'a, 'depth>)
        : 'state
        =
        match tree with
        | RedNode (left, right, value) ->
            let state = foldBlack folder state left
            let state = folder state (ValueAtDepth.value value)
            foldBlack folder state right

    let fold<'a, 'state when 'a : comparison>
        (folder : 'state -> 'a -> 'state)
        (state : 'state)
        (tree : RedBlackTree<'a>)
        : 'state
        =
        match tree with
        | RedBlackTree.RedRoot root -> foldRed folder state root
        | RedBlackTree.BlackRoot root -> foldBlack folder state root

    let toList<'a when 'a : comparison> (tree : RedBlackTree<'a>) : 'a list =
        fold (fun ls a -> a :: ls) [] tree |> List.rev
