namespace ctci.Library

(* One node of a binary tree. The data element stored is a single 
 * character.
 *)
type TreeNode =
    static member public tip = Unchecked.defaultof<TreeNode>
    val public data : int
    [<DefaultValue>] val mutable public left : TreeNode    
    [<DefaultValue>] val mutable public right : TreeNode 
    [<DefaultValue>] val mutable public parent : TreeNode
    val mutable private size : int

    new (d) = {
        data = d
        size = 1
    }

    member this.setLeftChild (left : TreeNode) =
        this.left <- left
        if (left <> TreeNode.tip) then
            left.parent <- this

    member this.setRightChild (right : TreeNode) =
        this.right <- right
        if (right <> TreeNode.tip) then
            right.parent <- this

    member this.Size with get() = this.size

    member this.insertInOrder d =
        if (d <= this.data) then
            if (this.left = TreeNode.tip) then
                this.setLeftChild(new TreeNode(d))
            else
                this.left.insertInOrder(d)
        else
            if (this.right = TreeNode.tip) then
                this.setRightChild(new TreeNode(d))
            else
                this.right.insertInOrder(d)
        this.size <- this.size + 1

module TreeNode =
    let rec height (this : TreeNode) =
        let leftHeight = if (this.left <> TreeNode.tip) then (height this.left) else 0
        let rightHeight = if (this.right <> TreeNode.tip) then (height this.right) else 0
        1 + max leftHeight rightHeight

    let rec isBST (this : TreeNode) =
        match this with
        | _ when (this.left <> TreeNode.tip)
            && (this.data < this.left.data || not (isBST this.left))
                -> false
        | _ when (this.right <> TreeNode.tip)
            && (this.data >= this.right.data || not (isBST this.right))
                -> false
        | _ -> true

    let rec find d (this : TreeNode) =
        if (d = this.data) then
            this
        else if (d <= this.data) then
            if (this.left <> TreeNode.tip) then this.left |> find d else TreeNode.tip
        else if (d > this.data) then
            if (this.right <> TreeNode.tip) then this.right |> find d else TreeNode.tip
        else TreeNode.tip

    let rec private createMinimalBSTrec arr start' end' =
        if (end' < start') then
            TreeNode.tip
        else
            let mid' = (start' + end') / 2
            let node = new TreeNode (Array.get arr mid')
            node.setLeftChild  ( createMinimalBSTrec arr start' (mid' - 1) )
            node.setRightChild ( createMinimalBSTrec arr (mid' + 1) end' )
            node

    let createMinimalBST array = createMinimalBSTrec array 0 (array.Length - 1)
