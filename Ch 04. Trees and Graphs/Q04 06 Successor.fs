namespace Ch_04._Trees_and_Graphs.``Q04 06 - Successor``

open ctci.Library

module Sln =
    let private leftMostChild n = Seq.head <| seq {
        if (n = TreeNode.tip) then
            yield TreeNode.tip
        let mutable n' = n
        while (n'.left <> TreeNode.tip) do
            n' <- n'.left 
        yield n' }

    let inorderSucc n = Seq.head <| seq {
        if (n = TreeNode.tip) then yield TreeNode.tip

        // Found right children -> return left most node of right subtree
        if (n.parent = TreeNode.tip || n.right <> TreeNode.tip) then 
            yield leftMostChild n.right 
        else 
            let mutable q = n
            let mutable x = q.parent

            // Go up until we're on left instead of right
            while (x <> TreeNode.tip && x.left <> q) do
                q <- x
                x <- x.parent

            yield x }

    module NoMutable = 
        let private leftMostChild n = 
            let rec rec' (n' : TreeNode) =
                if (n'.left = TreeNode.tip)
                then n'
                else rec' n'.left
            if (n = TreeNode.tip) 
            then TreeNode.tip
            else rec' n 

        let goUpUntilOnLeft (n : TreeNode) =
            // Go up until we're on left instead of right
            let rec rec' (q, x) =
                if (x = TreeNode.tip || x.left = q)
                then (q, x)
                else rec' (x, x.parent)
            let (_, x) = rec' (n, n.parent)
            x

        let inorderSucc n = 
            if (n = TreeNode.tip) 
            then TreeNode.tip
            else
                // Found right children -> return left most node of right subtree
                if (n.parent = TreeNode.tip || n.right <> TreeNode.tip) then 
                    leftMostChild n.right 
                else
                    goUpUntilOnLeft n

type Question() =
    inherit ctci.Contracts.Question()

    override this.DemoRun() =
        let array =  [| 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 |]

        // We needed this code for other files, so check out the code in the library
        let root = TreeNode.createMinimalBST array

        for i in 0 .. array.Length - 1 do
            let node = root |> TreeNode.find array.[i]
            let next = Sln.inorderSucc(node);
            if (next <> TreeNode.tip) then
                printfn "%i -> %i" node.data next.data
            else
                printfn "%i -> %A" node.data TreeNode.tip

        for i in 0 .. array.Length - 1 do
            let node = root |> TreeNode.find array.[i]
            let next = Sln.NoMutable.inorderSucc(node);
            if (next <> TreeNode.tip) then
                printfn "%i -> %i" node.data next.data
            else
                printfn "%i -> %A" node.data TreeNode.tip
