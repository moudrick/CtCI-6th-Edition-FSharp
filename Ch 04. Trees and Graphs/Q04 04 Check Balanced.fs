namespace ``Ch 04``.`` Trees and Graphs``.
    ``Q04 04 - Check Balanced``

open ctci.Library

module Sln =
    let rec private getHeight root =
        if (root = TreeNode.tip)
        then -1
        else max (root.left |> getHeight) (root.right |> getHeight) + 1

    let rec isBalanced root =
        if (root = TreeNode.tip)
        then true
        else
            let heightDiff = getHeight root.left - getHeight root.right
            if (abs (heightDiff) > 1)
            then false
            else isBalanced root.left && isBalanced root.right

    module Improved =
        type Result = | ErrorUp | Height of int
        let EmptyBalanced = Height -1

        let rec private checkHeight root =
            if (root = TreeNode.tip) then EmptyBalanced
            else 
                match checkHeight root.left with
                | ErrorUp -> ErrorUp // Propagate error up
                | Height leftHeight ->
                    match checkHeight root.right with
                    | ErrorUp -> ErrorUp // Propagate error up
                    | Height rightHeight ->
                        let heightDiff = leftHeight - rightHeight
                        if (abs heightDiff > 1) 
                        then ErrorUp // Found error -> pass it back
                        else Height (1 + (max leftHeight rightHeight))

        let isBalanced root = checkHeight root <> ErrorUp

type Question() =
    inherit ctci.Contracts.Question()

    override this.DemoRun() =
        // Create balanced tree
        let array =  [| 1 .. 10 |]
        let root = TreeNode.createMinimalBST array
        printfn "Root? %i" root.data
        printfn "Is balanced? %b" (Sln.isBalanced root)

        // Could be balanced, actually, but it's very unlikely...
        let unbalanced = new TreeNode 10
        for i in 0 .. 9 do
            unbalanced.insertInOrder (AssortedMethods.RandomIntInRange 0 100)
        printfn "Root? %i" unbalanced.data
        printfn "Is balanced? %b" (Sln.isBalanced unbalanced)

        printfn "Sln.Improved"
        let root = TreeNode.createMinimalBST array
        printfn "Is balanced? %b" (Sln.Improved.isBalanced root)
        root.insertInOrder 4 // Add 4 to make it unbalanced

        printfn "Is balanced? %b" (Sln.Improved.isBalanced root)
