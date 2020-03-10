namespace ``Ch 04``.`` Trees and Graphs``.
    ``Q04 02 - Minimal Tree``

open ctci.Library

type Question() =
    inherit ctci.Contracts.Question()

    override this.DemoRun() =
        let array =  [| 1 .. 10 |]

        // We needed this code for other files, so check out the code in the library
        let root = TreeNode.createMinimalBST array
        printfn "Root? %i" root.data
        printfn "Created BST? %b" (root |> TreeNode.isBST)
        printfn "Height: %i" (root |> TreeNode.height)
