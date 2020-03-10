namespace ``Ch 04``.`` Trees and Graphs``.
    ``Q04 - Introduction``

open ctci.Library

module Traversals =
    let rec inOrderTraversal visit node =
        if (node <> TreeNode.tip) then
            inOrderTraversal visit node.left
            node |> visit
            inOrderTraversal visit node.right

    let rec preOrderTraversal visit node =
        if (node <> TreeNode.tip) then
            visit node
            preOrderTraversal visit node.left
            preOrderTraversal visit node.right

    let rec postOrderTraversal visit node =
        if (node <> TreeNode.tip) then
            postOrderTraversal visit node.left
            postOrderTraversal visit node.right
            visit node

open Traversals

type Question() =
    inherit ctci.Contracts.Question()

    override this.DemoRun() =
        let printData node = 
            if (node <> TreeNode.tip)
            then printfn "%i" (node.data)

        let array =  [| 1 .. 10 |]

        // We needed this code for other files, so check out the code in the library
        let root = TreeNode.createMinimalBST array

        BTreePrinter.printNode root
        root |> inOrderTraversal printData
        printfn ""
        root |> preOrderTraversal printData
        printfn ""
        root |> postOrderTraversal printData
        printfn ""

        let array =  [| 0 .. 12 |]

        // We needed this code for other files, so check out the code in the library
        let root = TreeNode.createMinimalBST array

        BTreePrinter.printNode root
        root |> inOrderTraversal printData
        printfn ""
        root |> preOrderTraversal printData
        printfn ""
        root |> postOrderTraversal printData
        printfn ""
