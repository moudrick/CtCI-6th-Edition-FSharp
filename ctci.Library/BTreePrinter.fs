namespace ctci.Library

module BTreePrinter =

    let printNode root =

        let printWhitespaces count =
            for i in 0 .. count - 1 do
                printf " "

        let printNodesData nodes betweenSpaces =
            nodes |> Seq.fold (fun acc node ->
                if (node <> TreeNode.tip) then
                    printf "%i" (node.data); printWhitespaces betweenSpaces
                    (node.right) :: (node.left) :: acc
                else
                    printf " "; printWhitespaces betweenSpaces
                    (TreeNode.tip) :: (TreeNode.tip) :: acc
                ) []
            |> Seq.rev

        let printEdgeLines nodes edgeLines firstSpaces =
            for i in 1 .. edgeLines do

                for j in 0 .. (nodes |> List.length) - 1 do
                    printWhitespaces (firstSpaces - i)
                    if (nodes.[j] = TreeNode.tip) then
                        printWhitespaces (edgeLines + edgeLines + i + 1)
                    else
                        if (nodes.[j].left <> TreeNode.tip)
                        then printf "/"
                        else printWhitespaces 1

                        printWhitespaces (i + i - 1)

                        if (nodes.[j].right <> TreeNode.tip)
                        then printf "\\"
                        else printWhitespaces 1

                        printWhitespaces (edgeLines + edgeLines - i)
                printfn ""

        let rec printNodeInternal nodes level maxLevel =
            let isEmpty = nodes |> List.isEmpty
            let allTip = List.forall (fun n -> n = TreeNode.tip) nodes
            if (not isEmpty && not allTip)
            then
                let floor = (float) maxLevel - (float) level
                let endgeLines = int (2.0 ** max (floor - 1.0) 0.0)
                let firstSpaces = int (2.0 ** floor) - 1
                let betweenSpaces = int (2.0 ** (floor + 1.0)) - 1

                printWhitespaces firstSpaces
                let newNodes = printNodesData nodes betweenSpaces
                printfn ""
                printEdgeLines nodes endgeLines firstSpaces

                printNodeInternal (newNodes |> Seq.toList) (level + 1) maxLevel

        let rec maxLevel node =
            if (node = TreeNode.tip)
            then 0
            else 1 + max (maxLevel (node.left)) (maxLevel (node.right) )

        printNodeInternal [ root ] 1 (maxLevel root)
