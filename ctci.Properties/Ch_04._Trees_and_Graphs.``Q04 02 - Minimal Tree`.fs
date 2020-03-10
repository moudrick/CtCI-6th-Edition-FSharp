module ctci.Properties.
    Ch_04._Trees_and_Graphs.
    ``Q04 02 - Minimal Tree``

open ctci.Library

open Swensen.Unquote

let rec preOrderAssertion expected actual =
    if (expected <> TreeNode.tip && actual <> TreeNode.tip) then
        expected.data =! actual.data
        preOrderAssertion expected.left actual.left
        preOrderAssertion expected.right actual.right
    else
        expected =! actual

open FsCheck.Xunit

[< Property(Verbose = true) >]
let ``TreeNode.createMinimalBST works properly`` () =

    let n1 = new TreeNode (1)
    let n2 = new TreeNode (2)
    let n3 = new TreeNode (3)
    let n4 = new TreeNode (4)
    let n5 = new TreeNode (5)
    let n6 = new TreeNode (6)
    let n7 = new TreeNode (7)
    let n8 = new TreeNode (8)
    let n9 = new TreeNode (9)
    let n10 = new TreeNode (10)
    let n11 = new TreeNode (11)
    let n12 = new TreeNode (12)
    let n13 = new TreeNode (13)
    let n14 = new TreeNode (14)
    let n15 = new TreeNode (15)

    let root = n8

    (* We do not need setting parent here *)

    n8.left <- n4
    n8.right <- n12

    n4.left <- n2
    n4.right <- n6

    n12.left <- n10
    n12.right <- n14

    n2.left <- n1
    n2.right <- n3

    n6.left <- n5
    n6.right <- n7

    n10.left <- n9
    n10.right <- n11

    n14.left <- n13
    n14.right <- n15

    let bst = TreeNode.createMinimalBST [| 1 .. 15 |] 

    bst.data =! root.data
    bst.left.data =! root.left.data
    bst.right.data =! root.right.data
    preOrderAssertion root bst 
 