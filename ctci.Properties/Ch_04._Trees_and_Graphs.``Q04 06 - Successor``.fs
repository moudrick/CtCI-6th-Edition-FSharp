module ctci.Properties.
    Ch_04._Trees_and_Graphs.
    ``Q04 06 - Successor``

open ctci.Library
open Ch_04._Trees_and_Graphs.
    ``Q04 06 - Successor``

let node15Pairs = seq {
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

    (* We need setting parent here *)

    n8.setLeftChild n4
    n8.setRightChild n12

    n4.setLeftChild n2
    n4.setRightChild n6

    n12.setLeftChild n10
    n12.setRightChild n14

    n2.setLeftChild n1
    n2.setRightChild n3

    n6.setLeftChild n5
    n6.setRightChild n7

    n10.setLeftChild n9
    n10.setRightChild n11

    n14.setLeftChild n13
    n14.setRightChild n15

    yield (n1, n2)
    yield (n2, n3)
    yield (n3, n4)
    yield (n4, n5)
    yield (n5, n6)
    yield (n6, n7)
    yield (n7, n8)
    yield (n8, n9)
    yield (n9, n10)
    yield (n10, n11)
    yield (n11, n12)
    yield (n12, n13)
    yield (n13, n14)
    yield (n14, n15)
    yield (n15, TreeNode.tip)
} 

let node10Pairs =
    let root = TreeNode.createMinimalBST [| 1 .. 10 |]

    let rec inOrderCollect list node =
        if (node <> TreeNode.tip) then
            (inOrderCollect list node.left)
            @ [ node ] 
            @ (inOrderCollect list node.right)
        else
            [] 
    List.pairwise <| inOrderCollect [] root

let cases = Seq.concat [ node15Pairs; List.toSeq node10Pairs; ]

open FsCheck
open FsCheck.Xunit

type CasesGenArb() = static member Arb() = cases |> Gen.elements |> Arb.fromGen

open Swensen.Unquote

[<Properties( Verbose = true, 
    Arbitrary = [| typeof<CasesGenArb> |] )>]
module SlnProperties =

    [< Property >]
    let ``Sln.inorderSucc works properly`` 
        (predecessor : TreeNode, expectedSuccessor : TreeNode) =

        let actualSuccessor = Sln.inorderSucc predecessor
        expectedSuccessor =! actualSuccessor

    [< Property >]
    let ``Sln.NoMutable.inorderSucc works properly`` 
        (predecessor : TreeNode, expectedSuccessor : TreeNode) =

        let actualSuccessor = Sln.NoMutable.inorderSucc predecessor
        expectedSuccessor =! actualSuccessor
