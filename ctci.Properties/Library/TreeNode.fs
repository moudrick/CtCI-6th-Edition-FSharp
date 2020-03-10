module ctci.Properties.
    Library.TreeNode

open ctci.Library

let treePairs = seq {
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
    let n16 = new TreeNode (16)

    yield (TreeNode.tip, 0)
    yield (n1, 1)

    n1.left <- n2 
    yield (n1, 2)

    n1.right <- n3
    yield (n1, 2)

    n2.left <- n4
    yield (n1, 3)

    n2.right <- n5
    yield (n1, 3) 

    n3.left <- n6
    n3.right <- n7
    yield (n1, 3)

    n6.left <- n8
    n8.left <- n9
    yield (n1, 5)

    n6.right <- n10
    yield (n1, 5)

    n10.left <- n11
    yield (n1, 5)

    n4.left <- n12
    n4.right <- n13
    yield (n1, 5)

    n5.left <- n14
    n5.right <- n15
    yield (n1, 5)

    n7.left <- n16
    yield (n1, 5)
} 

let genTreeHeightPair n = Seq.head <| Seq.skip n treePairs

let cases = seq [ for i in 0 .. (Seq.length treePairs - 1) -> genTreeHeightPair i ]

open FsCheck
open FsCheck.Xunit

type CasesGenArb() = static member Arb() = cases |> Gen.elements |> Arb.fromGen

open Swensen.Unquote

[< Property( Verbose = true, 
    Arbitrary = [| typeof<CasesGenArb> |] ) >]
let ``TreeNode.height works properly`` 
    (n1 : TreeNode, h : int) =

    TreeNode.height n1 =! h
