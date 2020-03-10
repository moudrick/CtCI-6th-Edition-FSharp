module ctci.Properties.
    Ch_04._Trees_and_Graphs.
    ``Q04 04 - Check Balanced``

open ctci.Library
open Ch_04._Trees_and_Graphs.
    ``Q04 04 - Check Balanced``

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

    yield (TreeNode.tip, true)
    yield (n1, true)

    n1.left <- n2 
    yield (n1, true)

    n1.right <- n3
    yield (n1, true)

    n2.left <- n4
    yield (n1, true)

    n2.right <- n5
    yield (n1, true) 

    n3.left <- n6
    n3.right <- n7
    yield (n1, true)

    n6.left <- n8
    n8.left <- n9
    yield (n1, false)

    n6.right <- n10
    yield (n1, false)

    n10.left <- n11
    yield (n1, false)

    n4.left <- n12
    n4.right <- n13
    yield (n1, false)

    n5.left <- n14
    n5.right <- n15
    yield (n1, false)

    n7.left <- n16
    yield (n1, true)

    yield ([| 1 .. 10 |] |> TreeNode.createMinimalBST, true)

    let unbalanced array =
        let unbalanced = new TreeNode 10
        for d in array do unbalanced.insertInOrder d
        unbalanced

    yield ([| 46; 99; 66; 98; 3; 86; 93; 41; 20; 96 |] |> unbalanced, false)
    yield ([| 6; 69; 30; 93; 90; 12; 23; 71; 61; 33 |] |> unbalanced, false)
    yield ([| 38; 91; 13; 52; 15; 27; 97; 50; 65; 100 |] |> unbalanced, false)
    yield ([| 90; 24; 23; 24; 38; 52; 74; 47; 70; 71 |] |> unbalanced, false)
    yield ([| 59; 62; 89; 34; 52; 0; 35; 81; 88; 5 |] |> unbalanced, false)
} 

let genTreeHeightPair n = Seq.head <| Seq.skip n treePairs

let cases = seq [ for i in 0 .. (Seq.length treePairs - 1) -> genTreeHeightPair i ]

open FsCheck
open FsCheck.Xunit

type CasesGenArb() = static member Arb() = cases |> Gen.elements |> Arb.fromGen

open Swensen.Unquote

[<Properties( Verbose = true, 
    Arbitrary = [| typeof<CasesGenArb> |] )>]
module SlnProperties =

    [< Property >]
    let ``Sln.isBalanced works properly`` 
        (n1 : TreeNode, isBalanced : bool) =

        Sln.isBalanced n1 =! isBalanced

    [< Property >]
    let ``Sln.Improved.isBalanced works properly`` 
        (n1 : TreeNode, isBalanced : bool) =

        Sln.Improved.isBalanced n1 =! isBalanced
