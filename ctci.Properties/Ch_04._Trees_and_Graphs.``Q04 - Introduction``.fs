﻿module ctci.Properties.
    Ch_04._Trees_and_Graphs.
    ``Q04 - Introduction``

open FsCheck
open FsCheck.Xunit
open Swensen.Unquote

open ctci.Library

open Ch_04._Trees_and_Graphs.
    ``Q04 - Introduction``

type Case = { root : TreeNode; inOrder : string; preOrder : string; postOrder : string }

let cases = seq [
    {
        root = [| 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 |] |> TreeNode.createMinimalBST ;
        inOrder = " 1 2 3 4 5 6 7 8 9 10";
        preOrder = " 5 2 1 3 4 8 6 7 9 10";
        postOrder = " 1 4 3 2 7 6 10 9 8 5";
    };
    {
        root = [| 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12 |] |> TreeNode.createMinimalBST ;
        inOrder = " 1 2 3 4 5 6 7 8 9 10 11 12";
        preOrder = " 6 3 1 2 4 5 9 7 8 11 10 12";
        postOrder = " 2 1 5 4 3 8 7 10 12 11 9 6";
    };
]

type CasesGenArb() = static member Arb() = cases |> Gen.elements |> Arb.fromGen

type TreeVisitor() =
    let mutable data = ""
    member this.collectData node = 
        if (node <> TreeNode.tip)
        then data <- sprintf "%s %i" data (node.data)
    member this.collectedData with get() = data

[<Properties( Verbose = true, 
    Arbitrary = [| typeof<CasesGenArb> |] )>]
module SlnProperties =

    [< Property >]
    let ``Traversals.inOrderTraversal works properly`` (case : Case) =
        let treeVisitor = new TreeVisitor()

        case.root |> Traversals.inOrderTraversal treeVisitor.collectData

        treeVisitor.collectedData =! case.inOrder

    [< Property >]
    let ``Traversals.preOrderTraversal works properly`` (case : Case) =
        let treeVisitor = new TreeVisitor()

        case.root |> Traversals.preOrderTraversal treeVisitor.collectData

        treeVisitor.collectedData =! case.preOrder

    [< Property >]
    let ``Traversals.postOrderTraversal works properly`` (case : Case) =
        let treeVisitor = new TreeVisitor()

        case.root |> Traversals.postOrderTraversal treeVisitor.collectData

        treeVisitor.collectedData =! case.postOrder
