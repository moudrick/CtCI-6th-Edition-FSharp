namespace Ch_01._Arrays_and_Strings.``Q1 07 - Rotate Matrix``

open ctci.Library

module Sln =
    let rotate (matrix : int[,]) n =
            for layer in 0 .. n / 2 - 1 do
                let first = layer
                let last = n - 1 - layer

                for i in first .. last - 1 do
                    let offset = i - first
                    let top = matrix.[first, i] // save top

                    // left -> top
                    matrix.[first, i] <- matrix.[last - offset, first]

                    // bottom -> left
                    matrix.[last - offset, first] <- matrix.[last, last - offset]

                    // right -> bottom
                    matrix.[last, last - offset] <- matrix.[i, last]

                    // top -> right
                    matrix.[i, last] <- top // right <- saved top

    module ByRevTranspose = 
        let rotate m = m |> List.rev |> List.transpose

    module ByRevCustomTranspose =
        let rotate matrix = 
            let rec transpose M = 
                match M with 
                | []::_ -> []
                | _ -> List.map List.head M :: transpose (List.map List.tail M)
            matrix |> List.rev |> transpose

type Question() =
    inherit ctci.Contracts.Question()
 
    override this.DemoRun() =

        let size = 3

        let matrix = AssortedMethods.RandomMatrix size size 0 9
       
        AssortedMethods.PrintMatrix matrix

        Sln.rotate matrix size
        printfn ""
        AssortedMethods.PrintMatrix matrix

        printfn ""
        let intListList = matrix |> AssortedMethods.toIntListList
        AssortedMethods.PrintIntListListMatrix (Sln.ByRevTranspose.rotate intListList)

        printfn "---------------------------------"
        let matrix = AssortedMethods.RandomIntListListMatrix size size 0 9
        printfn ""
        AssortedMethods.PrintIntListListMatrix matrix
        printfn ""
        AssortedMethods.PrintIntListListMatrix (Sln.ByRevTranspose.rotate matrix)
        printfn ""
        AssortedMethods.PrintIntListListMatrix (Sln.ByRevCustomTranspose.rotate matrix)
