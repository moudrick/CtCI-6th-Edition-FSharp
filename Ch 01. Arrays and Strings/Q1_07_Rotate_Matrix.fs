namespace Chapter01

open ctci.Contracts
open ctci.Library

type ``Q1 07 - Rotate Matrix``() =
    inherit Question()

    let Rotate (matrix : int[,]) (n) =
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
        ignore()

    let rotateMatrixByRevTranspose m = m |> List.rev |> List.transpose

    let rotateMatrixByRevCustomTranspose matrix = 
        let rec transpose M = 
            match M with 
            | []::_ -> []
            | _ -> List.map List.head M :: transpose (List.map List.tail M)
        matrix |> List.rev |> transpose

    override this.Run() =

        let size = 3

        let matrix = AssortedMethods.RandomMatrix size size 0 9
       
        AssortedMethods.PrintMatrix matrix

        Rotate matrix size
        printfn ""
        AssortedMethods.PrintMatrix matrix

        printfn ""
        let toIntListList = [ for i in 0 .. (matrix |> Array2D.length1) - 1
            do List.ofArray matrix.[i, 0..(matrix |> Array2D.length2) - 1] ]
        AssortedMethods.PrintIntListListMatrix (rotateMatrixByRevTranspose toIntListList)

        printfn "---------------------------------"
        let matrix = AssortedMethods.RandomIntListListMatrix size size 0 9
        printfn ""
        AssortedMethods.PrintIntListListMatrix matrix
        printfn ""
        AssortedMethods.PrintIntListListMatrix (rotateMatrixByRevTranspose matrix)
        printfn ""
        AssortedMethods.PrintIntListListMatrix (rotateMatrixByRevCustomTranspose matrix)

        ignore()
