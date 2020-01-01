namespace ctci.Library

module AssortedMethods =

    let private RandomIntNumbers = new System.Random()

    let RandomInt n = RandomIntNumbers.Next n
    let RandomIntInRange min max = RandomInt (max + 1 - min) + min

    let RandomMatrix m n min max =
        Array2D.init m n (fun i j -> RandomIntInRange min max)

    let RandomIntListListMatrix m n min max =
        List.init m (fun i -> (List.init n (fun j -> RandomIntInRange min max)))

    let PrintMatrixItem mij =
        if (mij < 10 && mij > -10) then
            printf " "
        if (mij < 100 && mij > -100) then
            printf " "
        if (mij >= 0) then
            printf " "
        printf " %d" mij
 
    let PrintMatrix matrix =
        for i in 0 .. (matrix |> Array2D.length1) - 1 do
            for j in 0 .. (matrix |> Array2D.length2) - 1 do
                matrix.[i, j] |> PrintMatrixItem
            printfn ""

    let PrintIntListListMatrix matrix =
        for i in 0 .. (matrix |> List.length) - 1 do
            for j in 0 .. (matrix.[i] |> List.length) - 1 do
                matrix.[i].[j] |> PrintMatrixItem
            printfn ""
