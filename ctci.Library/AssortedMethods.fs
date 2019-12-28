namespace ctci.Library

module AssortedMethods =

    let private RandomIntNumbers = new System.Random()

    let RandomInt n = RandomIntNumbers.Next n
    let RandomIntInRange (min, max) = RandomInt (max + 1 - min) + min

    let RandomMatrix m n min max =
        let matrix = Array2D.zeroCreate m n
        for i in 0 .. m - 1 do
            for j in 0 .. n - 1 do
                matrix.[i,j] <- RandomIntInRange (min, max)
        matrix

    let RandomIntListListMatrix m n min max =
        [ for i in 0 .. m - 1 do
            yield [ for j in 0 .. n - 1 do
                    yield RandomIntInRange (min, max) ] ]
 
    let PrintMatrix matrix =
        for i in 0 .. (matrix |> Array2D.length1) - 1 do
            for j in 0 .. (matrix |> Array2D.length2) - 1 do
                let mij = matrix.[i, j]
                if (mij < 10 && mij > -10) then
                    printf " "
                if (mij < 100 && mij > -100) then
                    printf " "
                if (mij >= 0) then
                    printf " "
                printf " %d" mij
            printfn ""

    let PrintIntListListMatrix matrix =
        for i in 0 .. (matrix |> List.length) - 1 do
            for j in 0 .. (matrix.[i] |> List.length) - 1 do
                let mij = matrix.[i].[j]
                if (mij < 10 && mij > -10) then
                    printf " "
                if (mij < 100 && mij > -100) then
                    printf " "
                if (mij >= 0) then
                    printf " "
                printf " %d" mij
            printfn ""
