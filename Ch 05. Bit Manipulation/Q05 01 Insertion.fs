namespace Ch_05._Bit_Manipulation.``Q05 01 - Insertion``

open ctci.Library

module Sln =
    let updateBits n m i j = 
            seq {
                if (i > j || i < 0 || j >= 32) then
                    yield 0

                (* Create a mask to clear bits i through j in n
                 * EXAMPLE: i = 2, j = 4. Result should be 11100011.
                 * (Using 8 bits for this example.  This is obviously not actually 8 bits.)
                 *)
                let allOnes = ~~~0 // allOnes = 11111111

                let left = if (j < 31) then (allOnes <<< (j + 1)) else 0 // 1s until position j, then 0s. left = 11100000  
                let right = ((1 <<< i) - 1) // 1s after position i.  right = 00000011
                let mask = left ||| right // All 1s, except for 0s between i and j. mask = 11100011

                (* Clear i through j, then put m in there *)
                let n_cleared = n &&& mask // Clear bits j through i.
                let m_shifted = m <<< i // Move m into correct position.

                (* OR them, and we're done! *)
                yield n_cleared ||| m_shifted
            } |> Seq.head

type Question() =
    inherit ctci.Contracts.Question()

    override this.DemoRun() =
        let a = ~~~23423
        printfn "%s" (AssortedMethods.toFullBinaryString a)
        let b = 5
        printfn "%s" (AssortedMethods.toFullBinaryString b)
        let c = Sln.updateBits a b 29 31
        printfn "%s" (AssortedMethods.toFullBinaryString c)

        printfn ""
        let a1 = 1024
        printfn "%s" (AssortedMethods.toFullBinaryString a1)
        let b1 = 19
        printfn "%s" (AssortedMethods.toFullBinaryString b1)
        let c1 = Sln.updateBits a1 b1 2 6
        printfn "%s" (AssortedMethods.toFullBinaryString c1)
