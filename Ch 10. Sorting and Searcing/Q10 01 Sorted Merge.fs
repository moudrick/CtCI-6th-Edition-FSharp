﻿namespace Ch_10._Sorting_and_Search

open ctci.Contracts

type ``Q10 01 - Sorted Merge``() =
    inherit Question()

    /// <summary>
    /// Merges array
    /// </summary>
    /// <param name="a">a first array</param>
    /// <param name="b">a second array</param>
    /// <param name="countA">number of "real" elements in a</param>
    /// <param name="countB">number of "real" elements in b</param>
    let merge a b countA countB =
        let mutable indexMerged = countB + countA - 1 (* Index of last location of merged array *)
        let mutable indexA = countA - 1 (* Index of last element in array b *)
        let mutable indexB = countB - 1 (* Index of last element in array a *)

        (* Merge a and b, starting from the last element in each *)
        while (indexB >= 0) do
            if (indexA >= 0 && (a |> Array.get <| indexA) > (b |> Array.get <| indexB)) (* end of A is bigger than end of B *)
            then
                a.[indexMerged] <- a.[indexA] // copy element
                indexA <- indexA - 1 
            else
                a.[indexMerged] <- b.[indexB]; // copy element
                indexB <- indexB - 1
            indexMerged <- indexMerged - 1 // move indices

    override this.Run() =
        let a = [| 2; 3; 4; 5; 6; 8; 10; 100; 0; 0; 0; 0; 0; 0 |]
        let b = [| 1; 4; 7; 6; 7; 7 |];
        merge a b 8 6
        printfn "%A" a
