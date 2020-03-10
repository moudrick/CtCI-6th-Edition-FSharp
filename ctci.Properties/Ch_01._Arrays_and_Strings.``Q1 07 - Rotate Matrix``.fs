module ctci.Properties.
    Ch_01._Arrays_and_Strings.
    ``Q1 07 - Rotate Matrix``

open ctci.Library
open Ch_01._Arrays_and_Strings.
    ``Q1 07 - Rotate Matrix``

let matrices =
    seq {
    let testRotate2x2Matrix = 
              ( array2D [| [| 1; 2 |]; 
                           [| 3; 4 |] 
                        |], 
                array2D [| [| 3; 1 |]; 
                           [| 4; 2 |] 
                        |] )

    let testRotate3x3Matrix =
              ( array2D [| [| 1; 2; 3 |];
                           [| 4; 5; 6 |];
                           [| 7; 8; 9 |]
                        |],
                array2D [| [| 7; 4; 1 |];
                           [| 8; 5; 2 |];
                           [| 9; 6; 3 |]
                        |] )

    let testRotate4x4Matrix =
              ( array2D [| [|  1;  2;  3;  4 |];
                           [|  5;  6;  7;  8 |];
                           [|  9; 10; 11; 12 |];
                           [| 13; 14; 15; 16 |]
                        |],
                array2D [| [| 13;  9;  5;  1 |];
                           [| 14; 10;  6;  2 |];
                           [| 15; 11;  7;  3 |];
                           [| 16; 12;  8;  4 |]
                        |] )

    let testRotate5x5Matrix =
              ( array2D [| [|  1;  2;  3;  4;  5 |];
                           [|  6;  7;  8;  9; 10 |];
                           [| 11; 12; 13; 14; 15 |];
                           [| 16; 17; 18; 19; 20 |];
                           [| 21; 22; 23; 24; 25 |]
                        |],
                array2D [| [| 21; 16; 11;  6; 1 |];
                           [| 22; 17; 12;  7; 2 |];
                           [| 23; 18; 13;  8; 3 |];
                           [| 24; 19; 14;  9; 4 |];
                           [| 25; 20; 15; 10; 5 |]
                        |] )

    let testRotate6x6Matrix =
              ( array2D [| [|  1;  2;  3;  4;  5;  6 |];
                           [|  7;  8;  9; 10; 11; 12 |];
                           [| 13; 14; 15; 16; 17; 18 |];
                           [| 19; 20; 21; 22; 23; 24 |];
                           [| 25; 26; 27; 28; 29; 30 |];
                           [| 31; 32; 33; 34; 35; 36 |]
                        |],
                array2D [| [| 31; 25; 19; 13;  7;  1 |];
                           [| 32; 26; 20; 14;  8;  2 |];
                           [| 33; 27; 21; 15;  9;  3 |];
                           [| 34; 28; 22; 16; 10;  4 |];
                           [| 35; 29; 23; 17; 11;  5 |];
                           [| 36; 30; 24; 18; 12;  6 |]
                        |] )

    yield testRotate2x2Matrix
    yield testRotate3x3Matrix
    yield testRotate4x4Matrix
    yield testRotate5x5Matrix
    yield testRotate6x6Matrix
    }

open FsCheck
open FsCheck.Xunit

type ExpectedRotateMatrixGenArb() = static member Matrices() = matrices |> Gen.elements |> Arb.fromGen

type ExpectedRotateIntListListGenArb() =
    static member Matrices() = 
        matrices 
        |> Seq.map (fun t -> (t |> fst |> AssortedMethods.toIntListList, 
                              t |> snd |> AssortedMethods.toIntListList) )  
        |> Gen.elements 
        |> Arb.fromGen

open Swensen.Unquote

[<Properties( Verbose = true, 
    Arbitrary = [| typeof<ExpectedRotateMatrixGenArb> |] )>]
module SlnProperties =
    [< Property >]
    let ``Sln.rotate works properly``
        (matrix : int[,]) (expectedRotatedMatrix : int[,]) =

        matrix |> Sln.rotate <| Array2D.length1 matrix

        matrix =! expectedRotatedMatrix

[<Properties( Verbose = true, 
    Arbitrary = [| typeof<ExpectedRotateIntListListGenArb> |] )>]
module SlnIntListListProperties =
    [< Property >]
    let ``Sln.ByRevTranspose.rotate works properly``
        (matrix : int list list) (expectedRotatedMatrix : int list list) =

        matrix |> Sln.ByRevTranspose.rotate =! expectedRotatedMatrix

    [< Property >]
    let ``Sln.ByRevCustomTranspose.rotate works properly``
        (matrix : int list list) (expectedRotatedMatrix : int list list) =

        matrix |> Sln.ByRevCustomTranspose.rotate =! expectedRotatedMatrix
