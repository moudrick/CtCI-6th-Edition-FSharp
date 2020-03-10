module ctci.Properties.
    Ch_01._Arrays_and_Strings.
    ``Q1 01 - Is Unique``

open Ch_01._Arrays_and_Strings.``Q1 01 - Is Unique``

let isUnique =
    let unique = [| "abcde"; "kite"; "padle"; 
        "abcdefg";
        "abcdefghijklmnopqrstuvwxyz" |]

    let nonUnique = [| "hello"; "apple"; 
        "arena"; "well"; "lloyd";
        "level";
        String.replicate 4 "abcdefg"; String.replicate 17 "abcdefgh" |]

    let expectUnique a b = a |> Seq.map (fun x -> (x, b))

    Seq.concat [ expectUnique unique true; expectUnique nonUnique false ]

open FsCheck
open FsCheck.Xunit

type IsUniqueGenArb() = static member Arb() = isUnique |> Gen.elements |> Arb.fromGen

open Swensen.Unquote

[<Properties( Verbose = true, 
    Arbitrary = [| typeof<IsUniqueGenArb> |] )>]
module SlnProperties =

    [< Property >]
    let ``Sln.isUniqueChars works properly``
        (word : string) (isUnique : bool) =

        Sln.isUniqueChars word =! isUnique

    [< Property >]
    let ``Sln.NoMutable.isUniqueChars works properly``
        (word : string) (isUnique : bool) =

        Sln.NoMutable.isUniqueChars word =! isUnique

    [< Property >]
    let ``Sln.ExtendedAscii.isUniqueChars works properly``
        (word : string) (isUnique : bool) =

        Sln.ExtendedAscii.isUniqueChars word =! isUnique

    [< Property >]
    let ``Sln.GenericHashSet.isUniqueChars solution works properly``
        (word : string) (isUnique : bool) =

        Sln.GenericHashSet.isUniqueChars word =! isUnique

    [< Property >]
    let ``Sln.ImmutableSet.isUniqueChars works properly``
        (word : string) (isUnique : bool) =

        Sln.ImmutableSet.isUniqueChars word =! isUnique

    [< Property >]
    let ``Sln.ZipWithSort.isUniqueChars works properly``
        (word : string) (isUnique : bool) =

        Sln.ZipWithSort.isUniqueChars word =! isUnique
