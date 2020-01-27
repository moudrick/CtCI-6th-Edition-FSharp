module ctci.Properties.Ch_01._Arrays_and_Strings.``Q1 01 - Is Unique``

open FsCheck
open FsCheck.Xunit

open Ch_01._Arrays_and_Strings.``Q1 01 - Is Unique``

let unique = [| "abcde"; "kite"; "padle"; "abcdefghijklmnopqrstuvwxyz" |]
let nonUnique = [| "hello"; "apple"; 
    "arena"; "well"; "lloyd"; 
    String.replicate 4 "abcdefg"; String.replicate 17 "abcdefgh" |]
let expectUnique a b = (a |> Seq.ofArray |> Seq.map (fun x -> (x, b)))
let isUnique = seq [ expectUnique unique true; expectUnique nonUnique false ]

type IsUniqueGen() =
    static member IsUniqueWords() = 
        isUnique |> Seq.concat |> Gen.elements |> Arb.fromGen

[< Property(Verbose = true, Arbitrary = [| typeof<IsUniqueGen> |]) >]
let ``EnglishLower solution works properly``
    (word : string) (isUnique : bool) =
    Sln.IsUniqueChars word = isUnique

[< Property(Verbose = true, Arbitrary = [| typeof<IsUniqueGen> |]) >]
let ``EnglishLower_NoMutable solution works properly``
    (word : string) (isUnique : bool) =
    Sln.NoMutable.IsUniqueChars word = isUnique

[< Property(Verbose = true, Arbitrary = [| typeof<IsUniqueGen> |]) >]
let ``ExtendedAscii solution works properly``
    (word : string) (isUnique : bool) =
    Sln.ExtendedAscii.IsUniqueChars word = isUnique

[< Property(Verbose = true, Arbitrary = [| typeof<IsUniqueGen> |]) >]
let ``GenericHashSet solution works properly``
    (word : string) (isUnique : bool) =
    Sln.GenericHashSet.IsUniqueChars word = isUnique

[< Property(Verbose = true, Arbitrary = [| typeof<IsUniqueGen> |]) >]
let ``ImmutableSet solution works properly``
    (word : string) (isUnique : bool) =
    Sln.ImmutableSet.IsUniqueChars word = isUnique

[< Property(Verbose = true, Arbitrary = [| typeof<IsUniqueGen> |]) >]
let ``ZipWithSort solution works properly``
    (word : string) (isUnique : bool) =
    Sln.ZipWithSort.IsUniqueChars word = isUnique
