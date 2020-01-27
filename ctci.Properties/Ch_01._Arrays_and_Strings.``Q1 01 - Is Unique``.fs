module ctci.Properties.
    Ch_01._Arrays_and_Strings.
    ``Q1 01 - Is Unique``

open FsCheck
open FsCheck.Xunit

open Ch_01._Arrays_and_Strings.``Q1 01 - Is Unique``

let unique = [| "abcde"; "kite"; "padle"; 
    "abcdefg";
    "abcdefghijklmnopqrstuvwxyz" |]
let nonUnique = [| "hello"; "apple"; 
    "arena"; "well"; "lloyd";
    "level";
    String.replicate 4 "abcdefg"; String.replicate 17 "abcdefgh" |]
let expectUnique a b = (a |> Seq.ofArray |> Seq.map (fun x -> (x, b)))
let isUnique = seq [ expectUnique unique true; expectUnique nonUnique false ]

type IsUniqueGenArb() =
    static member Arb() = 
        isUnique |> Seq.concat |> Gen.elements |> Arb.fromGen

[< Property(Verbose = true, Arbitrary = [| typeof<IsUniqueGenArb> |]) >]
let ``Sln.IsUniqueChars works properly``
    (word : string) (isUnique : bool) =
    Sln.IsUniqueChars word = isUnique

[< Property(Verbose = true, Arbitrary = [| typeof<IsUniqueGenArb> |]) >]
let ``Sln.NoMutable.IsUniqueChars works properly``
    (word : string) (isUnique : bool) =
    Sln.NoMutable.IsUniqueChars word = isUnique

[< Property(Verbose = true, Arbitrary = [| typeof<IsUniqueGenArb> |]) >]
let ``Sln.ExtendedAscii.IsUniqueChars works properly``
    (word : string) (isUnique : bool) =
    Sln.ExtendedAscii.IsUniqueChars word = isUnique

[< Property(Verbose = true, Arbitrary = [| typeof<IsUniqueGenArb> |]) >]
let ``Sln.GenericHashSet.IsUniqueChars solution works properly``
    (word : string) (isUnique : bool) =
    Sln.GenericHashSet.IsUniqueChars word = isUnique

[< Property(Verbose = true, Arbitrary = [| typeof<IsUniqueGenArb> |]) >]
let ``Sln.ImmutableSet.IsUniqueChars works properly``
    (word : string) (isUnique : bool) =
    Sln.ImmutableSet.IsUniqueChars word = isUnique

[< Property(Verbose = true, Arbitrary = [| typeof<IsUniqueGenArb> |]) >]
let ``Sln.ZipWithSort.IsUniqueChars works properly``
    (word : string) (isUnique : bool) =
    Sln.ZipWithSort.IsUniqueChars word = isUnique
