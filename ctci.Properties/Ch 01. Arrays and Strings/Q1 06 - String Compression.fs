module ctci.Properties.
    Ch_01._Arrays_and_Strings.
    ``Q1 06 - String Compression``

open Ch_01._Arrays_and_Strings.
    ``Q1 06 - String Compression``

type Case = { original : string; compressed : string; name : string }
    with static member get o c n = { original = o; compressed = c; name = n }

let cases = seq [
    Case.get "" "" "Empty string"
    Case.get "a" "a" "One symbol"
    Case.get "aa" "aa" "Two same symbols"
    Case.get "aabcccccaaa" "a2b1c5a3" "General case"
    Case.get "compressme" "compressme" "WhenOriginalIsShorter: compressme is shorter than c1o1m1p1r1e1s2m1e1"
    Case.get "aaab" "aaab" "WhenCompressedIsNotShorterButWeDontKnowThatUntilTheEnd: a3b1 is not shorter than aaab"
    Case.get "aabccccca" "a2b1c5a1" "One at end"
    Case.get "abbccccccde" "a1b2c6d1e1" "One at end 2"
]

open FsCheck
open FsCheck.Xunit

type CasesGenArb() = static member Arb() = cases |> Gen.elements |> Arb.fromGen

open Swensen.Unquote

[<Properties( Verbose = true, 
    Arbitrary = [| typeof<CasesGenArb> |] )>]
module SlnProperties =

    [< Property >]
    let ``Sln.Bad.compress works properly``
        (case : Case) =

        case.compressed =! Sln.Bad.compress case.original

    [< Property >]
    let ``Sln.compress works properly``
        (case : Case) =

        case.compressed =! Sln.compress case.original

    [< Property >]
    let ``Sln.Better.compress works properly``
        (case : Case) =

        case.compressed =! Sln.Better.compress case.original

    [< Property >]
    let ``Sln.Better.NoMutable.compress works properly``
        (case : Case) =

        case.compressed =! Sln.Better.NoMutable.compress case.original

    [< Property >]
    let ``Sln.FoldrGroup.compress works properly``
        (case : Case) =

        case.compressed =! Sln.FoldrGroup.compress case.original
