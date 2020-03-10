module ctci.Properties.
    Ch_05._Bit_Manipulation.
    ``Q05 01 - Insertion``

open Ch_05._Bit_Manipulation.
    ``Q05 01 - Insertion``

type Case = { n : int; m : int; i : int; j : int; expected : int; }
    with static member get n m i j e =
            { n = n; m = m; i = i; j = j; expected = e; }

let cases = seq [
    Case.get          1024      19 2 6 0b10001001100
    Case.get 0b10000000000 0b10011 2 6 0b10001001100
    Case.get 0b10001111100 0b10011 2 6 0b10001001100

    Case.get                           ~~~23423     5 29 31 0b10111111111111111010010010000000
    Case.get 0b11111111111111111010010010000000 0b101 29 31 0b10111111111111111010010010000000
]

open FsCheck
open FsCheck.Xunit

type CasesGenArb() = static member Arb() = cases |> Gen.elements |> Arb.fromGen

open Swensen.Unquote

[< Property(Verbose = true, 
    Arbitrary = [| typeof<CasesGenArb> |]) >]
let ``Sln.updateBits works properly``
    (case : Case) =

    Sln.updateBits case.n case.m case.i case.j =! case.expected
