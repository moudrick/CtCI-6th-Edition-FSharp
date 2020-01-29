module ctci.Properties.
    Ch_10._Sorting_and_Searching.
    ``Q10 01 - Sorted Merge``

open FsCheck
open FsCheck.Xunit

open Ch_10._Sorting_and_Searching.``Q10 01 - Sorted Merge``

type Case = { a : int[]; b : int[]; countA : int; countB : int; }
    with static member get a b ca cb =
            { a = a; b = b; countA = ca; countB = cb; }

let cases = seq {
    yield (Case.get [| 1; 3; 5; 0; 0; 0; |]
                    [| 2; 4; 6; |] 3 3,
                    [| 1; 2; 3; 4; 5; 6; |])

    yield (Case.get [| 10; 20; 30; 0; 0; 0; |]
                    [| 40; 50; 60; |] 3 3,
                    [| 10; 20; 30; 40; 50; 60; |])

    yield (Case.get [| 44; 55; 66; 0; 0; 0; |]
                    [| 11; 22; 33; |] 3 3,
                    [| 11; 22; 33; 44; 55; 66; |])

    yield (Case.get [| 100; 0; 0; 0; |]
                    [| 99; 101; 102; |] 1 3,
                    [| 99; 100; 101; 102; |])

    yield (Case.get [| 100; 0; 0; 0; |]
                    [| 99; 100; 101; |] 1 3,
                    [| 99; 100; 100; 101; |])

    yield (Case.get [| 2; 3; 4; 5; 6; 8; 10; 100; 0; 0; 0; 0; 0; 0; |]
                    [| 1; 4; 6; 7; 7; 7; |] 8 6,
                    [| 1; 2; 3; 4; 4; 5; 6; 6; 7; 7; 7; 8; 10; 100 |])
}

type CasesGenArb() =
    static member Arb() = 
        cases |> Gen.elements |> Arb.fromGen

let exceptionalCases = seq {
    yield (Case.get [| 1; 3; 5; 0; 0; 0; |]
                     [| 2; 4; 6; 8; 10; 12; 14; |] 3 7,
                     typeof<System.IndexOutOfRangeException>)
}
 
type ExceptionalCasesGenArb() =
    static member Arb() = 
        exceptionalCases |> Gen.elements |> Arb.fromGen

[< Property(Verbose = true, Arbitrary = [| typeof<CasesGenArb> |]) >]
let ``Sln.merge works properly``
    (case : Case) (expected : int[]) =

    Sln.merge case.a case.b case.countA case.countB 
    case.a = expected

[< Property(Verbose = true, Arbitrary = [| typeof<ExceptionalCasesGenArb> |]) >]
let ``Sln.merge throws exception``
    (case : Case) (exc : System.Type) =

    lazy Xunit.Assert.Throws ( exc, 
            (fun () -> Sln.merge case.a case.b case.countA case.countB |> ignore ) )
        |> ignore

[< Property(Verbose = true, Arbitrary = [| typeof<CasesGenArb> |]) >]
let ``Sln.NoMutable.merge works properly``
    (case : Case) (expected : int[])  =

    Sln.NoMutable.merge case.a case.b case.countA case.countB 
    case.a = expected

[< Property(Verbose = true, Arbitrary = [| typeof<ExceptionalCasesGenArb> |]) >]
let ``Sl.NoMutable.merge throws exception``
    (case : Case) (exc : System.Type) =

    lazy Xunit.Assert.Throws ( exc, 
            (fun () -> Sln.NoMutable.merge case.a case.b case.countA case.countB |> ignore ) )
        |> ignore
