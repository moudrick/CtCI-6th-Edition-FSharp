namespace Ch_01._Arrays_and_Strings.``Q1 01 - Is Unique``

module Sln =
    let isUniqueChars (str : string) : bool =
            seq {
                if (str.Length > 26)
                    then yield false

                let mutable checker = 0
                for i in 0 .. (str.Length - 1) do
                    let v = int str.[i] - int 'a'

                    if ((checker &&& (1 <<< v)) > 0) 
                        then yield false
                    checker <- checker ||| (1 <<< v);
                yield true
            } |> Seq.head

    module NoMutable =
        let isUniqueChars str =
            seq {
                let length = str |> String.length
                if (length > 26) 
                    then yield false

                yield! (seq [ 0 .. (length - 1) ]
                    |> Seq.map (fun i -> int str.[i] - int 'a')
                    |> Seq.scan 
                        (fun checker v -> 
                            let duplicated = (checker |> Option.get &&& (1 <<< v)) > 0
                            if (duplicated) 
                                then None
                                else Some(checker.Value ||| (1 <<< v))
                        ) (Some 0)
                    |> Seq.skipWhile Option.isSome
                    |> Seq.map Option.isSome
                    |> Seq.tryHead |> Option.toList)
            } |> Seq.tryHead |> defaultArg <| true

    module ExtendedAscii =
        let isUniqueChars (str : string) : bool =
            seq {
                if (str.Length > 256) 
                    then yield false

                let char_set = Array.zeroCreate 256
                for i in 0 .. str.Length - 1 do
                    let val1 = int str.[i];
                    if (char_set.[val1]) then yield false
                    char_set.[val1] <- true;

                yield true
            } |> Seq.head

    module GenericHashSet =
        let isUniqueChars str =
            seq {
                let hashset = new System.Collections.Generic.HashSet<char>()
                for c in str do
                    if (hashset.Contains(c)) then yield false
                    hashset.Add(c) |> ignore
                yield true
            } |> Seq.head

    module ImmutableSet =
        let isUniqueChars str =
            seq {
                yield! str |> Seq.scan
                    (fun set c ->
                        if (set |> Option.get |> Set.contains c) 
                            then None
                            else Some(set |> Option.get |> Set.add c)
                    ) (Some Set.empty<char>)
                |> Seq.skipWhile Option.isSome
                |> Seq.map Option.isSome
                |> Seq.tryHead |> Option.toList
            } |> Seq.tryHead |> defaultArg <| true

    module ZipWithSort =
        let isUniqueChars str =
            let differentPairs s = Seq.reduce (&&) (Seq.map2 (<>) s (Seq.tail s))
            let isUnique = differentPairs << Seq.sort
            isUnique str

type Question() =
    inherit ctci.Contracts.Question()

    override this.DemoRun() =

        let words = [| "abcde"; "hello"; "apple"; "kite"; "padle"; 
            "arena"; "well"; "lloyd"; "abcdefghijklmnopqrstuvwxyz";
            String.replicate 4 "abcdefg"; String.replicate 17 "abcdefgh" |]

        for word in words do
            printfn "%s: %b %b %b %b %b %b" word
                (Sln.isUniqueChars word) (Sln.NoMutable.isUniqueChars word)
                (Sln.GenericHashSet.isUniqueChars word) (Sln.ImmutableSet.isUniqueChars word)
                (Sln.ZipWithSort.isUniqueChars word) (Sln.ExtendedAscii.isUniqueChars word)
