namespace Chapter01

open ctci.Contracts

type ``Q1 01 - Is Unique``() =
    inherit Question()

    let IsUniqueCharsEnglishLower (str : string) : bool =
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

    let IsUniqueCharsEnglishLower_NoMutable (str : string) : bool =
        seq {
            if (str.Length > 26) 
                then yield false

            yield! (seq [ 0 .. (str.Length - 1) ]
                |> Seq.map (fun i -> int str.[i] - int 'a')
                |> Seq.scan 
                    (fun (checker : int Option) v -> 
                        let duplicated = (checker.Value &&& (1 <<< v)) > 0
                        if (duplicated) 
                            then None
                            else Some(checker.Value ||| (1 <<< v))
                    ) (Some 0)
                |> Seq.skipWhile Option.isSome
                |> Seq.map Option.isSome
                |> Seq.tryHead |> Option.toList)
        } |> Seq.tryHead |> defaultArg <| true

    let IsUniqueChars_ExtendedAscii (str : string) : bool =
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

    let IsUniqueChars str =
        seq {
            let hashset = new System.Collections.Generic.HashSet<char>()
            for c in str do
                if (hashset.Contains(c)) then yield false
                hashset.Add(c) |> ignore
            yield true
        } |> Seq.head

    let IsUniqueChars_ImmutableSet str =
        seq {
            yield! str |> Seq.scan
                (fun (hashset : Set<char> option) (c : char) ->
                    if (Set.contains c hashset.Value) 
                        then None
                        else Some(hashset.Value.Add(c))
                ) (Some Set.empty<char>)
            |> Seq.skipWhile Option.isSome
            |> Seq.map Option.isSome
            |> Seq.tryHead |> Option.toList
        } |> Seq.tryHead |> defaultArg <| true

    let IsUniqueChars_ZipWithSort str =
        let differentPairs s = Seq.reduce (&&) (Seq.map2 (<>) s (Seq.tail s))
        let isUnique = differentPairs << Seq.sort
        isUnique str

    override this.Run() =

        let words = [| "abcde"; "hello"; "apple"; "kite"; "padle"; 
            "arena"; "well"; "lloyd"; "abcdefghijklmnopqrstuvwxyz";
            String.replicate 4 "abcdefg"; String.replicate 17 "abcdefgh" |]

        for word in words do
            printfn "%s: %b %b %b %b %b %b" word
                (IsUniqueCharsEnglishLower word) (IsUniqueCharsEnglishLower_NoMutable word)
                (IsUniqueChars word) (IsUniqueChars_ImmutableSet word)
                (IsUniqueChars_ZipWithSort word) (IsUniqueChars_ExtendedAscii word)

        |> ignore
