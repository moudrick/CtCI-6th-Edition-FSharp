namespace Ch_01._Arrays_and_Strings.``Q1 06 - String Compression``

module Sln =

    open System.Text

    module Bad =
        let compress str =
            seq {
                let mutable compressedString = ""
                let mutable countConsecutive = 0
                let length = str |> String.length

                for i in 0 .. length - 1 do
                    countConsecutive <- countConsecutive + 1

                    (* If next character is different than current, append this char to result. *)
                    if (i + 1 >= length || str.[i] <> str.[i + 1]) 
                    then
                        compressedString <- compressedString + str.[i].ToString() + countConsecutive.ToString()
                        countConsecutive <- 0

                yield if (compressedString |> String.length < length)
                    then compressedString 
                    else str
            } |> Seq.head

    let compress str =

        let compressed = new StringBuilder()
        let mutable countConsecutive = 0
        let length = str |> String.length

        for i in 0 .. length - 1 do
            countConsecutive <- countConsecutive + 1

            if (i + 1 >= length || str.[i] <> str.[i + 1]) 
            then
                compressed.Append(str.[i]) |> ignore
                compressed.Append(countConsecutive) |> ignore
                countConsecutive <- 0
        if (compressed.Length < length)
            then compressed.ToString() 
            else str


    module Better =
        let compress (str : string) : string =
            seq {
                let countCompression str =
                    let mutable compressedLength = 0
                    let mutable countConsecutive = 0
                    let length = str |> String.length

                    for i in 0 .. length - 1 do
                        countConsecutive <- countConsecutive + 1

                        (* If next character is different than current, append this char to result.*)
                        if (i + 1 >= length || str.[i] <> str.[i + 1])
                        then
                            compressedLength <- compressedLength + 1 + countConsecutive.ToString().Length
                            countConsecutive <- 0
                    compressedLength

                let length = str |> String.length
                let finalLength = str |> countCompression
                if (finalLength >= length) then yield str

                let compressed = new StringBuilder(finalLength) // initialize capacity
                let mutable countConsecutive = 0

                for i in 0 .. length - 1 do
                    countConsecutive <- countConsecutive + 1

                    (* If next character is different than current, append this char to result.*)
                    if (i + 1 >= length || str.[i] <> str.[i + 1])
                    then
                        compressed.Append(str.[i]) |> ignore
                        compressed.Append(countConsecutive) |> ignore
                        countConsecutive <- 0
                yield compressed.ToString()
            } |> Seq.head

type Question() =
    inherit ctci.Contracts.Question()

    override this.DemoRun() =

       let original = "abbccccccde";

       printfn "Original : '%s'" original
       printfn "Compressed Bad: '%s'" (original |> Sln.Bad.compress) 
       printfn "Compressed: '%s'" (original |> Sln.compress)
       printfn "Compressed Better: '%s'" (original |> Sln.Better.compress)
