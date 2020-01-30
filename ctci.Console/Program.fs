[<EntryPoint>]
let main argv =

    let chapters : ctci.Contracts.Question [] [] = [|
        [| 
            Ch_01._Arrays_and_Strings.``Q1 01 - Is Unique``.Question() 
            Ch_01._Arrays_and_Strings.``Q1 07 - Rotate Matrix``.Question()
        |]; [|
            Ch_05._Bit_Manipulation.``Q05 01 - Insertion``.Question()
        |]; [|
            Ch_10._Sorting_and_Searching.``Q10 01 - Sorted Merge``.Question()
        |]
    |]

    for chapter in chapters do
        for q in chapter do
            if (argv.Length = 0 || argv |> Array.exists q.Name.Contains) then
                printf "\n\n"
                printfn "// Executing: '%s'" q.Name 
                printfn "// ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----"

                q.DemoRun()

    Unchecked.defaultof<int>
