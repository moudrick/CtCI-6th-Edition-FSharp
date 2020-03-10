[<EntryPoint>]
let main argv =

    let chapters : ctci.Contracts.Question [] [] = [|
        [| 
            ``Ch 01``.`` Arrays and Strings``.``Q1 01 - Is Unique``.Question()
            ``Ch 01``.`` Arrays and Strings``.``Q1 06 - String Compression``.Question()
            ``Ch 01``.`` Arrays and Strings``.``Q1 07 - Rotate Matrix``.Question()
        |]; [|
            ``Ch 04``.`` Trees and Graphs``.``Q04 - Introduction``.Question()
            ``Ch 04``.`` Trees and Graphs``.``Q04 02 - Minimal Tree``.Question()
            ``Ch 04``.`` Trees and Graphs``.``Q04 04 - Check Balanced``.Question()
            ``Ch 04``.`` Trees and Graphs``.``Q04 06 - Successor``.Question()
        |]; [|
            ``Ch 05``.`` Bit Manipulation``.``Q05 01 - Insertion``.Question()
        |]; [|
            ``Ch 10``.`` Sorting and Searching``.``Q10 01 - Sorted Merge``.Question()
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
