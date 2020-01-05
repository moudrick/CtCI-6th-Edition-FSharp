open ctci.Contracts
open Ch_01._Arrays_and_Strings
open Ch_05._Bit_Manipulation
open Ch_10._Sorting_and_Searching

[<EntryPoint>]
let main argv =

    let chapters : Question [] [] = [|
        [| 
            ``Q1 01 - Is Unique``() 
            ``Q1 07 - Rotate Matrix``()
        |]; [|
            ``Q05 01 - Insertion``()
        |]; [|
            ``Q10 01 - Sorted Merge``()
        |]
    |]

    for chapter in chapters do
        for q in chapter do
            if (argv.Length = 0 || argv |> Array.exists (fun v -> q.Name.Contains v)) then
                printf "\n\n"
                printfn "// Executing: '%s'" q.Name 
                printfn "// ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----"

                q.Run()

    Unchecked.defaultof<int>
