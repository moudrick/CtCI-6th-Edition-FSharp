open ctci.Contracts
open Chapter01

[<EntryPoint>]
let main argv =

    let chapters : Question [] [] = [|
        [| 
            new ``Q1 01 - Is Unique``(); 
            new ``Q1 07 - Rotate Matrix``();
        |];
    |]

    for chapter in chapters do
        for q in chapter do
            if (argv.Length = 0 || argv |> Array.exists (fun v -> q.Name.Contains v)) then
                printf "\n\n"
                printfn "// Executing: '%s'" q.Name 
                printfn "// ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----"

                q.Run()

    Unchecked.defaultof<int>
