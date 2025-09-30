// let text = "1,2"
// let list = text.Split(",") |> Seq.toList
// let result = List.map int list |> List.sum

// printfn "calculation is: %d" result

let Add (numbers: string) : int = 
    match numbers with
    | "" -> 0
    | _ -> 
    let numbers_list = numbers.Split "," |> Seq.toList
    try numbers_list |> List.map int |> List.sum with
    | ex ->
        // Catch any exception type (general catch-all) and return error result
        printfn "An error occurred: %s" ex.Message
        -1
let test_cases = [
    "1,2"
    "1"
    ""
    "1,sd"
    "1,2,3,4"
]
for test in test_cases do
    printfn "result of \"%s\": %d" test (Add test)

