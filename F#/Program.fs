// let text = "1,2"
// let list = text.Split(",") |> Seq.toList
// let result = List.map int list |> List.sum

// printfn "calculation is: %d" result

// if numbers_with_delimeter has delimiter, separates the delimeter from the input, and returns them separately in a tuple
// else return the tuple (None, numbers_with_delimeter)
let extract_delimeter (numbers_with_delimeter : string) : option<string> * string =
    let char_list = numbers_with_delimeter.ToCharArray() |> Seq.toList
    match char_list with
        | '/'::'/'::delimeter::'\n' :: tail -> 
            Some(delimeter.ToString()), // delimeter captured
            Array.ofList tail |> System.String.Concat // rest of the numbers string
        | _ -> None, numbers_with_delimeter

let Add (numbers_with_delimeter: string) : int = 
    match numbers_with_delimeter with
    | "" -> 0
    | _ -> 
    let custom_delimeter, numbers = extract_delimeter numbers_with_delimeter
    let delimeters = 
        match custom_delimeter with
        | Some d -> [|","; "\n"; d|]
        | None -> [|","; "\n"|]
    let options = System.StringSplitOptions.TrimEntries
    let numbers_list = numbers.Split(delimeters, options) |> Seq.toList
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
    "1\n2,3"
    "1\n2,3\n4"
    "//;\n1;2"
    "//@\n1@2"
]
for test in test_cases do
    printfn "result of \"%s\": %d" (test.Replace("\n", "\\n")) (Add test)

