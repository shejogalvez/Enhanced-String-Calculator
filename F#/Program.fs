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


let extract_negatives (numbers_list: list<int>): list<int> =
    List.filter (fun n -> n<0) numbers_list

// extract the value of a number as a string
// returns 0 when number is too big
let extract_value (snumber: string): int =
    let number = int snumber
    let (|Valid|Giant|) n = if n > 1000 then Giant else Valid
    match number with
        | Giant -> 0
        | Valid -> number

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
    let int_list = numbers_list |> List.map extract_value
    let negative_numbers = extract_negatives int_list
    if List.length negative_numbers > 0 then
        failwith ("negatives not allowed: " + System.String.Join(", ", negative_numbers))
    else
    int_list |> List.sum // return the sum of the numbers

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
    "1,-2,-3"
    "1,-2, 2, 3, -5"
    "-0"
    "2,1001"
    "1,1000,50,2"
]
for test in test_cases do
    try 
    printfn "result of \"%s\": %d" (test.Replace("\n", "\\n")) (Add test) 
    with 
    | ex -> printfn "input \"%s\" failed with error: \"%s\"" (test.Replace("\n", "\\n")) ex.Message

