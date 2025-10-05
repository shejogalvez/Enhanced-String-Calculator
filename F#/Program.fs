// let text = "1,2"
// let list = text.Split(",") |> Seq.toList
// let result = List.map int list |> List.sum

// printfn "calculation is: %d" result

open System.Text.RegularExpressions

let separate_numbers_with_delimeters (numbers_with_delimeter : string) : option<string> * string = 
    let delimeter_match = Regex.Match(numbers_with_delimeter, "//(.*)\n")
    if not delimeter_match.Success
    then None, numbers_with_delimeter
    else 
    let delimeter_str = delimeter_match.Groups[1].Value 
    let numbers = numbers_with_delimeter.Replace(delimeter_match.Value, "")
    Some delimeter_str, numbers


// get a delimeter expression and returns an array of delimeter strings
let parse_delimeters (delimeter_str : option<string>) : array<string> =
    match delimeter_str with
    | None -> [||]
    | Some delimeter_expr_string ->
    // get the string between // and \n
    (match delimeter_expr_string.Length with
    | 1 -> [|delimeter_expr_string|] // single character delimeter captured
    | _ -> // complex delimeters
    let multiple_matches = Regex.Matches(delimeter_expr_string, "\[(.*?)\]")
    if multiple_matches.Count > 0 
    then 
        let delimeters_array = multiple_matches |> Seq.toArray |> Array.map (fun m -> m.Groups[1].Value)
        delimeters_array
    else
    failwithf "non valid delimeter expression: %s" delimeter_expr_string)

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
    // get delimeters
    let delimeters_str, numbers = separate_numbers_with_delimeters numbers_with_delimeter
    let custom_delimeters = parse_delimeters delimeters_str
    let defalut_delimeters = [|","; "\n"|]
    let delimeters = Array.append custom_delimeters defalut_delimeters

    // split numbers and get values
    let options = System.StringSplitOptions.TrimEntries
    let numbers_list = numbers.Split(delimeters, options) |> Seq.toList
    let int_list = numbers_list |> List.map extract_value

    // check for negative values
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
    "//[***]\n1***2***3"
    "//[*][%]\n1*2%3"
    "//[*][%]\n1*2%3\n4,5"
    "//[%][*]\n1*2%3\n4,5"
    "//[***][%%]\n1***2%%3"
    "//[***][%%]\n1***2%%3\n4,5"
    "//[***][%%][***]\n1***2%%3"
    "//[***][%%]\n1***2%%3,,5"
    "//***,%%\n1***2%%3,,5"
]
for test in test_cases do
    try 
    printfn "result of \"%s\": %d" (test.Replace("\n", "\\n")) (Add test) 
    with 
    | ex -> printfn "input \"%s\" failed with error: \"%s\"" (test.Replace("\n", "\\n")) ex.Message

