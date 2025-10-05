// let text = "1,2"
// let list = text.Split(",") |> Seq.toList
// let result = List.map int list |> List.sum

// printfn "calculation is: %d" result

let separate_numbers_with_delimiters (numbers_with_delimiter : string) : option<string> * string = 
    let char_list = numbers_with_delimiter.ToCharArray() |> Seq.toList
    match char_list with
    | '/'::'/':: tail ->
    let separation_index = List.findIndex (fun c -> c='\n') tail
    let list_without_newline = List.removeAt separation_index tail
    let delimiter_char_list, numbers_char_list = List.splitAt separation_index list_without_newline
    let ret = Some(System.String.Concat delimiter_char_list), System.String.Concat numbers_char_list
    ret
    | _ -> None, numbers_with_delimiter

let rec parse_flexible_delimiter (delimiter_expr: list<char>) (partial_delimiter: list<char>): list<char> * list<char> =
    match delimiter_expr with
    | ']' :: delimeter_expr_tail -> 
        if partial_delimiter.Length = 0 then failwith "empty delimiter encountered"
        else partial_delimiter, delimeter_expr_tail
    | head :: tail ->  
        parse_flexible_delimiter tail (partial_delimiter @ [head])
    | _ -> failwith "unusual end of string on delimiter encountered"

let rec parse_complex_delimiters (delimiter_expr: list<char>): list<string> =
    match delimiter_expr with
    | '[' :: tail -> 
        let delimeter_char_list, delimeter_expr_tail = parse_flexible_delimiter tail []
        System.String.Concat delimeter_char_list :: parse_complex_delimiters delimeter_expr_tail
    | [] -> []
    | head :: _ -> failwithf "delimeter has to start with '[', '%c' found instead" head

// get a delimiter expression and returns an array of delimiter strings
let parse_delimiters (delimiter_str : option<string>) : array<string> =
    match delimiter_str with
    | None -> [||]
    | Some delimiter_expr_string ->
    // get the string between // and \n
    match delimiter_expr_string.Length with
    | 1 -> [|delimiter_expr_string|] // single character delimiter captured
    | _ -> // complex delimiters
    let delimiter_char_list = delimiter_expr_string |> Seq.toList
    parse_complex_delimiters delimiter_char_list |> Seq.toArray


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

let Add (numbers_with_delimiter: string) : int = 
    match numbers_with_delimiter with
    | "" -> 0
    | _ -> 
    let delimiter_str, numbers = separate_numbers_with_delimiters numbers_with_delimiter
    let custom_delimiter = parse_delimiters delimiter_str
    let default_delimiters = [|","; "\n"|]
    let delimiters = Array.append default_delimiters custom_delimiter

    // split string and extract value of numbers
    let options = System.StringSplitOptions.TrimEntries
    let numbers_list = numbers.Split(delimiters, options) |> Seq.toList
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
    "//[***]\n1***2***3"
    "//[***]\n1\n2***3"
    "//[*][%]\n1*2%3"
    "//[%][*]\n1*2%3"
    "//[%][*]\n1\n2%3,4*5"
    "//[***][%%]\n1***2%%3"
    "//[%%][***][%%]\n1***2%%3"
    "//[%%][[***]\n1[***2%%3" // somehow valid expression
    "//[%%][***]\n1***2%%3"
    "//[%%]a[***]\n1***2%%3"
    "//[%%][***]]\n1***2%%3"
    "//[%%][***\n1***2%%3"
]
for test in test_cases do
    try 
    printfn "result of \"%s\": %d" (test.Replace("\n", "\\n")) (Add test) 
    with 
    | ex -> printfn "input \"%s\" failed with error: \"%s\"" (test.Replace("\n", "\\n")) ex.Message

