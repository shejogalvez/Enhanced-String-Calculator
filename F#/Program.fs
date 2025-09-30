// let text = "1,2"
// let list = text.Split(",") |> Seq.toList
// let result = List.map int list |> List.sum

// printfn "calculation is: %d" result

let Add (numbers: string) : int = 
    match numbers with
    | "" -> 0
    | _ -> 
    let numbers_list = numbers.Split "," |> Seq.toList
    match numbers_list.Length with
    | 1 -> int numbers_list[0]
    | 2 -> int numbers_list[0] + int numbers_list[1]
    // result -1 for a not considered case
    | _ -> -1
let test_cases = ["1,2";"1";"";"1,sd"]
for test in test_cases do
    printfn "result of \"%s\": %d" test (Add test)

