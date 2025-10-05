from typing import Tuple, Optional

class NegativeNumberException(Exception):
    pass

def separate_numbers_and_delimiter(numbers_with_delimiter: str) -> Tuple[str, Optional[str]]:
    if numbers_with_delimiter.find("//") != 0:
        return numbers_with_delimiter, None
    separation_index = numbers_with_delimiter.find("\n")
    if separation_index == -1: raise SyntaxError("Delimiter header endline not found")
    return numbers_with_delimiter[separation_index+1::], numbers_with_delimiter[2:separation_index]

def parse_delimiters(delimiters_expr: str) -> list[str]:
    if not delimiters_expr: raise SyntaxError("empty delimiters expression")
    if len(delimiters_expr) == 1: return [delimiters_expr] # simple delimiter
    delimiters = []
    while delimiters_expr: # complex delimiters
        # checks for valid definition of delimiter
        start = delimiters_expr.find('[') + 1
        end = delimiters_expr.find(']')
        if (start != 1): raise SyntaxError(f"delimiter has to start with '[', '{delimiters_expr[0]}' found instead")
        if (end == -1): raise SyntaxError("unusual end of string on delimiter encountered")

        # add to result substring between brackets found
        new_delimiter = delimiters_expr[start: end]
        if len(new_delimiter) < 0: raise SyntaxError("empty delimiter encountered")
        delimiters.append(new_delimiter)

        # remove captured delimiter from delimiters_expr
        delimiters_expr = delimiters_expr[end+1::]

    return delimiters


def extract_value(snumber: str) -> int:
    value = int(snumber)
    if (value > 1000): return 0
    return value

def Add(numbers_with_delimiter: str) -> int:
    if not numbers_with_delimiter: return 0
    
    numbers, delimiters_expr = separate_numbers_and_delimiter(numbers_with_delimiter)
    delimiters = ["\n"]
    if delimiters_expr is not None:
        delimiters.extend(parse_delimiters(delimiters_expr))

    # split string and extract value of numbers
    for delimiter in delimiters:
        numbers = numbers.replace(delimiter, ",") # 
    number_list = numbers.split(',')
    int_list: list[int] = [extract_value(snumber) for snumber in number_list]
    negative_numbers: list[str] = [str(n) for n in int_list if n < 0]
    if negative_numbers:
        raise NegativeNumberException("negatives not allowed: " + ", ".join(negative_numbers))
    
    return sum(int_list)

test_cases = [
    "1,2",  
    "1",
    "",
    "1,sd",
    "1,2,3,4",
    "1\n2,3",
    "1\n2,3\n4",
    "//;\n1;2",
    "//@\n1@2",
    "1,-2,-3",
    "1,-2, 2, 3, -5",
    "-0",
    "2,1001",
    "1,1000,50,2",
    "//[***]\n1***2***3",
    "//[***]\n1\n2***3",
    "//[*][%]\n1*2%3",
    "//[%][*]\n1*2%3",
    "//[%][*]\n1\n2%3,4*5",
    "//[***][%%]\n1***2%%3",
    "//[%%][***][%%]\n1***2%%3",
    "//[%%][[***]\n1[***2%%3" , # somehow valid expression
    "//[%%][***]\n1***2%%3",
    "//[%%]a[***]\n1***2%%3",
    "//[%%][***]]\n1***2%%3",
    "//[%%][***\n1***2%%3",
    "//[%%][***]\n1,,2%%3***4"
]
for test in test_cases:
    test_f = test.replace("\n", "\\n")
    try:
        print (f"result of \"{test_f}\": {Add(test)}" )
    except (SyntaxError, ValueError, NegativeNumberException) as e:
        print (f"input \"{test_f}\" failed with error: \"{e}\"")
    except Exception as e:
        print (f"input \"{test_f}\" failed with unexpected error:")
        raise e
