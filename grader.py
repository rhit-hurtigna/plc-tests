import subprocess
import json
import sys
from datetime import datetime

timeout = 2
assignment_name = sys.argv[1]
file_name = sys.argv[2]
repo_name = sys.argv[3]

# Runs at the very start of a student's submission. Parses through their submission to make
# sure it's well-formed, but doesn't call any tests.
def test_user_code():
    try:
        result = subprocess.run(["racket", f'{repo_name}/{assignment_name}/{file_name}'], capture_output=True, timeout=timeout)
    except subprocess.TimeoutExpired:
        return "Timed out while parsing your code."
    if result.returncode == 0:
        return None
    else:
        result = result.stderr.decode()[:-1]
        if result[:15] == "open-input-file":
            result = "Couldn't find \"" + file_name + "\" in your .zip submission."
        return result

# Call a specific test, writing the output to out.txt to avoid user print statements interfering
def run_test(suite_index, test_index):
    start = datetime.now()
    try:
        result = subprocess.run(["racket", "-l", "racket", "-t", f'{repo_name}/{assignment_name}/testcode.rkt', "-e", f'(call-with-output-file "out.txt" (lambda (out) (write (individual-test {suite_index} {test_index}) out)) #:exists \'replace)'], capture_output=True, timeout=timeout)
    except subprocess.TimeoutExpired:
        return (False, "Timed out while excuting this case.", timeout)
    time = (datetime.now() - start).total_seconds()
    if result.returncode == 0:
        return (True, read_output(), time)
    else:
        return (False, result.stderr.decode()[:-1], time)

def read_output():
    out_file = open("out.txt")
    contents = out_file.readline()
    out_file.close()
    return contents

# Runs once per submission. Queries the test Racket to see the structure and weights of the unit tests.
# We need to know the weights beforehand so if the tests crash, we know how many points to deduct.
def get_weights():
    weights_array = []
    subprocess.run(["racket", "-l", "racket", "-t", f'{repo_name}/{assignment_name}/testcode.rkt', "-e", f'(call-with-output-file "out.txt" (lambda (out) (write (get-weights) out)) #:exists \'replace)'], capture_output=True)
    for scheme_list in parse_scheme_list(read_output()):
        weights_array.append(parse_scheme_list(scheme_list))
    return weights_array

# Takes a string that represents a Scheme list and turns it into a Python list.
def parse_scheme_list(scheme_list):
    scheme_list = scheme_list[1:-1]
    list_of_items = []
    current_item = ""
    parens_depth = 0
    for char in scheme_list:
        if parens_depth == 0:
            if char == ' ':
                list_of_items.append(current_item)
                current_item = ""
            else:
                current_item += char
            if char == '(':
                parens_depth += 1
        else:
            current_item += char
            if char == '(':
                parens_depth += 1
            elif char == ')':
                parens_depth -= 1
    list_of_items.append(current_item)
    return list_of_items

# Parses a successful (didn't crash or time out) test case into a dictionary format for Gradescope.
def parse_result(result, weight, time):
    score, code, yours, expected = parse_scheme_list(result)
    return {
        "score": score,
        "max_score": weight,
        "output": f"Test case: {code}\nYour output: {yours}\nExpected output: {expected}\nExecution time: {time}"
    }

suite_index = 0
test_index = 0
start = datetime.now()
tests = []

error = test_user_code()
if error is not None:
    answer = {
        "score": 0,
        "output": error
    }
else:
    weights = get_weights()
    while True:
        if len(weights) <= suite_index:
            break
        elif len(weights[suite_index]) <= test_index:
            suite_index += 1
            test_index = 0
            continue
        (success, result, time) = run_test(suite_index, test_index)
        if success:
            tests.append(parse_result(result, weights[suite_index][test_index], time))
            test_index += 1
        else:
            tests.append({
                "score": 0,
                "max_score": weights[suite_index][test_index],
                "output": result
            })
            test_index += 1

    answer = {
        "execution_time": (datetime.now() - start).total_seconds(),
        "tests": tests
    }

print(answer)
# results_file = open("/autograder/results/results.json", "w+")
# json.dump(answer, results_file)
# results_file.close()
