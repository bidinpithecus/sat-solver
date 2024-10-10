import sys

def parse_cnf(file_path):
    """Parse a CNF file and return a list of clauses."""
    clauses = []
    try:
        with open(file_path, 'r') as file:
            for line in file:
                line = line.strip()
                if line.startswith('c') or not line:
                    continue
                if line.startswith('p'):
                    continue
                clause = list(map(int, line.split()))[:-1]
                clauses.append(clause)
    except IOError:
        print(f"Error: Could not read file {file_path}.")
        sys.exit(1)
    return clauses


def parse_result(file_path):
    """Parse a result file and return the SAT status and a list of literals."""
    try:
        with open(file_path, 'r') as file:
            lines = file.readlines()
            status = lines[0].strip()
            if status == "SAT":
                solution = list(map(int, lines[1].strip().split()))[:-1]
            else:
                solution = []
    except IOError:
        print(f"Error: Could not read file {file_path}.")
        sys.exit(1)
    return status, solution


def is_solution_valid(cnf, solution):
    """Check if the solution satisfies the CNF."""
    unsatisfied_clauses = []
    solution_set = set(solution)  # Convert to set for faster lookup
    for clause in cnf:
        if not (any(literal in solution_set for literal in clause) or
                any(-literal in solution_set for literal in clause)):
            unsatisfied_clauses.append(clause)
    return unsatisfied_clauses


def main():
    if len(sys.argv) != 3:
        print("Usage: python Verifier.py <cnf_file> <res_file>")
        sys.exit(1)

    cnf_file = sys.argv[1]
    res_file = sys.argv[2]

    cnf = parse_cnf(cnf_file)
    status, solution = parse_result(res_file)

    unsatisfied_clauses = is_solution_valid(cnf, solution)

    if status == "SAT":
        if not unsatisfied_clauses:
            print("The solution is valid.")
        else:
            print("The solution is invalid. Unsatisfied clauses:")
            for clause in unsatisfied_clauses:
                print(clause)
    else:
        print("The result indicates UNSAT; no valid solution exists.")


if __name__ == '__main__':
    main()
