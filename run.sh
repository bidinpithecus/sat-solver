#!/bin/bash

# This script runs the SAT solver program on a provided .cnf file or all .cnf files in the 'input' directory.

make >/dev/null 2>&1 || { echo "Make failed. Exiting."; exit 1; }

mkdir -p output

if [ "$#" -eq 1 ]; then
  FILE="$1"
elif [ "$#" -eq 0 ]; then
  DIR="input"
else
  echo "Usage: $0 [<filename>]"
  echo "<filename>: Path to the input file"
  exit 1
fi

process_file() {
  local FILE="$1"
  local BASENAME=$(basename "$FILE" .cnf)
  
  echo "reading file $FILE"

  START_TIME=$(date +%s.%N)
  
  OUTPUT=$(./bin/sat-solver "$FILE")
  echo "$OUTPUT" > "output/${BASENAME}.res"
  
  END_TIME=$(date +%s.%N)
  EXECUTION_TIME=$(echo "$END_TIME - $START_TIME" | bc)
  
  echo "$OUTPUT" | head -n 1
  echo "Execution time: $EXECUTION_TIME seconds"
}

if [ -n "$FILE" ]; then
  if [ -f "$FILE" ]; then
    process_file "$FILE"
  else
    echo "Error: File '$FILE' not found."
    exit 1
  fi

elif [ -n "$DIR" ]; then
  if [ -d "$DIR" ]; then
    find "$DIR" -type f -name "*.cnf" | sort | while read -r CNF_FILE; do
      process_file "$CNF_FILE"
    done
  else
    echo "Error: Directory '$DIR' not found."
    exit 1
  fi
else 
  echo "Unexpected error."
  exit 1
fi
