#!/usr/bin/env bash

set -euo pipefail

usage() {
  echo "Usage: $0 DIRECTORY"
  echo
  echo "Runs the slice evaluator on every .wat and .wasm file under DIRECTORY, recursively."
}

if [ "$#" -ne 1 ]; then
  usage >&2
  exit 1
fi

dir=$1


if [ ! -d "$dir" ]; then
  echo "Error: '$dir' is not a directory" >&2
  exit 1
fi

log_file="$dir/results.log"
: > "$log_file"
exec > >(tee "$log_file") 2>&1

random_seed=14
time_limit_seconds=120
parallel_jobs=${PARALLEL_JOBS:-$(nproc)}
start_time=$(date +%s)

print_elapsed_time() {
  end_time=$(date +%s)
  elapsed=$((end_time - start_time))
  hours=$((elapsed / 3600))
  minutes=$(((elapsed % 3600) / 60))
  seconds=$((elapsed % 60))

  printf '\nTotal execution time: %02dh %02dm %02ds\n' "$hours" "$minutes" "$seconds"
}

if ! command -v parallel >/dev/null 2>&1; then
  echo "Error: GNU parallel is not installed or not in PATH" >&2
  exit 1
fi

process_file() {
  file=$1
  file_basename=$(basename "$file")

  file_seed=$(printf '%s' "$file" | cksum | awk '{print $1}')
  RANDOM=$((random_seed + file_seed))

  function_indices=$(wassail functions "$file" | awk '{print $1}')
  number_of_functions=$(printf '%s\n' "$function_indices" | sed '/^$/d' | wc -l | tr -d ' ')
  sample_size=$(awk -v n="$number_of_functions" 'BEGIN { printf "%d", (n * 0.96) / ((0.01 * (n - 1)) + 1.92) }')

  sample_threshold=20

  if [ "$number_of_functions" -eq 0 ]; then
    echo "Skipping $file (no functions found)"
    return 0
  fi

  if [ "$sample_size" -lt "$sample_threshold" ]; then
    sample_size=$sample_threshold
  fi

  if [ "$sample_size" -gt "$number_of_functions" ]; then
    sample_size=$number_of_functions
  fi
    
  function_indices_array=()
  while IFS= read -r function_index; do
    function_indices_array+=("$function_index")
  done <<EOF
$function_indices
EOF

  for ((i = 0; i < sample_size; i++)); do
    j=$((i + RANDOM % (number_of_functions - i)))

    tmp=${function_indices_array[$i]}
    function_indices_array[$i]=${function_indices_array[$j]}
    function_indices_array[$j]=$tmp
  done

  function_indices=$(printf '%s\n' "${function_indices_array[@]:0:sample_size}")

  length_of_function_indices=$(printf '%s\n' "$function_indices" | sed '/^$/d' | wc -l | tr -d ' ')
  echo "Processing $file ($length_of_function_indices functions to slice)"

  function_indices_csv=$(printf '%s' "$function_indices" | paste -sd, -)
  seed=$number_of_functions

  printf '   [%s]\n      slicing of functions %s with seed %s\n' "$file_basename" "$function_indices_csv" "$seed"

  set +e
  timeout "${time_limit_seconds}s" \
    wassail slice-evaluator "$file" -f "$function_indices_csv" -r 20 -seed "$seed"
  status=$?
  set -e

  if [ "$status" -eq 124 ]; then
    printf '   [%s]\n      evaluation timed out after %ss; stopping this file ------------------------------------------timeout\n' "$file_basename" "$time_limit_seconds"
    return 0
  elif [ "$status" -ne 0 ]; then
    echo "[$file_basename] slice evaluator failed with status $status; stopping this file --------------------------------- error" >&2
    return "$status"
  fi
}


export -f process_file
export random_seed
export time_limit_seconds

echo "Running with $parallel_jobs parallel job(s)"

find "$dir" -type f \( -name '*.wat' -o -name '*.wasm' \) -print0 \
  | parallel -0 --line-buffer -j "$parallel_jobs" process_file {}

data_file="$dir/data.csv"
: > "$data_file"

first_data_csv=$(find "$dir" -maxdepth 1 -type f -name '*.data.csv' | sort | head -n 1)

if [ -n "$first_data_csv" ]; then
  head -n 1 "$first_data_csv" > "$data_file"

  find "$dir" -maxdepth 1 -type f -name '*.data.csv' -print0 \
    | sort -z \
    | while IFS= read -r -d '' file; do
        tail -n +2 "$file" >> "$data_file"
      done

  printf "\nCombined .data.csv files into $data_file\n"
else
  printf "\nNo .data.csv files found to combine\n"
fi

print_elapsed_time