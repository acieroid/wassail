#!/bin/sh

file_to_analyze="$1"

if [ -z "$file_to_analyze" ]; then
  echo "Usage: $0 <path-to-file>"
  exit 1
fi

find_function_end_line() {
  awk -v start="$1" '
    NR < start { next }
    {
      line = $0
      gsub(/;;.*/, "", line)
      opens = gsub(/\(/, "(", line)
      closes = gsub(/\)/, ")", line)
      depth += opens - closes
      if (NR > start && depth <= 0) {
        print NR
        exit
      }
    }
  ' "$file_to_analyze"
}

count_instruction_lines() {
  file="$1"
  first_line="$2"
  last_line="$3"

  sed -n "${first_line},${last_line}p" "$file" | awk '
    {
      line = $0

      # Remove WebAssembly line comments.
      sub(/;;.*/, "", line)

      # Trim whitespace.
      gsub(/^[[:space:]]+/, "", line)
      gsub(/[[:space:]]+$/, "", line)

      # Ignore empty/comment-only lines.
      if (line == "") next

      # Ignore pure structural/module lines.
      if (line == "(") next
      if (line == ")") next

      # Ignore WebAssembly control-flow markers that are not counted as real instructions here.
      if (line == "end") next
      if (line == "else") next

      # Ignore function signature/declaration lines.
      if (line ~ /^\(func([[:space:]]|$)/) next
      if (line ~ /^\(export([[:space:]]|$)/) next
      if (line ~ /^\(param([[:space:]]|$)/) next
      if (line ~ /^\(result([[:space:]]|$)/) next
      if (line ~ /^\(local([[:space:]]|$)/) next

      count++
    }
    END { print count + 0 }
  '
}

echo "File to analyze: $file_to_analyze"

function_numbers=$(grep -o 'func (;[0-9]\+;)' "$file_to_analyze" | sed 's/func (;//; s/;)//' | sort -n | uniq)
echo "Function numbers found: $function_numbers"

for function_number in $function_numbers; do
  start_line=$(grep -n "func (;$function_number;)" "$file_to_analyze" | cut -d: -f1 | head -n 1)
  body_first_line=$start_line
  body_last_line=$(find_function_end_line "$start_line")

  echo "Processing function $function_number"
  echo "  Body starts at line $body_first_line"
  echo "  Body ends at line $body_last_line"
  
  instructions_count=$(count_instruction_lines "$file_to_analyze" "$body_first_line" "$body_last_line")

  OUTPUT_CSV="${file_to_analyze}.fct.${function_number}_version3.csv"
  printf 'line number,slicing criterion,original instr count,nb of instr in sliced fct,nb of instr with ptr analysis,ratio without ptr,ratio with ptr,instructions éliminées,relative gain\n' > "$OUTPUT_CSV"

  echo "  Function $function_number contains $instructions_count instructions."

  current_line=$body_first_line
  rm -f sliced.wat sliced2.wat

  while [ "$current_line" -le "$body_last_line" ]; do
    
    if dune exec -- wassail slice "$file_to_analyze" "$function_number" "$current_line" sliced.wat >/dev/null 2>&1; then
      line_content=$(sed -n "${current_line}p" "$file_to_analyze")
      csv_line_content=$(printf '%s' "$line_content" | sed 's/"/""/g')
      # echo "      $line_content"

      rm -f sliced2.wat
      if ! dune exec -- wassail slice "$file_to_analyze" "$function_number" "$current_line" sliced2.wat --pointers >/dev/null 2>&1; then
        current_line=$((current_line + 1))
        continue
      fi

      # Count number of lines in the sliced function
      sliced_start=$(grep -n "func (;$function_number;)" sliced.wat | cut -d: -f1 | head -n 1)
      sliced_end=$(awk -v start="$sliced_start" '
        NR < start { next }
        {
          line = $0
          gsub(/;;.*/, "", line)
          opens = gsub(/\(/, "(", line)
          closes = gsub(/\)/, ")", line)
          depth += opens - closes
          if (NR > start && depth <= 0) {
            print NR
            exit
          }
        }
      ' sliced.wat)

      sliced_line_count_no_pointers=$(count_instruction_lines sliced.wat "$sliced_start" "$sliced_end")

      # Count number of lines in the sliced function (with ptr analysis)
      sliced_start=$(grep -n "func (;$function_number;)" sliced2.wat | cut -d: -f1 | head -n 1)
      sliced_end=$(awk -v start="$sliced_start" '
        NR < start { next }
        {
          line = $0
          gsub(/;;.*/, "", line)
          opens = gsub(/\(/, "(", line)
          closes = gsub(/\)/, ")", line)
          depth += opens - closes
          if (NR > start && depth <= 0) {
            print NR
            exit
          }
        }
      ' sliced2.wat)

      sliced_line_count_with_pointers=$(count_instruction_lines sliced2.wat "$sliced_start" "$sliced_end")
      # echo "      Sliced function has $sliced_line_count_no_pointers instructions"
      # echo "      Sliced_with_ptrs function has $sliced_line_count_with_pointers instructions"


      difference=$((sliced_line_count_no_pointers - sliced_line_count_with_pointers))

      ratio_no_pointers=$(awk -v sliced="$sliced_line_count_no_pointers" -v original="$instructions_count" 'BEGIN {
        if (original > 0) printf "%.6f", sliced / original;
        else printf "0.000000";
      }')

      ratio_with_pointers=$(awk -v sliced="$sliced_line_count_with_pointers" -v original="$instructions_count" 'BEGIN {
        if (original > 0) printf "%.6f", sliced / original;
        else printf "0.000000";
      }')

      relative_gain=$(awk -v no_ptr="$ratio_no_pointers" -v with_ptr="$ratio_with_pointers" 'BEGIN {
        if (no_ptr > 0) printf "%.6f", (no_ptr - with_ptr) / no_ptr;
        else printf "0.000000";
      }')

      printf '%s,"%s",%s,%s,%s,%s,%s,%s,%s\n' "$current_line" "$csv_line_content" "$instructions_count" "$sliced_line_count_no_pointers" "$sliced_line_count_with_pointers" "$ratio_no_pointers" "$ratio_with_pointers" "$difference" "$relative_gain" >> "$OUTPUT_CSV"
    fi

    current_line=$((current_line + 1))
  done
  
  echo "  Results written in file $OUTPUT_CSV."
done