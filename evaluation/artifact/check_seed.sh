#!/usr/bin/env bash
set -euo pipefail
if [[ ${1:-} == --help ]]; then
  echo "Usage: $0 NEW_OUTPUT_DIR"
  echo 'Requires built T2 (lib/terminator-compiler-1.0.jar), java and sbt.'
  echo 'Runs fresh JVM snapshots for int/double × valid/invalid/baseline.'
  echo 'Defaults: COUNT=3 SEED=20260929 OTHER_SEED=20260930.'
  exit 0
fi
if [[ $# -ne 1 ]]; then echo "Usage: $0 NEW_OUTPUT_DIR" >&2; exit 2; fi
root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
count="${COUNT:-3}"
seed="${SEED:-20260929}"
other="${OTHER_SEED:-20260930}"
[[ "$count" =~ ^[1-9][0-9]*$ ]] || { echo 'COUNT must be positive' >&2; exit 2; }
[[ "$seed" =~ ^[0-9]+$ && "$other" =~ ^[0-9]+$ && "$seed" != "$other" ]] || {
  echo 'SEED and OTHER_SEED must be distinct nonnegative Long values' >&2; exit 2;
}
[[ -f "$root/lib/terminator-compiler-1.0.jar" ]] || { echo 'Build T2 first with build_project.sh' >&2; exit 1; }
[[ ! -e "$1" ]] || { echo 'Use a new output directory to preserve previous evidence' >&2; exit 2; }
mkdir -p "$1"
output="$(cd "$1" && pwd)"
[[ "$output" != *'"'* && "$output" != *$'\n'* && "$output" != *"\\"* ]] || {
  echo 'Output path cannot contain quotes, newlines or backslashes' >&2; exit 2;
}
commands=('set Compile / unmanagedSources += baseDirectory.value / "evaluation" / "artifact" / "SeedSnapshot.scala"' 'set Compile / fork := true')
for type in int double; do
  for mode in valid invalid baseline; do
    for run in first repeat other; do
      run_seed="$seed"
      if [[ "$run" == other ]]; then run_seed="$other"; fi
      commands+=("runMain SeedSnapshot \"$output/$type-$mode-$run.txt\" $type $mode $run_seed $count")
    done
  done
done
cd "$root"
printf 'seed=%s\nother_seed=%s\ncount=%s\n' "$seed" "$other" "$count" > "$output/settings.txt"
bash evaluation/artifact/record_environment.sh "$output"
sbt "${commands[@]}" > "$output/generation.log" 2>&1
printf 'type,mode,same_seed_equal,different_seed_differs\n' > "$output/summary.csv"
for type in int double; do
  for mode in valid invalid baseline; do
    cmp "$output/$type-$mode-first.txt" "$output/$type-$mode-repeat.txt"
    if cmp -s "$output/$type-$mode-first.txt" "$output/$type-$mode-other.txt"; then
      echo "Different seeds produced identical snapshots: $type/$mode" >&2; exit 1
    fi
    printf '%s,%s,true,true\n' "$type" "$mode" >> "$output/summary.csv"
  done
done
cat "$output/summary.csv"
