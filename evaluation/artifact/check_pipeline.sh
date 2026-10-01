#!/usr/bin/env bash
set -euo pipefail
if [[ $# -ne 1 ]]; then echo "Usage: $0 NEW_OUTPUT_DIR" >&2; exit 2; fi
root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$root"
[[ ! -e "$1" ]] || { echo 'Use a new output directory' >&2; exit 2; }
mkdir -p "$1"
out="$(cd "$1" && pwd)"
[[ "$out" != *'"'* && "$out" != *"\\"* && "$out" != *$'\n'* ]] || {
  echo 'Output path cannot contain quotes, backslashes or newlines' >&2; exit 2;
}
manifest="$out/fixture-run.txt"
printf 'MODE=fixture\nBASELINE_REPEATS=1\n' > "$manifest"
printf 'commit=%s\n' "$(git rev-parse HEAD)" >> "$manifest"
commands=('set Compile / unmanagedSources += baseDirectory.value / "evaluation" / "artifact" / "FixtureCheck.scala"' 'set Compile / fork := true')
for enc in int double; do
  for kind in valid invalid; do
    for group in guided baseline; do
      directory="$out/$enc-$kind-$group"
      case "$kind/$group" in
        valid/guided) key="valid_dir_$enc" ;;
        valid/baseline) key="random_dir_${enc}_1" ;;
        invalid/guided) key="invalid_dir_$enc" ;;
        invalid/baseline) key="invalid_random_dirs_${enc}_1" ;;
      esac
      printf '%s=%s\n' "$key" "$directory" >> "$manifest"
      commands+=("set Compile / envVars += (\"FHETEST_RUN_DIR\" -> \"$directory\")")
      commands+=("runMain FixtureCheck $enc $kind $group")
    done
  done
done
printf '%q ' sbt "${commands[@]}" > "$out/commands.txt"
printf '\n' >> "$out/commands.txt"
sbt "${commands[@]}" > "$out/fixtures.log" 2>&1
printf 'RUN_FINISHED=1\n' >> "$manifest"
uv run evaluation/aggregate_rq2_rq3.py --summary "$manifest"
