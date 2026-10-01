#!/usr/bin/env bash
set -euo pipefail

MODE="${1:-smoke}"
case "$MODE" in
  --help|-h)
    echo "Usage: $0 [smoke|full]"
    echo "Options (environment): RUN_SEED, BASELINE_REPEATS, DURATION, EVAL_OUTDIR,"
    echo "INVALID_RANDOM_CHUNK, INVALID_RANDOM_MAX_ITERS, GUIDED_COUNT (smoke only)."
    exit 0 ;;
  smoke) DURATION="${DURATION:-60s}"; INVALID_RANDOM_CHUNK="${INVALID_RANDOM_CHUNK:-2}"; INVALID_RANDOM_MAX_ITERS="${INVALID_RANDOM_MAX_ITERS:-1}" ;;
  full) DURATION="${DURATION:-24h}"; INVALID_RANDOM_CHUNK="${INVALID_RANDOM_CHUNK:-200}"; INVALID_RANDOM_MAX_ITERS="${INVALID_RANDOM_MAX_ITERS:-1000}" ;;
  *) echo "Expected smoke or full; see --help" >&2; exit 2 ;;
esac
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"
OPENFHE_DIR="${OPENFHE_DIR:-${OpenFHE_DIR:-/usr/local/OpenFHE-v1.4.2/lib/OpenFHE}}"
OPENFHE_VER="${OPENFHE_VER:-1.4.2}"
RUN_SEED="${RUN_SEED:-20260929}"
BASELINE_REPEATS="${BASELINE_REPEATS:-3}"
GUIDED_COUNT="${GUIDED_COUNT:-}"
for name in RUN_SEED BASELINE_REPEATS INVALID_RANDOM_CHUNK INVALID_RANDOM_MAX_ITERS; do
  value="${!name}"
  if [[ ! "$value" =~ ^(0|[1-9][0-9]*)$ || ${#value} -gt 9 ]]; then
    echo "$name must be an integer between 0 and 999999999" >&2; exit 2
  fi
done
if (( BASELINE_REPEATS < 1 || INVALID_RANDOM_CHUNK < 1 || INVALID_RANDOM_MAX_ITERS < 1 )); then
  echo "Repeat and chunk limits must be positive" >&2; exit 2
fi
if [[ -n "$GUIDED_COUNT" ]] && { [[ "$MODE" != smoke || ! "$GUIDED_COUNT" =~ ^[1-9][0-9]*$ ]]; }; then
  echo "GUIDED_COUNT is a positive smoke-only limit" >&2; exit 2
fi
if command -v gtimeout >/dev/null 2>&1; then TIMEOUT_BIN=gtimeout
elif command -v timeout >/dev/null 2>&1; then TIMEOUT_BIN=timeout
else echo "Install GNU coreutils (timeout / gtimeout)" >&2; exit 1; fi
[[ -x "$ROOT/bin/fhetest" ]] || { echo "Build bin/fhetest first" >&2; exit 1; }
RUN_TS="$(date +%Y%m%d_%H%M%S)-$$"
EVAL_OUTDIR="${EVAL_OUTDIR:-$ROOT/evaluation/evaluation-$RUN_TS}"
# Refuse reuse so an interrupted or older run cannot contaminate this run.
mkdir "$EVAL_OUTDIR"
EVAL_OUTDIR="$(cd "$EVAL_OUTDIR" && pwd)"
summary="$EVAL_OUTDIR/rq2_rq3_run_${MODE}_${RUN_TS}.txt"
record() { printf '%s=%s\n' "$1" "$2" | tee -a "$summary"; }
record MODE "$MODE"
record OPENFHE_DIR "$OPENFHE_DIR"
record OPENFHE_VER "$OPENFHE_VER"
record RUN_SEED "$RUN_SEED"
record BASELINE_REPEATS "$BASELINE_REPEATS"
record DURATION "$DURATION"
record INVALID_RANDOM_CHUNK "$INVALID_RANDOM_CHUNK"
record INVALID_RANDOM_MAX_ITERS "$INVALID_RANDOM_MAX_ITERS"
record GUIDED_COUNT "$GUIDED_COUNT"
record VALID_COUNT_BASIS generated
record eval_outdir "$EVAL_OUTDIR"
record commit "$(git rev-parse HEAD)"
record dirty "$(git status --porcelain | wc -l | tr -d ' ')"
SEED_COUNTER=0
RUN_DIR=""
run() {
  local label="$1" timed="$2"
  shift 2
  RUN_DIR="$EVAL_OUTDIR/$label"
  local seed=$((RUN_SEED + SEED_COUNTER)) rc
  SEED_COUNTER=$((SEED_COUNTER + 1))
  record "seed_$label" "$seed"
  record "started_$label" "$(date -u +%FT%TZ)"
  local prefix=()
  [[ "$timed" == yes ]] && prefix=("$TIMEOUT_BIN" -k 30s "$DURATION")
  {
    printf 'FHETEST_RUN_DIR=%q OpenFHE_DIR=%q ' "$RUN_DIR" "$OPENFHE_DIR"
    printf '%q ' ${prefix[@]+"${prefix[@]}"} "$ROOT/bin/fhetest" "$@" "-seed:$seed"
    printf '\n'
  } > "$EVAL_OUTDIR/$label.command.txt"
  set +e
  env FHETEST_RUN_DIR="$RUN_DIR" OpenFHE_DIR="$OPENFHE_DIR" \
    ${prefix[@]+"${prefix[@]}"} "$ROOT/bin/fhetest" "$@" "-seed:$seed" \
    > "$EVAL_OUTDIR/$label.log" 2>&1
  rc=$?
  set -e
  record "exit_$label" "$rc"
  record "finished_$label" "$(date -u +%FT%TZ)"
  if (( rc != 0 && rc != 124 )); then
    echo "Failed: $label; see $EVAL_OUTDIR/$label.log" >&2
    exit "$rc"
  fi
  [[ -d "$RUN_DIR" ]] || { echo "No result directory: $RUN_DIR; inspect $EVAL_OUTDIR/$label.log" >&2; exit 1; }
}
count_valid() { find "$1/succ" "$1/fail" "$1/psr_err" -name '*.json' | wc -l | tr -d ' '; }
count_invalid() { find "$1/exception" -name '*.json' | wc -l | tr -d ' '; }
count_generated() {
  awk '/^FHETEST_GENERATED=[0-9]+$/ {split($0, value, "="); count=value[2]} END {print count+0}' "$1"
}
guided_args=()
[[ -n "$GUIDED_COUNT" ]] && guided_args=(-count:"$GUIDED_COUNT")

for enc in int double; do
  run "RQ2-valid-$enc" yes test -type:"$enc" -stg:random -json:true -openfhe:"$OPENFHE_VER" ${guided_args[@]+"${guided_args[@]}"}
  record "valid_dir_$enc" "$RUN_DIR"
  record "valid_count_$enc" "$(count_valid "$RUN_DIR")"
  target="$(count_generated "$EVAL_OUTDIR/RQ2-valid-$enc.log")"
  record "valid_generated_count_$enc" "$target"
  if (( target == 0 )); then echo "No generated guided inputs for $enc; rebuild fhetest and inspect the log" >&2; exit 1; fi
  for (( repeat=1; repeat<=BASELINE_REPEATS; repeat++ )); do
    timed=no
    [[ "$MODE" == smoke ]] && timed=yes
    run "RQ2-random-$enc-repeat$repeat" "$timed" test -type:"$enc" -stg:random -json:true -openfhe:"$OPENFHE_VER" -nofilter:true -count:"$target"
    record "random_dir_${enc}_$repeat" "$RUN_DIR"
    actual="$(count_valid "$RUN_DIR")"
    record "random_count_${enc}_$repeat" "$actual"
    generated="$(count_generated "$EVAL_OUTDIR/RQ2-random-$enc-repeat$repeat.log")"
    record "random_generated_count_${enc}_$repeat" "$generated"
    if [[ "$MODE" == full && "$generated" != "$target" ]]; then
      echo "Baseline did not match generated target ($generated/$target)" >&2; exit 1
    fi
  done
  run "RQ2-invalid-$enc" yes test -type:"$enc" -stg:random -filter:false -json:true -openfhe:"$OPENFHE_VER" ${guided_args[@]+"${guided_args[@]}"}
  record "invalid_dir_$enc" "$RUN_DIR"
  target="$(count_invalid "$RUN_DIR")"
  record "invalid_random_exc_target_$enc" "$target"
  if (( target == 0 )); then echo "No guided exceptions for $enc" >&2; exit 1; fi
  for (( repeat=1; repeat<=BASELINE_REPEATS; repeat++ )); do
    total=0
    dirs=()
    for (( iter=1; iter<=INVALID_RANDOM_MAX_ITERS && total<target; iter++ )); do
      timed=no
      [[ "$MODE" == smoke ]] && timed=yes
      run "RQ2-invalid-random-$enc-repeat$repeat-iter$iter" "$timed" test -type:"$enc" -stg:random -filter:false -nofilter:true -json:true -openfhe:"$OPENFHE_VER" -count:"$INVALID_RANDOM_CHUNK"
      dirs+=("$RUN_DIR")
      total=$((total + $(count_invalid "$RUN_DIR")))
    done
    joined="$(IFS=,; echo "${dirs[*]}")"
    record "invalid_random_dirs_${enc}_$repeat" "$joined"
    record "invalid_random_exc_total_${enc}_$repeat" "$total"
    if [[ "$MODE" == full ]] && (( total < target )); then
      echo "Baseline did not reach exception target ($total/$target)" >&2; exit 1
    fi
  done
done
record RUN_FINISHED 1
printf 'Summary: %s\nAggregate: uv run evaluation/aggregate_rq2_rq3.py --summary %q' "$summary" "$summary"
[[ "$MODE" == smoke ]] && printf ' --allow-partial'
printf '\n'
