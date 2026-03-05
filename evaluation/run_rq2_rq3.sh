#!/usr/bin/env bash
set -euo pipefail

MODE="${1:-smoke}" # smoke | full
OPENFHE_DIR="${OPENFHE_DIR:-/usr/local/OpenFHE-v1.4.2/lib/OpenFHE}"
OPENFHE_VER="${OPENFHE_VER:-1.4.2}"

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
RUN_TS="$(date +%Y%m%d_%H%M%S)"
EVAL_OUTDIR="$ROOT/evaluation/evaluation-${RUN_TS}"
mkdir -p "$EVAL_OUTDIR"

if command -v gtimeout >/dev/null 2>&1; then
  TIMEOUT_BIN="gtimeout"
elif command -v timeout >/dev/null 2>&1; then
  TIMEOUT_BIN="timeout"
else
  echo "ERROR: gtimeout/timeout not found. Install coreutils for gtimeout." >&2
  exit 1
fi

case "$MODE" in
  smoke)
    DURATION="60s"
    TIMEBOX_ALL="1"
    INVALID_RANDOM_CHUNK="${INVALID_RANDOM_CHUNK:-2}"
    INVALID_RANDOM_MAX_ITERS="${INVALID_RANDOM_MAX_ITERS:-1}"
    ;;
  full)
    DURATION="24h"
    TIMEBOX_ALL="0"
    INVALID_RANDOM_CHUNK="${INVALID_RANDOM_CHUNK:-200}"
    INVALID_RANDOM_MAX_ITERS="${INVALID_RANDOM_MAX_ITERS:-1000}"
    ;;
  *)
    echo "Usage: $0 [smoke|full]" >&2
    exit 1
    ;;
esac

summary="$EVAL_OUTDIR/rq2_rq3_run_${MODE}_${RUN_TS}.txt"
echo "MODE=$MODE" | tee -a "$summary"
echo "OPENFHE_DIR=$OPENFHE_DIR" | tee -a "$summary"
echo "OPENFHE_VER=$OPENFHE_VER" | tee -a "$summary"
echo "TIMEOUT_BIN=$TIMEOUT_BIN DURATION=$DURATION" | tee -a "$summary"
echo "eval_outdir=$EVAL_OUTDIR" | tee -a "$summary"

run_timeboxed() {
  local label="$1"
  shift
  echo "== $label ==" | tee -a "$summary"
  set +e
  env OpenFHE_DIR="$OPENFHE_DIR" \
    "$TIMEOUT_BIN" "$DURATION" \
    "$ROOT/bin/fhetest" "$@"
  local rc=$?
  set -e
  echo "exit=$rc" | tee -a "$summary"
}

run_counted() {
  local label="$1"
  shift
  echo "== $label ==" | tee -a "$summary"
  env OpenFHE_DIR="$OPENFHE_DIR" \
    "$ROOT/bin/fhetest" "$@"
}

latest_test_dir() { ls -td "$ROOT"/logs/test-* 2>/dev/null | head -1; }
latest_invalid_dir() { ls -td "$ROOT"/logs/test-invalid-* 2>/dev/null | head -1; }
count_valid() {
  local dir="$1"
  find "$dir"/succ "$dir"/fail "$dir"/psr_err -name '*.json' 2>/dev/null | wc -l | tr -d ' '
}
count_invalid_ex() {
  local dir="$1"
  find "$dir"/exception -name '*.json' 2>/dev/null | wc -l | tr -d ' '
}
count_invalid_expected() {
  local dir="$1"
  find "$dir"/exception/expected -name '*.json' 2>/dev/null | wc -l | tr -d ' '
}
count_invalid_unexpected() {
  local dir="$1"
  find "$dir"/exception/unexpected -name '*.json' 2>/dev/null | wc -l | tr -d ' '
}

run_valid_pair() {
  local enc="$1"
  run_timeboxed "RQ2-valid-${enc}" test -type:"$enc" -stg:random -json:true -openfhe:"$OPENFHE_VER"
  local dir
  dir="$(latest_test_dir)"
  echo "valid_dir_${enc}=$dir" | tee -a "$summary"
  local cnt
  cnt="$(count_valid "$dir")"
  echo "valid_count_${enc}=$cnt" | tee -a "$summary"

  if [[ "$cnt" == "0" ]]; then
    echo "WARN: valid_count_${enc} is 0; skip RQ2-random-${enc}" | tee -a "$summary"
    return
  fi

  if [[ "$TIMEBOX_ALL" == "1" ]]; then
    run_timeboxed "RQ2-random-${enc}" test -type:"$enc" -stg:random -json:true -openfhe:"$OPENFHE_VER" -nofilter:true -count:"$cnt"
  else
    run_counted "RQ2-random-${enc}" test -type:"$enc" -stg:random -json:true -openfhe:"$OPENFHE_VER" -nofilter:true -count:"$cnt"
  fi

  local dir2
  dir2="$(latest_test_dir)"
  echo "random_dir_${enc}=$dir2" | tee -a "$summary"
}

run_invalid_pair() {
  local enc="$1"
  run_timeboxed "RQ2-invalid-${enc}" test -type:"$enc" -stg:random -filter:false -json:true -openfhe:"$OPENFHE_VER"
  local dir
  dir="$(latest_invalid_dir)"
  echo "invalid_dir_${enc}=$dir" | tee -a "$summary"
  local exc exp unexp
  exc="$(count_invalid_ex "$dir")"
  exp="$(count_invalid_expected "$dir")"
  unexp="$(count_invalid_unexpected "$dir")"
  echo "invalid_exc_count_${enc}=$exc" | tee -a "$summary"
  echo "invalid_expected_${enc}=$exp" | tee -a "$summary"
  echo "invalid_unexpected_${enc}=$unexp" | tee -a "$summary"

  # Random baseline for invalid programs: match the number of exception messages.
  local target_exc="${RAND_INVALID_COUNT:-$exc}"
  echo "invalid_random_exc_target_${enc}=$target_exc" | tee -a "$summary"

  local total_exc=0
  local iter=0
  local dirs=()
  while [[ "$total_exc" -lt "$target_exc" && "$iter" -lt "$INVALID_RANDOM_MAX_ITERS" ]]; do
    iter=$((iter + 1))
    run_counted "RQ2-invalid-random-${enc}-iter${iter}" test -type:"$enc" -stg:random -filter:false -nofilter:true -json:true -openfhe:"$OPENFHE_VER" -count:"$INVALID_RANDOM_CHUNK"
    local dir2
    dir2="$(latest_invalid_dir)"
    dirs+=("$dir2")
    local exc2
    exc2="$(count_invalid_ex "$dir2")"
    total_exc=$((total_exc + exc2))
    echo "invalid_random_iter_${enc}_${iter}_dir=$dir2" | tee -a "$summary"
    echo "invalid_random_iter_${enc}_${iter}_exc=$exc2" | tee -a "$summary"
  done
  echo "invalid_random_exc_total_${enc}=$total_exc" | tee -a "$summary"
  if [[ "${#dirs[@]}" -gt 0 ]]; then
    local joined
    joined="$(IFS=,; echo "${dirs[*]}")"
    echo "invalid_random_dirs_${enc}=$joined" | tee -a "$summary"
  fi
}

run_valid_pair int
run_valid_pair double
run_invalid_pair int
run_invalid_pair double

echo "Summary written to: $summary"
