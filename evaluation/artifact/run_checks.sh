#!/usr/bin/env bash
set -euo pipefail
usage() {
  echo "Usage: $0 NEW_OUTPUT_DIR [core|rq1|rq4|seed|rq23]"
  echo 'Default core: record environment, RQ1 interpreter example, RQ4 tests.'
  echo 'seed: fresh-JVM generation checks; rq23: bounded smoke only, no full experiment.'
  echo 'Each invocation preserves command/log/status files and creates a .tar.gz bundle.'
}
if [[ ${1:-} == --help || ${1:-} == -h ]]; then usage; exit 0; fi
if [[ $# -lt 1 || $# -gt 2 ]]; then usage >&2; exit 2; fi
mode="${2:-core}"
case "$mode" in core|rq1|rq4|seed|rq23) ;; *) usage >&2; exit 2 ;; esac
root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$root"
if command -v gtimeout >/dev/null 2>&1; then timer=gtimeout
elif command -v timeout >/dev/null 2>&1; then timer=timeout
else echo 'Install GNU coreutils (timeout/gtimeout)' >&2; exit 1; fi
limit="${CHECK_TIMEOUT:-20m}"
[[ ! -e "$1" && ! -e "$1.tar.gz" ]] || { echo 'Use a new output path' >&2; exit 2; }
mkdir -p "$1"
out="$(cd "$1" && pwd)"
# A run bundle can contain machine paths and experiment data; review before sharing.
printf 'stage\texit_code\tstarted_utc\tfinished_utc\n' > "$out/status.tsv"
overall=0
# shellcheck disable=SC2329 # Invoked by the EXIT trap.
finish() {
  local rc=$?
  trap - EXIT
  printf 'wrapper_exit=%s\nfinished_utc=%s\n' "$rc" "$(date -u +%FT%TZ)" > "$out/completion.txt"
  if tar -czf "$out.tar.gz" -C "$(dirname "$out")" "$(basename "$out")"; then
    printf 'Bundle: %s.tar.gz\n' "$out"
  else
    echo "Archive failed; raw records remain at $out" >&2
    rc=1
  fi
  exit "$rc"
}
trap finish EXIT
trap 'exit 130' INT
trap 'exit 143' TERM
run() {
  local label="$1" started rc
  shift
  started="$(date -u +%FT%TZ)"
  printf '%q ' "$timer" -k 30s "$limit" "$@" > "$out/$label.command.txt"
  printf '\n' >> "$out/$label.command.txt"
  echo "Running $label (limit $limit); log: $out/$label.log"
  set +e
  "$timer" -k 30s "$limit" "$@" > "$out/$label.log" 2>&1
  rc=$?
  set -e
  printf '%s\t%s\t%s\t%s\n' "$label" "$rc" "$started" "$(date -u +%FT%TZ)" >> "$out/status.tsv"
  if (( rc != 0 )); then overall=1; fi
  echo "$label exit=$rc"
}
printf 'mode=%s\ncheck_timeout=%s\n' "$mode" "$limit" > "$out/settings.txt"
run environment bash evaluation/artifact/record_environment.sh "$out"
case "$mode" in
  core|rq1)
    input=src/main/resources/paper/logistic_regression_a4_fp_paper.t2
    cp "$input" "$out/rq1-input.t2"
    run rq1-interpreter bin/fhetest interp -file:"$input" -n:32768 -m:65537
    ;;
esac
case "$mode" in
  core|rq4)
    run rq4-interpreter sbt -Dsbt.color=false 'testOnly BasicInterpTest'
    report=target/test-reports/TEST-BasicInterpTest.xml
    if [[ -f "$report" && "$report" -nt "$out/rq4-interpreter.command.txt" ]]; then
      cp "$report" "$out/rq4-test-report.xml"
    fi
    ;;
esac
case "$mode" in
  seed) run seed bash evaluation/artifact/check_seed.sh "$out/seed" ;;
  rq23)
    run rq23-smoke env EVAL_OUTDIR="$out/rq23" bash evaluation/run_rq2_rq3.sh smoke
    manifests=("$out"/rq23/rq2_rq3_run_*.txt)
    if [[ -f "${manifests[0]}" ]]; then
      run rq23-aggregate uv run evaluation/aggregate_rq2_rq3.py --summary "${manifests[0]}" --allow-partial
    fi
    ;;
esac
exit "$overall"
