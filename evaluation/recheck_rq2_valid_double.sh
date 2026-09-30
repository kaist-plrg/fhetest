#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 2 ]]; then
  echo "Usage: $0 ARCHIVED_VALID_DOUBLE_DIR REPORT_DIR" >&2
  exit 2
fi

root="$(cd "$(dirname "$0")/.." && pwd)"
archive="$(cd "$1" && pwd)"
mkdir -p "$2"
report="$(cd "$2" && pwd)"
expected_count="${EXPECTED_COUNT:-6183}"
openfhe_dir="${OPENFHE_DIR:-/usr/local/OpenFHE-v1.4.2/lib/OpenFHE}"

if [[ ! -d "$archive/succ" || ! -d "$archive/fail" || ! -d "$archive/psr_err" ]]; then
  echo "Archive must contain succ/, fail/, and psr_err/: $archive" >&2
  exit 2
fi
if [[ ! -d "$openfhe_dir" ]]; then
  echo "OpenFHE directory does not exist: $openfhe_dir" >&2
  exit 2
fi

cd "$root"
sbt buildT2 assembly

set +e
env OpenFHE_DIR="$openfhe_dir" "$root/bin/fhetest" recheck \
  -dir:"$archive" -openfhe:1.4.2 -count:"$expected_count" \
  > "$report/recheck.log" 2>&1
result=$?
set -e

grep '^RECHECK,' "$report/recheck.log" > "$report/recheck.csv" || true
grep '^RECHECK_SUMMARY,' "$report/recheck.log" > "$report/summary.txt" || true
printf 'Exit status: %s\nFull log: %s\nComparison CSV: %s\nSummary: %s\n' \
  "$result" "$report/recheck.log" "$report/recheck.csv" "$report/summary.txt"
exit "$result"
