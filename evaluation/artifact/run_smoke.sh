#!/usr/bin/env bash
set -euo pipefail
if [[ ${1:-} == --help || ${1:-} == -h ]]; then
  echo "Usage: $0"
  echo 'Build committed HEAD in a separate checkout and run bounded RQ2/RQ3 checks on Ubuntu.'
  echo 'Requires existing build tools, uv, OpenFHE 1.4.2 and SEAL 4.1.2.'
  echo 'Set OpenFHE_DIR and SEAL_DIR for non-default installation paths.'
  exit 0
fi
if [[ $# -ne 0 ]]; then echo "Usage: $0" >&2; exit 2; fi
exec 3>&1
root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
revision="$(git -C "$root" rev-parse HEAD)"
export OpenFHE_DIR="${OpenFHE_DIR:-${OPENFHE_DIR:-/usr/local/OpenFHE-v1.4.2/lib/OpenFHE}}"
export SEAL_DIR="${SEAL_DIR:-/usr/local/SEAL-v4.1.2/lib/cmake/SEAL-4.1}"
export PATH="$HOME/.local/bin:$PATH"
delivery_dir="$(mktemp -d "$HOME/fhetest-smoke.XXXXXX")"
mkdir "$delivery_dir/results"
cp "${BASH_SOURCE[0]}" "$delivery_dir/results/run_smoke.sh"
finish() {
  local rc=$?
  trap - EXIT
  printf 'exit_code=%s\n' "$rc" > "$delivery_dir/results/completion.txt"
  if ! tar -czf "$delivery_dir/results.tar.gz" -C "$delivery_dir" results; then
    echo "Archive failed; logs remain in $delivery_dir/results" >&2
    exit 1
  fi
  printf '\nResults: %s/results.tar.gz\nExit code: %s\n' "$delivery_dir" "$rc" >&3
  exit "$rc"
}
trap finish EXIT
printf 'Working directory: %s\nTesting committed revision: %s\n' "$delivery_dir" "$revision"
{
  date -u
  uname -a
  free -h
  git clone --no-hardlinks "$root" "$delivery_dir/repo"
  cd "$delivery_dir/repo"
  git checkout --detach "$revision"
  git submodule update --init src/main/java/T2-FHE-Compiler-and-Benchmarks
  bash evaluation/artifact/check_prerequisites.sh --rq23
  bash evaluation/artifact/build_project.sh "$delivery_dir/results/build"
} > "$delivery_dir/results/setup.log" 2>&1

RUN_SEED=20260929 BASELINE_REPEATS=1 GUIDED_COUNT=2 \
INVALID_RANDOM_CHUNK=2 INVALID_RANDOM_MAX_ITERS=1 \
DURATION=5m CHECK_TIMEOUT=45m \
bash evaluation/artifact/run_checks.sh "$delivery_dir/results/rq23" rq23
