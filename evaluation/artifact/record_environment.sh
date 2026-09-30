#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 1 ]]; then
  echo "Usage: $0 OUTPUT_DIR" >&2
  exit 2
fi

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
output_dir="$1"
mkdir -p "$output_dir"
output_dir="$(cd "$output_dir" && pwd)"

show_version() {
  local name="$1"
  shift
  if command -v "$1" >/dev/null 2>&1; then
    printf '\n[%s]\n' "$name"
    "$@" 2>&1 | sed -n '1,2p'
  else
    printf '\n[%s]\nnot installed\n' "$name"
  fi
}

{
  printf 'recorded_utc=%s\n' "$(date -u +%Y-%m-%dT%H:%M:%SZ)"
  printf 'fhetest_commit=%s\n' "$(git -C "$root" rev-parse HEAD)"
  printf 'fhetest_dirty=%s\n' "$(if [[ -n "$(git -C "$root" status --porcelain)" ]]; then echo yes; else echo no; fi)"
  t2_dir="$root/src/main/java/T2-FHE-Compiler-and-Benchmarks"
  if [[ -e "$t2_dir/.git" ]] && git -C "$t2_dir" rev-parse HEAD >/dev/null 2>&1; then
    printf 't2_commit=%s\n' "$(git -C "$t2_dir" rev-parse HEAD)"
  else
    printf 't2_commit=not initialized\n'
  fi
  printf 'openfhe_dir=%s\n' "${OpenFHE_DIR:-unset}"
  printf 'seal_dir=%s\n' "${SEAL_DIR:-unset}"
  for library in OpenFHE-v1.4.2 SEAL-v4.1.2; do
    source_dir="$t2_dir/$library"
    if [[ -e "$source_dir/.git" ]] && git -C "$source_dir" rev-parse HEAD >/dev/null 2>&1; then
      printf '%s_commit=%s\n' "$library" "$(git -C "$source_dir" rev-parse HEAD)"
    else
      printf '%s_commit=not found\n' "$library"
    fi
  done
  printf '\n[system]\n'
  uname -srm
  if [[ -f /etc/os-release ]]; then cat /etc/os-release; fi
  printf '\n[sbt project version]\n'
  cat "$root/project/build.properties"
  show_version git git --version
  show_version cmake cmake --version
  show_version gcc gcc --version
  show_version g++ g++ --version
  show_version java java -version
  show_version javacc javacc -version
  show_version mvn mvn -version
  show_version sbt-launcher sbt --script-version
} > "$output_dir/environment.txt"

printf 'Environment written to: %s\n' "$output_dir/environment.txt"
