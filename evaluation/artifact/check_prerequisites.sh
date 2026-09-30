#!/usr/bin/env bash
set -euo pipefail
if [[ ${1:-} == --help || ${1:-} == -h ]]; then
  echo "Usage: $0 [--rq23]"
  echo 'Check tools and existing library package paths without installing or running experiments.'
  echo '--rq23 additionally requires uv for aggregation.'
  exit 0
fi
if [[ $# -gt 1 || ( $# -eq 1 && "$1" != --rq23 ) ]]; then
  echo "Usage: $0 [--rq23]" >&2
  exit 2
fi
failed=0
for tool in git cmake make c++ java javac javacc mvn sbt tar; do
  if command -v "$tool" >/dev/null 2>&1; then
    printf 'FOUND tool %s: %s\n' "$tool" "$(command -v "$tool")"
  else
    printf 'MISSING tool: %s\n' "$tool" >&2
    failed=1
  fi
done
if command -v timeout >/dev/null 2>&1 || command -v gtimeout >/dev/null 2>&1; then
  echo 'FOUND timeout command (GNU coreutils required)'
else
  echo 'MISSING GNU coreutils timeout/gtimeout' >&2
  failed=1
fi
if [[ ${1:-} == --rq23 ]] && ! command -v uv >/dev/null 2>&1; then
  echo 'MISSING uv for RQ2/RQ3 aggregation' >&2
  failed=1
fi
openfhe="${OpenFHE_DIR:-${OPENFHE_DIR:-}}"
seal="${SEAL_DIR:-}"
if [[ -n "$openfhe" && -f "$openfhe/OpenFHEConfig.cmake" ]]; then
  printf 'FOUND OpenFHE CMake package: %s\n' "$openfhe"
else
  echo 'MISSING OpenFHE_DIR/OpenFHEConfig.cmake; set the installed v1.4.2 package path' >&2
  failed=1
fi
if [[ -n "$seal" && -f "$seal/SEALConfig.cmake" ]]; then
  printf 'FOUND SEAL CMake package: %s\n' "$seal"
else
  echo 'MISSING SEAL_DIR/SEALConfig.cmake; set the installed v4.1.2 package path' >&2
  failed=1
fi
if (( failed == 0 )); then
  echo 'Prerequisites found. build_project.sh must still verify exact library versions and build compatibility.'
else
  echo 'Resolve the missing prerequisites before running build_project.sh.' >&2
fi
exit "$failed"
