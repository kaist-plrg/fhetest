#!/usr/bin/env bash
set -euo pipefail

usage() {
  echo "Usage: $0 INSTALL_ROOT"
  echo 'Build OpenFHE v1.4.2 and SEAL v4.1.2 without sudo.'
  echo 'Environment: JOBS=4, WITH_OPENMP=ON (Linux) / OFF (macOS).'
  echo 'Use a dedicated directory; sources, builds, logs and installs are retained.'
}
if [[ ${1:-} == --help ]]; then usage; exit 0; fi
if [[ $# -ne 1 ]]; then usage >&2; exit 2; fi
jobs="${JOBS:-4}"
[[ "$jobs" =~ ^[1-9][0-9]*$ ]] || { echo 'JOBS must be a positive integer' >&2; exit 2; }
case "$(uname -s)" in
  Linux) openmp="${WITH_OPENMP:-ON}" ;;
  Darwin) openmp="${WITH_OPENMP:-OFF}" ;;
  *) echo 'Supported hosts: Linux and macOS' >&2; exit 2 ;;
esac
[[ "$openmp" == ON || "$openmp" == OFF ]] || { echo 'WITH_OPENMP must be ON or OFF' >&2; exit 2; }
for tool in git cmake make; do
  command -v "$tool" >/dev/null || { echo "Missing dependency: $tool" >&2; exit 1; }
done
mkdir -p "$1"
prefix="$(cd "$1" && pwd)"
mkdir -p "$prefix/logs"
exec > >(tee "$prefix/logs/build-libraries.log") 2>&1
printf 'started_utc=%s\nJOBS=%s\nWITH_OPENMP=%s\n' "$(date -u +%FT%TZ)" "$jobs" "$openmp"

checkout() {
  local name="$1" url="$2" tag="$3" revision="$4"
  local source="$prefix/$name-src"
  if [[ ! -e "$source" ]]; then
    git clone --depth 1 --branch "$tag" "$url" "$source"
  fi
  [[ -e "$source/.git" ]] || { echo "Not a source checkout: $source" >&2; exit 1; }
  [[ "$(git -C "$source" rev-parse HEAD)" == "$revision" ]] || {
    echo "Unexpected revision in $source; use a new INSTALL_ROOT" >&2; exit 1;
  }
  [[ -z "$(git -C "$source" status --porcelain)" ]] || {
    echo "Source checkout has local changes: $source" >&2; exit 1;
  }
  printf '%s_commit=%s\n' "$name" "$revision"
}
checkout OpenFHE https://github.com/openfheorg/openfhe-development.git v1.4.2 aa391988d354d4360f390f223a90e0d1b98839d7
checkout SEAL https://github.com/microsoft/SEAL.git v4.1.2 119dc32e135cb89c1062076a69310d4413ebc824

git -C "$prefix/OpenFHE-src" submodule update --init --recursive
cmake -S "$prefix/OpenFHE-src" -B "$prefix/OpenFHE-build" \
  -DCMAKE_POLICY_VERSION_MINIMUM=3.5 -DCMAKE_BUILD_TYPE=Release \
  -DCMAKE_INSTALL_PREFIX="$prefix/OpenFHE-v1.4.2" \
  -DNATIVE_SIZE=64 -DMATHBACKEND=4 -DWITH_OPENMP="$openmp" \
  -DBUILD_SHARED=ON -DBUILD_STATIC=OFF \
  -DBUILD_UNITTESTS=OFF -DBUILD_EXAMPLES=OFF -DBUILD_BENCHMARKS=OFF
cmake --build "$prefix/OpenFHE-build" --parallel "$jobs"
cmake --install "$prefix/OpenFHE-build"
cmake -S "$prefix/SEAL-src" -B "$prefix/SEAL-build" \
  -DCMAKE_POLICY_VERSION_MINIMUM=3.5 -DCMAKE_BUILD_TYPE=Release \
  -DCMAKE_INSTALL_PREFIX="$prefix/SEAL-v4.1.2" \
  -DSEAL_BUILD_BENCH=OFF -DSEAL_BUILD_EXAMPLES=OFF -DSEAL_BUILD_TESTS=OFF \
  -DSEAL_USE_MSGSL=OFF -DSEAL_USE_ZLIB=OFF -DSEAL_USE_ZSTD=OFF
cmake --build "$prefix/SEAL-build" --parallel "$jobs"
cmake --install "$prefix/SEAL-build"
cp "$prefix/OpenFHE-build/CMakeCache.txt" "$prefix/logs/OpenFHE-CMakeCache.txt"
cp "$prefix/SEAL-build/CMakeCache.txt" "$prefix/logs/SEAL-CMakeCache.txt"
{
  printf 'export OpenFHE_DIR=%q\n' "$prefix/OpenFHE-v1.4.2/lib/OpenFHE"
  printf 'export OPENFHE_DIR=%q\n' "$prefix/OpenFHE-v1.4.2/lib/OpenFHE"
  printf 'export SEAL_DIR=%q\n' "$prefix/SEAL-v4.1.2/lib/cmake/SEAL-4.1"
} > "$prefix/env.sh"
printf 'finished_utc=%s\n' "$(date -u +%FT%TZ)"
printf 'Next: source %q\n' "$prefix/env.sh"
