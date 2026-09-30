#!/usr/bin/env bash
set -euo pipefail

usage() {
  echo "Usage: $0 OUTPUT_DIR"
  echo 'Requires OpenFHE_DIR and SEAL_DIR pointing to installed CMake packages.'
  echo 'Checks both libraries, builds T2 and fhetest, and saves logs.'
  echo 'Dependencies: git, cmake, make, C++ compiler, java, javacc, mvn, sbt.'
}
if [[ ${1:-} == --help ]]; then usage; exit 0; fi
if [[ $# -ne 1 ]]; then usage >&2; exit 2; fi
root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
export OpenFHE_DIR="${OpenFHE_DIR:-${OPENFHE_DIR:-}}"
: "${OpenFHE_DIR:?Set OpenFHE_DIR to the OpenFHE v1.4.2 CMake package directory}"
: "${SEAL_DIR:?Set SEAL_DIR to the SEAL v4.1.2 CMake package directory}"
export SEAL_DIR
for tool in git cmake make java javacc mvn sbt; do
  command -v "$tool" >/dev/null || { echo "Missing dependency: $tool" >&2; exit 1; }
done
mkdir -p "$1"
output="$(cd "$1" && pwd)"
exec > >(tee "$output/build-project.log") 2>&1
printf 'started_utc=%s\n' "$(date -u +%FT%TZ)"
t2="$root/src/main/java/T2-FHE-Compiler-and-Benchmarks"
expected_t2=c5c8607657d1f1c5ba096b078b84be6b9b8e1c82
[[ "$(git -C "$root" rev-parse HEAD:src/main/java/T2-FHE-Compiler-and-Benchmarks)" == "$expected_t2" ]] || {
  echo 'Unexpected T2 gitlink; this setup targets the paper artifact revision' >&2; exit 1;
}
if [[ ! -e "$t2/.git" ]]; then
  git -C "$root" -c submodule.src/main/java/T2-FHE-Compiler-and-Benchmarks.url=https://github.com/kaist-plrg/T2-FHE-Compiler-and-Benchmarks.git submodule update --init src/main/java/T2-FHE-Compiler-and-Benchmarks
fi
[[ "$(git -C "$t2" rev-parse HEAD)" == "$expected_t2" ]] || { echo 'Unexpected T2 revision' >&2; exit 1; }
if ! git -C "$t2" diff --quiet || ! git -C "$t2" diff --cached --quiet; then
  echo 'T2 has tracked local changes; use a clean checkout' >&2; exit 1
fi
probe="$output/library-check"
mkdir -p "$probe"
cat > "$probe/CMakeLists.txt" <<'CMAKE'
cmake_minimum_required(VERSION 3.13)
project(artifact_library_check LANGUAGES CXX)
set(CMAKE_CXX_STANDARD 17)
find_package(OpenFHE 1.4.2 EXACT REQUIRED)
find_package(SEAL 4.1.2 EXACT REQUIRED)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${OpenFHE_CXX_FLAGS}")
include_directories(${OPENMP_INCLUDES} ${OpenFHE_INCLUDE} ${OpenFHE_INCLUDE}/third-party/include ${OpenFHE_INCLUDE}/core ${OpenFHE_INCLUDE}/pke ${OpenFHE_INCLUDE}/binfhe)
link_directories(${OpenFHE_LIBDIR} ${OPENMP_LIBRARIES})
add_executable(check_openfhe openfhe.cpp)
target_link_libraries(check_openfhe ${OpenFHE_SHARED_LIBRARIES})
add_executable(check_seal seal.cpp)
if(TARGET SEAL::seal)
  target_link_libraries(check_seal SEAL::seal)
else()
  target_link_libraries(check_seal SEAL::seal_shared)
endif()
CMAKE
cat > "$probe/openfhe.cpp" <<'CPP'
#include "openfhe.h"
int main() {
  lbcrypto::CCParams<lbcrypto::CryptoContextCKKSRNS> parameters;
  parameters.SetMultiplicativeDepth(1);
  parameters.SetScalingModSize(40);
  return lbcrypto::GenCryptoContext(parameters) ? 0 : 1;
}
CPP
cat > "$probe/seal.cpp" <<'CPP'
#include <seal/seal.h>
int main() {
  seal::EncryptionParameters parameters(seal::scheme_type::ckks);
  parameters.set_poly_modulus_degree(8192);
  parameters.set_coeff_modulus(seal::CoeffModulus::Create(8192, {60, 40, 60}));
  return seal::SEALContext(parameters).parameters_set() ? 0 : 1;
}
CPP
cmake -S "$probe" -B "$probe/build" -DOpenFHE_DIR="$OpenFHE_DIR" -DSEAL_DIR="$SEAL_DIR"
cmake --build "$probe/build" --parallel 2
"$probe/build/check_openfhe"
"$probe/build/check_seal"
cp "$probe/build/CMakeCache.txt" "$output/library-check-CMakeCache.txt"
cd "$t2"
mvn -B package -Dmaven.test.skip
cd "$root"
sbt buildT2 assembly
"$root/bin/fhetest" help
bash "$root/evaluation/artifact/record_environment.sh" "$output"
printf 'finished_utc=%s\n' "$(date -u +%FT%TZ)"
