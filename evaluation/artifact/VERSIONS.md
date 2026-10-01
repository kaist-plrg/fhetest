# Source versions and build settings

This artifact uses the library versions evaluated in the paper.

## Source revisions

| Component | Version / revision | Evidence |
| --- | --- | --- |
| Extended T2 compiler | `c5c8607657d1f1c5ba096b078b84be6b9b8e1c82` | Repository submodule gitlink |
| OpenFHE | v1.4.2, `aa391988d354d4360f390f223a90e0d1b98839d7` | Upstream tag |
| Microsoft SEAL | v4.1.2, `119dc32e135cb89c1062076a69310d4413ebc824` | Upstream tag |
| Scala | 3.3.1 | `build.sbt` |
| sbt | 1.9.7 | `project/build.properties` |

Repositories:

- Extended T2: https://github.com/kaist-plrg/T2-FHE-Compiler-and-Benchmarks
- OpenFHE: https://github.com/openfheorg/openfhe-development
- SEAL: https://github.com/microsoft/SEAL

## Project requirements

The fhetest consumer projects require OpenFHE 1.4.2 and SEAL 4.1.2 with CMake
`EXACT`. The OpenFHE consumer sets C++17 and defaults `BUILD_STATIC=OFF`.
T2's Maven configuration targets Java 8 bytecode.
The project build commands are `sbt buildT2` and `sbt assembly`.

## Experimental environment

The paper's experiments were conducted on an Ubuntu machine with a 16-core
AMD Ryzen 9 9950X processor and 128 GB of RAM.

## Artifact build settings

`build_libraries.sh` selects Release, OpenFHE NATIVE_SIZE=64 / MATHBACKEND=4,
shared libraries, and OpenMP ON on Linux or OFF on macOS (overridable).
OpenFHE's `LIBINSTALL` points to the installation's `lib` directory so its
shared libraries can resolve their dependencies at runtime.
SEAL's optional MSGSL, zlib and zstd integrations are disabled. Both libraries
disable tests, examples and benchmarks. The installation prefix is user-selected.
The script saves full CMake caches and a build log next to the installation.

`build_project.sh` can instead use existing library installations via
`OpenFHE_DIR` and `SEAL_DIR`. It checks exact package versions and runs context
creation probes before building T2 and fhetest.
