# Source versions and build settings

This artifact retains the library versions used in the paper. The immutable
repository link in the paper identifies the artifact snapshot.

## Source revisions

| Component | Version / revision | Evidence |
| --- | --- | --- |
| fhetest | Record `git rev-parse HEAD` | Check out the immutable revision linked from the paper |
| Extended T2 compiler | `c5c8607657d1f1c5ba096b078b84be6b9b8e1c82` | Repository submodule gitlink |
| OpenFHE | v1.4.2, `aa391988d354d4360f390f223a90e0d1b98839d7` | Upstream tag, checked on 2026-09-28 |
| Microsoft SEAL | v4.1.2, `119dc32e135cb89c1062076a69310d4413ebc824` | Upstream tag, checked on 2026-09-28 |
| Scala | 3.3.1 | `build.sbt` |
| sbt | 1.9.7 | `project/build.properties` |

Repositories:

- fhetest: https://github.com/kaist-plrg/fhetest
- Extended T2: https://github.com/kaist-plrg/T2-FHE-Compiler-and-Benchmarks
- OpenFHE: https://github.com/openfheorg/openfhe-development
- SEAL: https://github.com/microsoft/SEAL

For each run, record the actual
checkout with `record_environment.sh`. A dirty checkout is not fully identified
by HEAD: preserve the local diff or use a clean committed checkout.

## Project requirements

The fhetest consumer projects require OpenFHE 1.4.2 and SEAL 4.1.2 with CMake
`EXACT`. The OpenFHE consumer sets C++17 and defaults `BUILD_STATIC=OFF`.
T2's Maven configuration targets Java 8 bytecode.
The project build commands are `sbt buildT2` and `sbt assembly`.

`record_environment.sh` records the JavaCC Debian/Ubuntu package version when
available.

## Environment provenance

The paper reports a 16-core AMD Ryzen 9 9950X and 128 GB RAM. Original server
build caches are not included. The artifact's setup and basic checks were
validated separately on Ubuntu 24.04, four CPU cores and 16 GB RAM, using
OpenJDK 17.0.20.1, Maven 3.9.16, JavaCC package 7.0.12-1, CMake 3.28.3 and
GCC 13.3.0. Record the actual tool versions and build caches for each run.

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
