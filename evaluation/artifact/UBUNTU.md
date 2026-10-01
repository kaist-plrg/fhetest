# HEProgTest artifact: start on your own Ubuntu machine

Install the dependencies, build HEProgTest and run the RQ-specific procedures
below. Use the artifact revision linked from the paper.

## 1. Prepare the machine and toolchain

Use Bash and run the blocks in order, stopping on errors. Administrator access
is needed for system packages; subsequent builds/installations use your home
directory. Internet access is needed for GitHub, Maven/sbt dependencies and,
optionally, Python packages. Setup and basic checks were exercised on Ubuntu
24.04 with four CPU cores and 16 GB RAM.
`JOBS=2` below limits library build parallelism.

```sh
sudo apt-get update
sudo apt-get install -y build-essential cmake git curl ca-certificates \
  openjdk-17-jdk maven javacc coreutils tar python3
java -version
javac -version
mvn -version
```

Ubuntu 24.04 provides [OpenJDK 17](https://packages.ubuntu.com/noble/openjdk-17-jdk)
and [JavaCC](https://packages.ubuntu.com/noble/javacc); JavaCC is in the Universe
component, which must be enabled in your package sources. If several JDKs are
installed, select the intended JDK consistently for Java, Maven and sbt.

Install the sbt 1.9.7 launcher in a new user-owned tools directory:

```sh
artifact_tools="$HOME/heprogtest-tools"
mkdir "$artifact_tools"
curl -fL https://github.com/sbt/sbt/releases/download/v1.9.7/sbt-1.9.7.tgz \
  -o "$artifact_tools/sbt-1.9.7.tgz"
tar -xzf "$artifact_tools/sbt-1.9.7.tgz" -C "$artifact_tools"
export PATH="$artifact_tools/sbt/bin:$PATH"
sbt --script-version
```

This uses the universal package installation route in the
[official sbt Linux guide](https://www.scala-sbt.org/1.x/docs/Installing-sbt-on-Linux.html)
and the [1.9.7 release](https://github.com/sbt/sbt/releases/tag/v1.9.7).
The repository separately pins sbt 1.9.7 and Scala 3.3.1; a standalone Scala
installation is not necessary. If the tools directory already exists, use a new
name or your existing sbt installation rather than overwriting it.

## 2. Obtain the artifact sources

```sh
git clone --branch artifact/reproducibility https://github.com/kaist-plrg/fhetest.git heprogtest-artifact
cd heprogtest-artifact
git rev-parse HEAD
git submodule update --init src/main/java/T2-FHE-Compiler-and-Benchmarks
```

The branch can change. Check out the immutable commit identified in the paper before
initializing the submodule. See [VERSIONS.md](VERSIONS.md) for the pinned T2 and
library revisions.
Use a Git checkout, not an exported source ZIP. Run one experiment at a time per
checkout because generated backend build files are shared.

## 3. Build the two pinned libraries and the project

From the repository root, in the same shell:

```sh
artifact_deps="$HOME/heprogtest-deps"
artifact_runs="$HOME/heprogtest-runs-$(date +%Y%m%d-%H%M%S)"
mkdir "$artifact_runs"
JOBS=2 bash evaluation/artifact/build_libraries.sh "$artifact_deps"
source "$artifact_deps/env.sh"
bash evaluation/artifact/check_prerequisites.sh
bash evaluation/artifact/build_project.sh "$artifact_runs/build"
```

Choose a new dependencies directory for a new toolchain. The script checks
OpenFHE v1.4.2 and SEAL v4.1.2 source hashes, builds Release libraries and installs
them without sudo. Its Linux OpenFHE build enables OpenMP. Effective CMake caches
and logs are saved under `$artifact_deps/logs`; see [VERSIONS.md](VERSIONS.md)
for the build options.

The prerequisite check reports tools/package files; the project build then
checks **exact** library versions, compiles and runs native context-creation
probes, builds T2 and fhetest, and invokes `fhetest help`.
Preserve `$artifact_runs/build` on failure.

In a later shell, return to the checkout root, restore the sbt PATH, and source
`$HOME/heprogtest-deps/env.sh` again (or your chosen dependency path).

## 4. Check basic execution

```sh
bash evaluation/artifact/run_checks.sh "$artifact_runs/core" core
```

Expected results:

| Check | Expected evidence |
| --- | --- |
| Environment | `environment.txt` contains the actual checkout/toolchain |
| RQ1 interpreter | Exit 0, numeric output `209 2936 12467` |
| RQ4 interpreter | 15 tests run, 15 succeeded; exit 0 |
| Wrapper | Exit 0; `status.tsv`, logs and `core.tar.gz` created |

Each wrapper stage has a 20-minute default time limit plus a 30-second kill grace.
Set `CHECK_TIMEOUT=40m` if necessary. Logs are retained even on ordinary failure.

## 5. Select the RQ-specific procedure

| Task | Procedure |
| --- | --- |
| Seeded generation | `bash evaluation/artifact/run_checks.sh "$artifact_runs/seed" seed`; checks six modes using fresh JVMs |
| RQ1 | [RQ1.md](RQ1.md): representative input through the interpreter, OpenFHE and SEAL |
| RQ2/RQ3 | [RQ2_RQ3.md](RQ2_RQ3.md): smoke/full execution, count matching, repeated baselines, aggregation |
| RQ3 inspection | [RQ3.md](RQ3.md): expected/unexpected exception counts and manual inspection |
| RQ4 | [RQ4.md](RQ4.md): T2 implementation, interpreter suite and timing interpretation |
| RQ5 | [RQ5.md](RQ5.md): sources for 18 reports and an OpenFHE 1.4.2 parameter check |

For RQ2/RQ3 aggregation, install uv using its
[official installation instructions](https://docs.astral.sh/uv/getting-started/installation/):

```sh
curl -LsSf https://astral.sh/uv/install.sh -o "$artifact_tools/uv-install.sh"
env UV_NO_MODIFY_PATH=1 sh "$artifact_tools/uv-install.sh"
export PATH="$HOME/.local/bin:$PATH"
uv --version
bash evaluation/artifact/check_prerequisites.sh --rq23
bash evaluation/artifact/run_checks.sh "$artifact_runs/pipeline" pipeline
```

Record the installed uv version with `uv --version`. Python
3.10+ is required by aggregation, and the script pins its Pydantic dependency.
The `pipeline` wrapper checks small fixed inputs, JSON recording and aggregation.
The optional randomized `rq23` check is described in [RQ2_RQ3.md](RQ2_RQ3.md);
it can time out or finish without enough recorded inputs. Full runs can take
days.

For a combined build and fixed-input pipeline check on Ubuntu, after installing the
tools, uv and libraries above, run `bash evaluation/artifact/run_smoke.sh`.
It tests the current committed HEAD in a separate checkout under your home
directory; uncommitted changes are not included. It uses small fixed BFV/CKKS
inputs, including invalid modulus parameters, and checks native execution,
JSON recording and aggregation. Each wrapper stage has a 15-minute limit plus
a 30-second kill grace; project build time is separate.
The script prints the path to `results.tar.gz`, containing build logs and any
execution/aggregation records, including on ordinary failure.

## Data and outputs

[DATA.md](DATA.md) describes the included evaluation records;
[GENERATION.md](GENERATION.md) describes generation rules.

Keep setup logs, the tested commit and each run's configuration/results together.
