# HEProgTest artifact: start on your own Ubuntu machine

Run the following steps in order in Bash. After cloning, use the repository root.

## 1. Prepare the machine and toolchain

Install system packages on Ubuntu 24.04. Subsequent builds use your home directory.
Setup and fixed-input checks were tested with four CPU cores and 16 GB RAM.

```sh
sudo apt-get update
sudo apt-get install -y build-essential cmake git curl ca-certificates \
  openjdk-17-jdk maven javacc coreutils tar python3
```

JavaCC requires Ubuntu's Universe repository. If multiple JDKs are installed,
use the same JDK for Java, Maven and sbt.

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

Scala 3.3.1 is managed by sbt; no separate Scala installation is needed.
If the tools directory exists, use your existing installation or a new directory.

## 2. Obtain the artifact sources

```sh
git clone --branch artifact/reproducibility https://github.com/kaist-plrg/fhetest.git heprogtest-artifact
cd heprogtest-artifact
git rev-parse HEAD
git submodule update --init src/main/java/T2-FHE-Compiler-and-Benchmarks
```

Use a Git checkout to obtain the T2 submodule. Run experiments sequentially
within each checkout because backend build files are shared.

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

The scripts verify source revisions and library versions, test context creation,
and build T2 and fhetest. `JOBS=2` limits library build parallelism. Build logs
are saved under `$artifact_deps/logs` and `$artifact_runs/build`;
see [VERSIONS.md](VERSIONS.md) for build options.
In a new shell, restore the sbt PATH and source the installation's `env.sh`.

## 4. Check basic execution

```sh
bash evaluation/artifact/run_checks.sh "$artifact_runs/core" core
```

Expect RQ1 output `209 2936 12467`, 15 passing RQ4 tests and exit zero.
Logs, environment information and `status.tsv` are bundled in `core.tar.gz`.
Each stage has a 20-minute limit plus 30 seconds before forced termination;
override with `CHECK_TIMEOUT=40m` if needed.

## 5. Select the RQ-specific procedure

| Task | Procedure |
| --- | --- |
| Seeded generation | `bash evaluation/artifact/run_checks.sh "$artifact_runs/seed" seed`; checks six modes using fresh JVMs |
| RQ1–RQ5 | Follow the [RQ-specific guides](README.md) |

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

Aggregation requires Python 3.10+; uv installs the pinned Pydantic dependency.
`pipeline` checks fixed BFV/CKKS inputs, JSON recording and aggregation.
To repeat the build and pipeline check in a separate checkout, use
[`run_smoke.sh`](README.md). Its check stages have a 15-minute limit plus a
30-second kill grace; build time is separate.
Randomized smoke/full experiments are described in [RQ2.md](RQ2.md).
