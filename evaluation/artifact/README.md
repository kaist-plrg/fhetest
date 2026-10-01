# HEProgTest reproducibility artifact

Start with [UBUNTU.md](UBUNTU.md) to install the pinned libraries, extended T2
compiler and HEProgTest on Ubuntu. Run commands from the repository root.

| Task | Instructions |
| --- | --- |
| Build and basic checks | [UBUNTU.md](UBUNTU.md) |
| Source versions and build options | [VERSIONS.md](VERSIONS.md) |
| Generation domains, distributions and seed behavior | [GENERATION.md](GENERATION.md) |
| RQ1 example through the interpreter and both libraries | [RQ1.md](RQ1.md) |
| RQ2/RQ3 execution and aggregation | [RQ2_RQ3.md](RQ2_RQ3.md) |
| RQ3 exception classification and manual inspection | [RQ3.md](RQ3.md) |
| RQ4 interpreter tests and T2 implementation | [RQ4.md](RQ4.md) |
| RQ5 bug reports and available reproduction material | [RQ5.md](RQ5.md), [report index](RQ5_REPORTS.csv) |
| Included evaluation data | [DATA.md](DATA.md) |

For a small installation check after installing the prerequisites, run:

```sh
bash evaluation/artifact/run_smoke.sh
```

This builds the committed revision in a separate checkout, runs small fixed
inputs through OpenFHE and the result writer/aggregator, and prints a
`results.tar.gz` path. For the randomized experiments, follow [RQ2_RQ3.md](RQ2_RQ3.md).
Uncommitted edits are not tested by this command.

The artifact includes the [extended T2 compiler](../../src/main/java/T2-FHE-Compiler-and-Benchmarks)
as a pinned submodule and the [T2DSL interpreter](../../src/main/scala/fhetest/Phase/Interp.scala)
in this repository. See [RQ4.md](RQ4.md) for their roles and tests.

Use the immutable revision linked from the paper for a fixed artifact snapshot.
