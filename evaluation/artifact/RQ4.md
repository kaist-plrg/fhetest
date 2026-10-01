# RQ4: T2DSL interpreter

RQ4 checks the interpreter's outputs against saved reference outputs for the 15
programs in the paper's interpreter-evaluation table. Eleven are adopted from
the T2 test suite and four cover extended expressions: `const_ops2`,
`const_ops2_fp`, `mult_relin` and `mult_relin_fp`.

## Implementations and inputs

- [Extended T2 compiler](../../src/main/java/T2-FHE-Compiler-and-Benchmarks): the
  pinned Git submodule provides the parser and HE backend code generators.
  `build_project.sh` builds it and copies its JAR into `lib/`.
- [T2DSL interpreter](../../src/main/scala/fhetest/Phase/Interp.scala): Scala code
  in HEProgTest that computes reference outputs without HE encryption.
- [Test programs](../../src/main/resources/basic_test/t2) and
  [expected outputs](../../src/main/resources/basic_test/result): matching `.t2`
  and `.res` filenames identify each case.
- [BasicInterpTest](../../src/test/scala/BackendTest.scala): the executable suite.

The suite parses each program with T2, runs the interpreter and compares its
output with the corresponding `.res` file. This is not an execution of those
programs through OpenFHE or SEAL.

## Run

Complete the build in [UBUNTU.md](UBUNTU.md), then use a new output directory:

```sh
bash evaluation/artifact/run_checks.sh "$HOME/rq4-check-$(date +%Y%m%d-%H%M%S)" rq4
```

The wrapper records the environment and runs `sbt 'testOnly BasicInterpTest'`.
Expect 15 tests succeeded, zero failed, and exit code zero. The output directory
contains `rq4-interpreter.log`, `rq4-test-report.xml`, command records and
`status.tsv`; the wrapper also creates a `.tar.gz` bundle.

## Timing interpretation

The test suite prints a duration in milliseconds for each test. Its timed region
includes reading the expected output, parsing, interpreting and comparison;
JVM startup and sbt build time are not included. The current suite uses ring
dimension 32768 and plaintext modulus 65537. Record these settings and your
machine when reporting timings. Passing these tests checks output consistency;
new timings need not match the paper's table. The paper's batching discussion
mentions a default ring dimension of 4096, so this suite's timings alone should
not be presented as a reproduction of that historical configuration.
