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
output with the corresponding `.res` file.

## Run

Complete the build in [UBUNTU.md](UBUNTU.md), then use a new output directory:

```sh
bash evaluation/artifact/run_checks.sh "$HOME/rq4-check-$(date +%Y%m%d-%H%M%S)" rq4
```

The wrapper records the environment and runs `sbt 'testOnly BasicInterpTest'`.
Expect 15 passing tests and exit zero. Logs, the XML test report and `status.tsv`
are saved in the output directory and a `.tar.gz` bundle.

## Timing interpretation

The test suite prints a duration in milliseconds for each test. Its timed region
includes reading the expected output, parsing, interpreting and comparison;
JVM startup and sbt build time are not included. The suite uses ring dimension
32768 and plaintext modulus 65537.
