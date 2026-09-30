# Generation rules and reproducibility

`gen` and `test` accept `-seed:<Long>` for Scala's generator. Repeating an input
sequence requires the same generator version, settings, environment and filter
order. The seed does not control HE encryption, execution time or the number of
inputs completed within a time limit, and cannot recover historical unrecorded
seeds. Filter order is reflection-derived and is saved by the seed check;
cross-platform ordering has not been established.

## Parameter proposals

Source: `src/main/scala/fhetest/Generate/LibConfigGenerator.scala`.

| Field | Domain |
| --- | --- |
| Scheme | int: BFV/BGV; double: CKKS |
| ringDim | 8192, 16384, 32768, 65536, 131072 |
| mulDepth | Integers -20 through 20 |
| plainMod | 65537 for ringDim <= 32768; otherwise 786433 |
| firstModSize, scalingModSize | Integers -100 through 100 |
| securityLevel | HEStd_128_classic, HEStd_192_classic, HEStd_256_classic, HEStd_NotSet |
| scalingTechnique | NORESCALE, FIXEDMANUAL, FIXEDAUTO, FLEXIBLEAUTO, FLEXIBLEAUTOEXT |
| Vector length cap | Integers 1 through 100000 |
| Value bound | int: integers 1 through 1000; double: [1, 2^64) |
| Rotation bound | Integers 0 through 40 |

List entries have equal proposal probability. Integer domains include both
endpoints; double domains exclude the upper endpoint. Valid generation applies
all filters; invalid generation negates nonempty subsets of filters and skips
empty domains. Baseline generation bypasses filters. These constraints change
the distribution; final programs are not uniformly sampled from program space.

## Program proposals

Sources: `AbsProgramGenerator.scala`, `AbsStatement.scala`, `AbsProgram.scala`.

| Component | Selection rule |
| --- | --- |
| Random template length | Uniform integer, 1–20 operations |
| Operation | Uniform over Add/Sub/Mul × encrypted/plaintext/constant, Rot, Relin: 11 templates, each 1/11 before rejection/transformations |
| Operand vector length | Uniform integer, 1 through the selected length cap |
| Operand values | Integer or double values in [0, selected value bound); double literals printed with six decimal places |
| Rotation amount | Inclusive [0, rotateBound] when rotateBound < 21, otherwise [21, rotateBound] |
| Depth | Counts Mul/MulP/MulC templates; the valid depth filter requires mulDepth to exceed that count |

Assignments, rescaling and eligible relinearization are inserted afterward, so
20 does not bound the final statement count. Exhaustive mode enumerates increasing
template lengths without this bound; RQ2/RQ3 uses Random. Parsing and interpreter
output-bound checks can skip programs, so generated and recorded counts differ.

## Timeouts and classification

`-timeout:<seconds>` is unset by default. It limits waiting on a Future, not the
underlying native process. The evaluation script uses an outer timeout of 60s
in smoke mode or 24h for full-mode guided runs; full-mode baselines are
count-limited. See [RQ2_RQ3.md](RQ2_RQ3.md) for the recorded overrides.

Native exits 139/136 receive segmentation-fault/floating-point-exception messages.
Other failures follow stderr handling. `Check.execute` maps the termination
prefix to LibraryError, numeric output to Normal, other text to LibraryException,
waiting timeouts to TimeoutError, and other caught failures to PrintError.
Invalid-input screening uses filter keywords and special cases; remaining
candidates require manual assessment. This is not an exhaustive portable crash
classifier. See `Phase/Execute.scala`, `Phase/Check.scala`, and `Utils/Utils.scala`.

## Seed check

After building, run `bash evaluation/artifact/check_seed.sh NEW_OUTPUT_DIR`.
For int/double × valid/invalid/baseline, it launches separate JVMs with seeds A,
A and B, hashing program text, configuration and invalid-filter indices and
recording filter order. Defaults: COUNT=3, SEED=20260929, OTHER_SEED=20260930.
A/A snapshots must match; A/B differences are a sanity check for these seeds,
not a mathematical guarantee for every finite sample. This checks generation,
not HE execution or end-to-end `test` classifications.
