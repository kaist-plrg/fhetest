# Generation rules and reproducibility

`gen` and `test` accept `-seed:<Long>`. The same generator version, settings,
environment and filter order reproduce the input sequence. HE encryption and
time-limited result counts remain variable. The seed check records the
reflection-derived filter order; cross-platform ordering is not guaranteed.

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
20 does not bound the final statement count. RQ2/RQ3 uses Random generation.
Parsing and interpreter output-bound checks can skip candidates before recording.

## Timeouts and classification

`-timeout:<seconds>` is unset by default and limits Future waiting, without
terminating the native process. Evaluation limits are in [RQ2.md](RQ2.md).

Native exits 139/136 receive segmentation-fault/floating-point-exception messages.
Other failures follow stderr handling. `Check.execute` maps the termination
prefix to LibraryError, numeric output to Normal, other text to LibraryException,
waiting timeouts to TimeoutError, and other caught failures to PrintError.
Invalid-input screening uses filter keywords and special cases; remaining
candidates require manual assessment. See `Phase/Execute.scala`,
`Phase/Check.scala`, and `Utils/Utils.scala`.

## Seed check

After building, run `bash evaluation/artifact/check_seed.sh NEW_OUTPUT_DIR`.
It compares fresh-JVM snapshots for int/double × valid/invalid/baseline:
two runs with the same seed must match; the selected different seed must change
the snapshot. Snapshots include program/configuration hashes and filter order.
Defaults: COUNT=3, SEED=20260929, OTHER_SEED=20260930.
