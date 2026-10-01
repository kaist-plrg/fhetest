# Running and aggregating RQ2/RQ3

Use the artifact's pinned library/project setup first. Run one experiment at a
time per checkout: generated backend build files are shared within that checkout.
The runner requires Bash and GNU coreutils (`timeout` on Ubuntu, `gtimeout` on
macOS). Aggregation requires Python 3.10+ and `uv`; its inline metadata pins
Pydantic.

## Fixed-input installation check

After building, run:

```sh
bash evaluation/artifact/run_checks.sh "$HOME/he-pipeline-$(date +%Y%m%d-%H%M%S)" pipeline
```

This executes the four small fixtures defined in `FixtureCheck.scala`: BFV and
CKKS addition with valid parameters, and each with an invalid scaling modulus
size of zero. The ring dimension is 8192 and the vector length is four. Valid
cases must agree with the interpreter; invalid cases must produce native
modulus-parameter exceptions. It uses the normal checker and JSON writer.

Each fixture is executed in both groups expected by the RQ2/RQ3 aggregator.
The `guided`/`baseline` labels exercise its input layout.
The manifest and summary identify `MODE=fixture`. Aggregation must succeed
without `--allow-partial`.
Use `seed` separately to check reproducible generation.

## Randomized short execution check

```sh
source /path/to/artifact-install/env.sh
RUN_SEED=20260929 bash evaluation/run_rq2_rq3.sh smoke
uv run evaluation/aggregate_rq2_rq3.py --summary /path/printed/by/runner.txt --allow-partial
```

The smoke mode limits **every subprocess** to 60 seconds (plus a 30-second kill
grace). Invalid baselines attempt one chunk of two inputs per repeat by default,
so they may not reach the guided exception count. `--allow-partial` exports these
partial rows with `complete=false`, and omits statistics for any baseline group
with an incomplete repeat. Missing directories, corrupt JSON, unfinished manifests,
and abnormal process exits remain errors, even with this flag. A guided run with
no recorded valid results or no invalid exception records stops. A valid run
can generate candidates but record no results because checking discards them or
the time limit interrupts execution. Inspect both counts and the subprocess log.
`GUIDED_COUNT=2` can additionally cap smoke guided inputs; it does not guarantee
two exception records, nor does it guarantee a complete smoke comparison.

Large generated vectors can spend minutes in the interpreter before a library
is invoked: its element-wise operations use indexed access and appends on linked
lists. Increase `DURATION` for longer randomized checks, or use the fixed-input
installation check above.

## Run the RQ2/RQ3 experiments

```sh
RUN_SEED=20260929 BASELINE_REPEATS=3 bash evaluation/run_rq2_rq3.sh full
uv run evaluation/aggregate_rq2_rq3.py --summary /path/printed/by/runner.txt
```

Full mode timeboxes each guided run to 24 hours. Valid baselines request the
number of recorded guided results. Inputs discarded before library execution,
including overflow exclusions, do not contribute to this count or the success-rate
denominator. The `test -resultcount:N -json:true` option stops after N checked
valid-program results; `-count:N` still caps generated candidates. The two options
cannot be combined. Invalid baselines collect chunks of
200 inputs until they reach the guided exception-record count (up to 1,000
chunks). Baselines in full mode are count-limited, **not time-limited** and may
run substantially longer than guided runs. Full mode fails if a baseline does
not reach the recorded-result/exception target. `DURATION`, `INVALID_RANDOM_CHUNK`, and
`INVALID_RANDOM_MAX_ITERS` override the defaults and are recorded.

Three baseline repeats are the default. Each subprocess receives a distinct seed
(base seed plus a sequential counter), and each repeat has separate result paths.
Wall-clock cutoffs, cryptographic randomness and machine performance can change
the completed prefix and timings even with a fixed generation seed.

## Records and outputs

Every invocation creates a fresh `evaluation/evaluation-TIMESTAMP-PID` directory;
`EVAL_OUTDIR` can name another **nonexistent** directory with an existing parent.
Each subprocess has `.log` and `.command.txt` files and a dedicated JSON directory
selected by `FHETEST_RUN_DIR`. The manifest records seeds, timestamps, exits,
configuration and repository commit/dirty status. Exit 124 denotes the planned
time limit; other nonzero exits stop execution. Successful orchestration ends
with `RUN_FINISHED=1`.

`VALID_COUNT_BASIS=recorded` identifies new randomized runs. `FHETEST_GENERATED=N`
progress lines count candidates after generation and before checking. The manifest
stores these counts separately from JSON counts. On a timeboxed guided run, the
last generated candidate may still be executing when the process is stopped.

Aggregation validates inputs before writing to `aggregated/` beside the manifest:

- `rq2_valid_summary.csv`: generated counts, recorded counts (`total`), and
  unrecorded counts. `unrecorded` includes discarded candidates and any candidate
  interrupted by the time limit; it is not an exact skip count. For new runs,
  `succ_rate` divides successful records by recorded results, as does
  `recorded_succ_rate`. Both are ratios (0–1); generated counts are diagnostic.
- `rq2_rq3_invalid_summary.csv`: exception records, distinct OpenFHE messages,
  and expected/unexpected record counts, also used for RQ3 inspection workload.
- `baseline_statistics.csv`: mean and **sample** standard deviation (`n-1`) across
  repeats; success-rate statistics use percent and percentage points. One repeat
  has no sample standard deviation (empty CSV cell / JSON null).
- `rq2_rq3_summary.json`: the above tables, manifest path, mode and count basis.
- `output-rq2-1-*.csv`: per-input failures/exceptions; baseline filenames include
  `repeat1`, `repeat2`, etc. Historical single-baseline manifests retain the old
  filenames.

Invalid baseline truncation sorts JSON paths lexicographically across all chunks
and takes the first target-count records, preserving the selection rule in the
pre-artifact aggregator (`03175b3`). This is path order, not numeric `programId`
or execution order: for example, `10.json` precedes `2.json`. Preserve original
relative paths when reaggregating. Changing the selected subset can change the
distinct-message count. Counts refer to stored exception records.

Manifests without `VALID_COUNT_BASIS` also use recorded results.
Other count bases are rejected.
Reaggregation requires the JSON directories referenced by the manifest.
See [DATA.md](DATA.md) for the included evaluation records.

## Script regression checks

```sh
PYTHONPATH=evaluation uv run --with pytest --with pydantic==2.12.5 pytest -q evaluation/tests
shellcheck evaluation/run_rq2_rq3.sh
```

The runner tests use a controlled executable to check smoke/full orchestration,
unique seeds, aggregation and error propagation.
