# Running and aggregating RQ2/RQ3

Use the artifact's pinned library/project setup first. Run one experiment at a
time per checkout: generated backend build files are shared within that checkout.
The runner requires Bash and GNU coreutils (`timeout` on Ubuntu, `gtimeout` on
macOS). Aggregation requires Python 3.10+ and `uv`; its inline metadata pins
Pydantic. Neither command overwrites the historical `evaluation-202602-data`.

## Short execution check

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
no completed valid inputs or no exceptions cannot support a comparison and stops.
`GUIDED_COUNT=2` can additionally cap smoke guided inputs; it does not guarantee
two exception records, nor does it guarantee a complete smoke comparison.

## New experiment, only when needed

```sh
RUN_SEED=20260929 BASELINE_REPEATS=3 bash evaluation/run_rq2_rq3.sh full
uv run evaluation/aggregate_rq2_rq3.py --summary /path/printed/by/runner.txt
```

Full mode timeboxes each guided run to 24 hours. Valid baselines request the
number of completed guided JSON records. Invalid baselines collect chunks of
200 inputs until they reach the guided exception-record count (up to 1,000
chunks). Baselines in full mode are count-limited, **not time-limited** and may
run substantially longer than guided runs. Full mode fails if a baseline does
not reach the target. `DURATION`, `INVALID_RANDOM_CHUNK`, and
`INVALID_RANDOM_MAX_ITERS` override the defaults and are recorded.

Three baseline repeats are the default. Each subprocess receives a distinct seed
(base seed plus a sequential counter), and each repeat has separate result paths.
This makes generation reproducible; wall-clock cutoffs, cryptographic randomness,
and machine performance can still change the completed prefix and timings.
Seeds do not guarantee bit-identical ciphertexts or identical 24-hour counts.

## Records and outputs

Every invocation creates a fresh `evaluation/evaluation-TIMESTAMP-PID` directory;
`EVAL_OUTDIR` can name another **nonexistent** directory with an existing parent.
Each subprocess has `.log` and `.command.txt` files and a dedicated JSON directory
selected by `FHETEST_RUN_DIR`. The manifest records seeds, timestamps, exits,
configuration and repository commit/dirty status. Exit 124 denotes the planned
time limit; other nonzero exits stop execution. Successful orchestration ends
with `RUN_FINISHED=1`.

Aggregation validates inputs before writing to `aggregated/` beside the manifest:

- `rq2_valid_summary.csv`: guided/repeat counts and success ratios (0–1).
- `rq2_rq3_invalid_summary.csv`: exception records, distinct OpenFHE messages,
  and expected/unexpected record counts, also used for RQ3 inspection workload.
- `baseline_statistics.csv`: mean and **sample** standard deviation (`n-1`) across
  repeats; success-rate statistics use percent and percentage points. One repeat
  has no sample standard deviation (empty CSV cell / JSON null).
- `rq2_rq3_summary.json`: the above tables, manifest path and smoke/full label.
- `output-rq2-1-*.csv`: per-input failures/exceptions; baseline filenames include
  `repeat1`, `repeat2`, etc. Historical single-baseline manifests retain the old
  filenames.

Invalid baseline truncation sorts JSON paths lexicographically across all chunks
and takes the first target-count records, preserving the selection rule in the
pre-artifact aggregator (`03175b3`). This is path order, not numeric `programId`
or execution order: for example, `10.json` precedes `2.json`. Preserve original
relative paths when reaggregating. Changing the selected subset can change the
distinct-message count. The count is of stored exception records, not a claim
that each record is a different test program. The final paper's three-repeat
records must still be checked against the aggregation procedure actually used.

Historical manifest paths must actually exist before reaggregation. The tool
cannot recreate original JSON from summary CSVs. It also rejects recorded
abnormal exits. Obtain the original three-repeat records before comparing with
the revised paper; new seeded smoke/full results are not historical evidence.
Ubuntu execution and the paper's historical statistics still require separate
confirmation. This change does not alter the HE filters or library versions.

## Script regression checks

```sh
PYTHONPATH=evaluation uv run --with pytest --with pydantic==2.12.5 pytest -q evaluation/tests
shellcheck evaluation/run_rq2_rq3.sh
```

The runner tests use a controlled executable to check smoke/full orchestration,
unique seeds, aggregation and error propagation. They are not native HE runs.
