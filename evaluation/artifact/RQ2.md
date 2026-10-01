# RQ2: generation effectiveness

Complete [UBUNTU.md](UBUNTU.md) first. Run experiments sequentially per checkout.

## Run and aggregate

```sh
rq2_out="$HOME/rq2-$(date +%Y%m%d-%H%M%S)"
EVAL_OUTDIR="$rq2_out" RUN_SEED=20260929 BASELINE_REPEATS=3 \
  bash evaluation/run_rq2_rq3.sh full
uv run evaluation/aggregate_rq2_rq3.py --summary "$rq2_out"/rq2_rq3_run_full_*.txt
```

Guided runs each have a 24-hour limit. Baselines repeat three times by default:

- Valid: match the guided **recorded-result count**, excluding discarded inputs
  such as overflow cases.
- Invalid: match the guided **exception-record count**, using chunks of 200
  candidates, up to 1,000 chunks.

Full baselines are count-limited and can take longer than guided runs. Unmet
targets fail. Override `DURATION`, `BASELINE_REPEATS`, `INVALID_RANDOM_CHUNK`
and `INVALID_RANDOM_MAX_ITERS` as needed; all settings are recorded.
Each subprocess uses the base seed plus a sequential counter and a separate
result directory. Timing and cryptographic randomness can change which inputs
complete even with the same generation seed.

## Records and outputs

Each invocation creates `evaluation/evaluation-TIMESTAMP-PID`. `EVAL_OUTDIR`
can specify another new directory with an existing parent. Subprocess commands,
logs and JSON directories are saved separately. The manifest records settings,
seeds, timestamps, exits and source revision. Exit 124 is the planned time limit;
other nonzero exits stop the run. Generated candidates and recorded JSON results
are counted separately.

Aggregation validates inputs before writing to `aggregated/` beside the manifest:

- `rq2_valid_summary.csv`: generated counts, recorded counts (`total`), and
  unrecorded counts (discarded or interrupted). `succ_rate` and
  `recorded_succ_rate` are successful records / recorded results, as ratios (0–1).
- `rq2_rq3_invalid_summary.csv`: exception records, distinct OpenFHE messages,
  and expected/unexpected record counts, also used for RQ3 inspection workload.
- `baseline_statistics.csv`: mean and **sample** standard deviation (`n-1`) across
  repeats; success-rate statistics use percent and percentage points. One repeat
  has no sample standard deviation (empty CSV cell / JSON null).
- `rq2_rq3_summary.json`: the above tables, manifest path, mode and count basis.
- `output-rq2-1-*.csv`: per-input failures/exceptions; baseline filenames include
  `repeat1`, `repeat2`, etc.

Invalid baselines select the first target-count exception records after sorting
JSON paths lexicographically across chunks (`10.json` precedes `2.json`).
Preserve these paths when reaggregating.
Reaggregation requires the JSON directories referenced by the run manifest.

## Optional short run

For an installation check, use [run_smoke.sh](README.md). To run randomized
generation with shorter limits instead:

```sh
rq2_out="$HOME/rq2-smoke-$(date +%Y%m%d-%H%M%S)"
EVAL_OUTDIR="$rq2_out" RUN_SEED=20260929 \
  bash evaluation/run_rq2_rq3.sh smoke
uv run evaluation/aggregate_rq2_rq3.py \
  --summary "$rq2_out"/rq2_rq3_run_smoke_*.txt --allow-partial
```

Each subprocess is limited to 60 seconds plus a 30-second kill grace;
override with `DURATION`. Invalid baselines attempt one two-input chunk per
repeat. `--allow-partial` exports unmet targets as `complete=false` and omits
that group's statistics. Missing/corrupt records and abnormal exits remain errors.
Large vectors and overflow exclusions may leave no recorded results, stopping
the guided run. `GUIDED_COUNT` optionally caps smoke guided candidates.
