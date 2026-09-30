# Included evaluation data

[evaluation-202602-data](../evaluation-202602-data/) contains historical records.
New outputs from the artifact scripts are stored separately and do not replace
these files.

| Files | Contents |
| --- | --- |
| `rq2_valid_summary.csv` | Guided/random counts and success ratios for integer and CKKS inputs |
| `rq2_rq3_invalid_summary.csv` | Exception counts, distinct messages, expected/unexpected counts |
| `rq2_rq3_summary.json` | Combined summaries and original source paths |
| `rq2_rq3_run_full_202602.txt` | Run directories, configuration and recorded exit statuses |
| Eight `output-rq2-1-*.csv` files | Per-input failure/exception exports for each input kind and filter setting |

These files are not a complete raw-input archive or the per-repeat records for
the revised paper's three-baseline statistics. The stored CKKS valid summary
contains 6,183 guided inputs and 3,077 baseline inputs; it does not establish a
count-matched comparison. The manifest records exit 1 for both guided invalid
runs, so their completion cannot be assumed. The aggregator rejects these exits;
do not alter the manifest to bypass that check.

Historical server paths are references, not included files. Summary CSVs cannot
reconstruct missing programs, outputs or seeds. See [RQ2_RQ3.md](RQ2_RQ3.md) for
new execution and aggregation, [UBUNTU.md](UBUNTU.md) for the available RQ
procedures, and [RQ5.md](RQ5.md) for public bug-report sources.

## Rechecking archived CKKS inputs

`evaluation/recheck_rq2_valid_double.sh ARCHIVE_DIR REPORT_DIR` recompiles and
executes the archived programs with OpenFHE, then compares their new classifications
with the stored labels. It does not just reapply a formula to saved output values.
Runtime depends on the archive and machine. `EXPECTED_COUNT` defaults to 6,183
as an archive-size check, not evidence that those files are included or available.
Cryptographic randomness and other implementation/environment changes can affect
the comparison. `changed=0` describes this rerun; it does not isolate the denominator
change. To isolate that change, apply both formulas to the same output values.
