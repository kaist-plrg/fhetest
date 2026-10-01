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

The stored CKKS valid summary contains 6,183 guided inputs and 3,077 baseline
inputs. The revised paper's complete three-repeat records and raw JSON archive
are not included. The manifest records exit 1 for both guided invalid runs;
the aggregator rejects these abnormal exits.

See [RQ2_RQ3.md](RQ2_RQ3.md) to generate and aggregate results. Reaggregating
stored manifests requires access to their referenced JSON directories.
