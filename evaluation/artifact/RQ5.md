# RQ5 report and reproduction sources

[RQ5_REPORTS.csv](RQ5_REPORTS.csv) maps the 18 Table 5 entries to public
reproduction material. The artifact includes one executable check for OpenFHE
1.4.2; the remaining material is available through the linked reports.

## Run a current-version check for Table 5 #2

[negative_depth.cpp](rq5/negative_depth.cpp) preserves the negative-depth context
construction from [OpenFHE issue #576](https://github.com/openfheorg/openfhe-development/issues/576).
It adds exception handling and an exit status: rejection by an exception returns
zero, accepting the parameter returns one, and a native crash remains a failure.
This checks the response to an invalid parameter, not the wording of its message.

After the build in [UBUNTU.md](UBUNTU.md), run from the repository root with
`OpenFHE_DIR` set by the installation's `env.sh`:

```sh
rq5_build="$HOME/rq5-negative-depth-$(date +%Y%m%d-%H%M%S)"
cmake -S evaluation/artifact/rq5 -B "$rq5_build" -DOpenFHE_DIR="$OpenFHE_DIR"
cmake --build "$rq5_build" --parallel 2
timeout --kill-after=30s 60s "$rq5_build/negative_depth"
```

Expect `Negative depth rejected:` followed by the library exception and exit
zero on OpenFHE 1.4.2. The original report concerns a crash on OpenFHE 1.0.4.
[PR #612](https://github.com/openfheorg/openfhe-development/pull/612) links the
report to parameter validation; its merge commit is
`62044f6c0f8fcecd62f9b8f5b5531da29b017f52`.

## Available material

| Material in the linked source | Table 5 IDs | Count |
| --- | --- | ---: |
| Code with `main` and headers; compilation not checked | 1–6 | 6 |
| Code requiring assembly, headers, or helper dependencies | 7–14, 16, 18 | 10 |
| Parameter description without a standalone test | 15 | 1 |
| Library source comparison rather than an executable test | 17 | 1 |

Apart from the adapted #2 check above, code remains at the linked public sources.
Empty `input_path` fields indicate that no local input file is included.

## Reading the manifest

- `version`, `validity`, and `paper_status` retain the paper's entries. They are
  not independently verified release or bug-status claims.
- `report_url` is the paper reference. `source_url` locates the relevant code or
  description; for #3 this is the GitHub issue linked by the forum report.
- `input_path` is relative to this directory. For #2 it points to the adapted
  OpenFHE 1.4.2 check above; `version` retains the paper's affected version.
- `source_kind`, `source_locator`, and `notes` distinguish code from descriptions
  and identify variants, missing dependencies, and discrepancies.
