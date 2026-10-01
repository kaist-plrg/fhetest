# RQ5 report and reproduction sources

[RQ5_REPORTS.csv](RQ5_REPORTS.csv) maps the 18 Table 5 entries to public
reproduction material. The artifact includes one executable check for OpenFHE
1.4.2; the remaining material is available through the linked reports.

## Run a current-version check for Table 5 #2

[negative_depth.cpp](rq5/negative_depth.cpp) preserves the negative-depth context
construction from [OpenFHE issue #576](https://github.com/openfheorg/openfhe-development/issues/576).
The check returns zero when an exception rejects the parameter, one if it is
accepted, and fails on a native crash.

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
[PR #612](https://github.com/openfheorg/openfhe-development/pull/612) adds
parameter validation for this report.

## Other reports

In [RQ5_REPORTS.csv](RQ5_REPORTS.csv), `report_url` and `source_url` link each
finding to its report and reproduction material. `source_kind`, `source_locator`
and `notes` identify code, parameter descriptions and required dependencies.
`input_path` is relative to this directory; an empty field means the material
is at the linked source. `version`, `validity` and `paper_status` follow the paper;
for #2, the local check uses 1.4.2 while `version` records the affected version.
