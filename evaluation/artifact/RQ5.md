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

After the build in [UBUNTU.md](UBUNTU.md), run:

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
`62044f6c0f8fcecd62f9b8f5b5531da29b017f52`. The source index retains
`reproduction_status=not_run` for historical execution.

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
- `source_kind`, `source_locator`, and `notes` distinguish code from descriptions
  and identify variants, missing dependencies, and discrepancies.
- `source_sha256` fingerprints the selected source post, not executable input:
  UTF-8 bytes of the decoded GitHub API issue `body`, or Discourse API post
  `cooked` field. It does not include later replies. Public edits can change it.
- `checked_on` records source inspection. `reproduction_status=not_run` means
  that this artifact mapping provides no execution evidence for the entry.

For Discourse, retrieve `/t/<topic-id>.json` and select the indicated
`post_number` from `post_stream.posts`. For GitHub, retrieve
`https://api.github.com/repos/<owner>/<repository>/issues/<number>`.
Read the replies as well as the selected code before preparing a runnable case.

## Cases needing particular care

- **#4:** the paper lists OpenFHE 1.1.4, while the code report describes 1.0.4
  and 1.1.3.
- **#7 and #8:** later posts correct the CKKS examples. #7's linked post has
  two scaling variants; the Table 5 FIXEDAUTO case is the first. #8's fourth
  post removes the irrelevant plaintext-modulus setting from both examples.
- **#11:** the original program discards the return value of `Relinearize`.
  The follow-up concerns BFV handling of an unrelinearized ciphertext. Keep
  this invalid input when reconstructing that report; changing it to an
  in-place relinearization would remove the reported condition.
- **#12 and #13:** public replies explain parameter/noise requirements or
  discuss future validation. These replies alone do not establish the paper's
  `fixed` labels; fix references are not included in the manifest.
- **#14 and #15:** one report covers two entries. Its executable example
  belongs to #14; do not also count it as a reproducer for #15.
- **#16 and #18:** external headers or helper functions need to be resolved
  before treating the posted programs as standalone inputs.
- **#17:** this is a source-level condition/message inconsistency. Record the
  cited commit and inspected conditions.
