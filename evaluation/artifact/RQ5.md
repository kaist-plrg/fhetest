# RQ5 report and reproduction sources

[RQ5_REPORTS.csv](RQ5_REPORTS.csv) maps the 18 Table 5 entries to public
reproduction material inspected on 2026-09-30. It is a source inventory, not an
executable reproduction suite. No historical-library reproduction was run in this
mapping task. The repository's examples have not been established as the original
inputs for these reports.

## Available material

| Material in the linked source | Table 5 IDs | Count |
| --- | --- | ---: |
| Code with `main` and headers; compilation not checked | 1–6 | 6 |
| Code requiring assembly, headers, or helper dependencies | 7–14, 16, 18 | 10 |
| Parameter description without a standalone test | 15 | 1 |
| Library source comparison rather than an executable test | 17 | 1 |

The code remains at the linked public sources; it is not bundled locally.
In particular, locating code does not recover the original FHEtest JSON, build
command, execution environment, or output log. Empty `input_path` fields mean
that no local reproduction file has been mapped.

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
  and 1.1.3. Confirm the intended tested version with the authors.
- **#7 and #8:** later posts correct the CKKS examples. #7's linked post has
  two scaling variants; the Table 5 FIXEDAUTO case is the first. #8's fourth
  post removes the irrelevant plaintext-modulus setting from both examples.
- **#11:** the original program discards the return value of `Relinearize`.
  The follow-up concerns BFV handling of an unrelinearized ciphertext. Keep
  this invalid input when reconstructing that report; changing it to an
  in-place relinearization would remove the reported condition.
- **#12 and #13:** public replies explain parameter/noise requirements or
  discuss future validation. These replies alone do not establish the paper's
  `fixed` labels. Obtain the relevant fix reference or author records before
  claiming that a released version fixes these cases.
- **#14 and #15:** one report covers two entries. Its executable example
  belongs to #14; do not also count it as a reproducer for #15.
- **#16 and #18:** external headers or helper functions need to be resolved
  before treating the posted programs as standalone inputs.
- **#17:** this is a source-level condition/message inconsistency. Record the
  cited commit and inspected conditions rather than inventing a runtime crash.

## Completing a reproduction case

For each entry, obtain or assemble the corresponding source, preserve its
provenance, and document any changes needed to compile it. Record the affected
library commit, build options, compiler, command, expected symptom, and observed
output. Keep an author-supplied original input distinct from a newly assembled
example. Run each case with a timeout and retain failures as well as successes.

The artifact's default OpenFHE 1.4.2 / SEAL 4.1.2 environment does not reproduce
every historical version in this table. Do not change that default or rerun bug
discovery merely to fill this inventory. Historical-version execution is a
separate task; its scope can be chosen after the original materials are checked.
