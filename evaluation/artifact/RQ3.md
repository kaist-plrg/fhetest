# RQ3: reduction in manual exception inspection

RQ3 uses the guided invalid-input results from RQ2.
Follow [RQ2_RQ3.md](RQ2_RQ3.md) to run and aggregate them.

## Read the results

In `aggregated/rq2_rq3_invalid_summary.csv`, select `kind=invalid` for each
`encType`. `exceptions` is the number of stored exception records, `expected`
is the number classified as expected, and `unexpected` is the remaining number.
The automatically screened proportion is `expected / exceptions`; it is
undefined if no exceptions were collected. `unique_messages` is a different
measure used by RQ2 and must not be used as this denominator.

The paper reports 198/198 for integer inputs and 744/760 for CKKS, leaving
16 CKKS records for inspection.

The classifier is in
[Check.scala](../../src/main/scala/fhetest/Phase/Check.scala), in
`classifyInvalidResults`. Library exceptions are compared with the keywords
associated with the violated filters. Other result categories, including native
errors and disabled-context screening, are recorded separately.

## Inspect the remaining records

The guided run's `exception/unexpected/` directory contains the JSON inputs and
exception results requiring inspection. The `output-rq2-1-b-filterOn-*.csv`
exports provide program identifiers and messages; keep the JSON files alongside
these exports to retain each program and its configuration.

For each candidate, check the relevant library version's documented behavior,
the generated code and parameters, and any developer response. Record the
reason for accepting or dismissing it. The keyword classifier selects candidates
for this manual assessment.
