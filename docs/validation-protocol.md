# Held-out validation protocol for `attribute_truth_fusion()` (Phase 4 design)

Status: **specification only — no held-out reference data has been identified or built yet.**
This document defines the schema, cohort rules, and benchmark protocol Phase 4 will use once
real reference data exists. Nothing in this file should be read as a result, and no Phase 4
empirical run should begin until the "Data source status" section below is resolved.

## 0. Data source status (read this first)

A search of the isochrones/midwifery research ecosystem (`~/isochrones`, `~/midwifery` and its
seven `-bon`/`-provider-directory` worktrees, `~/mysterymedicaid`) for an independently validated
(phone-audited or board-verified) multi-year physician-location panel found **none**. What exists:

- `~/midwifery/artifacts/amcb_npi_linkage_panel-midwifery-plus-nursing_years-{through-2017,
  2007-2026}.csv` — two vintages of the same AMCB→NPI name-matching crosswalk (22,358 rows each;
  `npi`, `nppes_city`, `nppes_state`, `nppes_zip`, `nppes_practice_address`,
  `nppes_location_year`, plus match-confidence fields). Real, dated, on disk — but a
  name-matched administrative crosswalk, not a phone-verified reference standard. Comparing the
  two vintages on 10,508 shared NPIs shows ~10.5% differ in city/state, a genuine drift signal,
  but drift between two uncertain sources is not evidence either one is correct.
- `~/isochrones`, path `audit-records/geocoding/practice-location-reconstruction-2026-10-31/
  e1_last_update_staleness.R` (git history, commit `48dff9a27`) — the original of this package's
  own `record_age_years()`/`record_age_band()`/`classify_record_staleness()`. Its source parquet
  panels (`nppes_observations.parquet`, `dac_sites.parquet`, `d1_location_evidence_ledger.parquet`,
  `d4_disagreement_ledger.parquet`) **do not exist on disk**; only aggregate derived CSVs survive,
  also only in git history, not at `HEAD`.
- `~/isochrones/tests/test_regression_and_gold_standard.R` contains a `GOLD_STANDARD_NPIS` table
  explicitly commented `# (EXAMPLE - replace with real data)`, with placeholder NPIs
  (`"1234567890"`, ...). **Not usable as data under any circumstance** — it was never real.
- `~/isochrones/validation_results/reconciliation_report.md` cross-references ABOG roster against
  NPPES (49.2% overall match) — real, but two uncertain administrative sources compared against
  each other, which is exactly the kind of comparison this validation protocol is designed to
  test, not a source of ground truth for it.

**Decision (explicit, from the project owner):** proceed with this specification document only.
Hold Phase 4 (and any benchmark accuracy claim) until a genuinely independent reference standard
is identified — most plausibly a completed telephone-audit or board-verification call log from one
of this package's own validity studies (the ENT or OB/GYN registry-validity work this package was
originally built for; see README's "Protocol" table), since that is the one category of data this
ecosystem has that is actually verified against ground truth rather than against another
administrative source. Re-run the data-source search against that possibility before Phase 4
starts.

## 1. Two schemas, kept structurally separate

`attribute_claims_validate()`'s schema (`entity_id`, `attribute`, `source_id`, `claimed_value`,
`reference_year`, ...) is deliberately reused for evidence claims — every administrative source's
assertion about a physician's location in a given year is one row, exactly as it is today. The
reference standard is a **second, structurally distinct** table, never unioned into the claims
table and never passed to `attribute_claims_validate()`/`attribute_truth_fusion()` as a "source":

| column | type | meaning |
|---|---|---|
| `entity_id` | character | physician identifier (NPI), joinable to `claims$entity_id` |
| `reference_year` | integer | the year this reference location is asserted to hold |
| `reference_city` | character | validated practice city |
| `reference_state` | character | validated practice state |
| `verification_method` | character | e.g. `"phone_audit"`, `"board_verification"` — never
  `"administrative_crosswalk"`; a reference row whose only support is another unverified source
  does not belong in this table at all |
| `verification_date` | date | when the verification actually happened (not the claim's own
  `reference_year` — a 2026 phone call can validate a 2024 practice-year, and the gap matters for
  the temporal-holdout design in §6) |
| `verified_by` | character | study identifier, for provenance and for excluding a study's own
  phone-audit output from being used both to validate itself and to train source reliability on |

One row per `(entity_id, reference_year)`. Unlike `claimed_value`, `reference_city`/
`reference_state` are never missing within this table by construction — a physician-year with no
verified location simply has no row here, and is excluded from the benchmark for that year (never
imputed, never backfilled from a claim).

## 2. Why the separation is load-bearing, not cosmetic

Every leakage risk in this protocol reduces to one rule: **nothing in the reference table may
flow into inference, source-reliability estimation, or parameter selection.** Concretely:

- `attribute_truth_fusion()` is run on the claims table alone, exactly as it is today, for both
  the training and test partitions. The reference table is consulted only afterward, to score
  predictions already made.
- Dawid-Skene/TruthFinder-family per-source reliability (`source_diagnostics`) must be estimated
  from the **training partition's claims only**, never re-estimated (or peeked at) using test-set
  claims — this is why the partition in §6 is by whole physician, not by physician-year: fitting
  Dawid-Skene jointly across a source's claims for both a training physician and a test physician
  would let the test physician's claims influence that source's estimated reliability, contaminating
  every prediction that reliability estimate touches.
- Any threshold tuned on outcomes (`abstain_threshold`, `max_iterations`, `convergence_tolerance`)
  is selected using training-partition accuracy only; the test partition is scored exactly once,
  at the end, with parameters already fixed.

## 3–5. Cohorts

All three cohorts are defined on **physician-years with a reference-table row** (§1) — a
physician-year the inference methods will be scored on must be independently verified, full stop.
Within that eligible set:

- **Stable-physician cohort.** A physician whose reference-table rows across all of their
  eligible years agree on `(reference_city, reference_state)`. Tests whether a method correctly
  holds a location steady when nothing actually changed — the easy case, and a sanity floor: a
  method that cannot get this cohort right should not be trusted on the harder ones.
- **Relocation cohort.** A physician with at least two reference-table rows across different
  years whose `(reference_city, reference_state)` differs. The year of the move (and the
  immediately following year) are the eligible years under test — this is deliberately the
  opposite of the stable cohort's sanity check: tests whether a method follows a genuine update
  rather than anchoring on whichever location the majority of (possibly stale) sources still show.
- **Discordant cohort.** A physician-year where the claims table's sources do not already agree
  (i.e. `attribute_truth_fusion(method = "majority")` would report `n_candidate_values > 1`),
  restricted to physician-years that are also in the eligible (reference-verified) set. This is
  the cohort that actually tests whether Dawid-Skene/TruthFinder-family inference earns its
  complexity over plain majority vote — on physician-years where every source already agrees,
  no method can do better than majority by definition.

A physician-year may appear in more than one cohort only if its own facts put it there (e.g. a
relocation year that also happens to be discordant); cohort membership is derived from data, never
assigned to force balance.

## 6. Physician-level train/test partition

Partition **by `entity_id`**, not by physician-year: every year for a given physician is on the
same side of the split. A physician in the test partition must have *zero* claims rows used for
source-reliability estimation, and zero reference rows used for threshold tuning. Recommended
split: stratify the partition by cohort membership (§3–5) so training and test sets both contain
stable, relocating, and discordant physicians in roughly the proportions they occur in the full
eligible population — a test set that happens to be all-stable physicians would make every method
look artificially good.

## 7. Temporal holdout (optional second design)

As an alternative or complement to the physician-level split: fit (estimate source reliability,
select thresholds) using only claims and reference rows with `reference_year <= Y_train_max`, and
score only physician-years with `reference_year > Y_train_max`. This answers a different, also
useful question — "does a model fit on past years generalize to a later year" — and is the design
that actually matches how this package would be used prospectively (fit on history, predict the
current year). Use `verification_date` (§1), not `reference_year`, to additionally confirm no
reference row used for test-set scoring was itself verified before the training cutoff in a way
that could have back-leaked into source-reliability estimation (e.g. a phone audit conducted in
2023 that verifies a 2024 claim should not silently count as "future" information available in
2023's training fit).

## 8. Primary outcome

**Correct city-state classification**: for each eligible physician-year, does
`predicted_value == paste(reference_city, reference_state)` (or an equivalent joint city+state
match — exact string equality after whatever normalization `attribute_claims_validate()` already
documents for `claimed_value`, no new fuzzy matching invented for this comparison alone). Reported
as accuracy among *non-abstaining* predictions, with abstention rate reported alongside (§9) —
accuracy conditional on a prediction being offered at all is not the same claim as accuracy
including abstentions as wrong, and both numbers must be reported together, never one without the
other.

## 9. Secondary outcomes

- **Geographic distance** (for predictions that are wrong by city/state): distance between the
  predicted and reference locations, so a miss that lands one town over is visibly distinguished
  from a miss that lands across the country. Requires a geocoded reference and candidate location
  — out of scope for this package itself (see `isochrones` for geocoding), computed only at
  benchmark time from already-geocoded inputs, not inferred here.
- **Abstention rate**, overall and per cohort (§3–5) — a method that abstains far more often on
  the discordant cohort than the stable cohort is behaving sensibly; one with a flat abstention
  rate across cohorts despite wildly different evidence quality is suspect.
- **Prediction coverage**: share of eligible physician-years with a non-`NA` `predicted_value`.
- **Accuracy among discordant records specifically** (§5's cohort) — the single number closest to
  answering "does sophisticated inference beat majority vote where it matters."

## 10. Paired comparison against majority voting

Every method (`"majority"`, `"dawid_skene"`, `"one_coin_dawid_skene"`, and, once Phase 3's own
baseline-vs-majority question (see its vignettes) is answered, the `truthdiscovery` methods) is
run on the **same eligible physician-years**, from the **same claims table**, with the **same
train/test partition**. Report paired differences (method accuracy − majority accuracy) per
cohort, not just each method's accuracy in isolation — the scientific question this whole project
exists to answer is comparative ("which approach identifies the correct physician location most
reliably when sources disagree"), not "is method X accurate" standalone.

## What Phase 4 must not do

- Must not substitute the AMCB linkage panel, the isochrones aggregate CSVs, or any other
  uncorroborated administrative extract for the reference table in §1, without the caveat in §0
  attached to every reported number.
- Must not report a benchmark accuracy number before this protocol's leakage rules (§2) are
  implemented and checked — "it ran" is not "it's valid."
- Must not begin before a real reference table (§1) exists.
