# Expanding the midwifery evidence ledger: what actually happened when real data was added

Status: Step 2-4 of the expanded-evidence-ledger plan, actually run, on the real frozen
1,806-case cohort, with real T-MSIS and facility-affiliation data. Every number below is an
aggregate count or percentage computed from NPI-level files that remain in `~/midwifery`,
read-only, with explicit per-file clearance from that repository's own in-progress session
(coordinated live, cross-session, rather than guessed). **No NPI, name, or other
person-level value appears below** -- consistent with "keep person-level files private."

## 0. Cohort reproduction, checked before trusting anything built on top of it

Rebuilt the exact same claims construction `docs/midwifery-ablation-findings.md` used
(`diagnose_truth_fusion_migration_states.R`'s own logic, read directly rather than
re-derived by guesswork): the same 6 transitions (2016-17/2018-19/2022-23/2024-25 as
controls, 2017-18/2023-24 as the two spikes), the same AMCB-cohort-restricted NPPES panel,
the same DAC/PECOS/Open Payments sources.

**3,556 NPI-years touched -- identical to the published cohort, exactly.** This confirms
the frozen cohort itself (`artifacts/amcb_npi_linkage_FROZEN.csv`, 16,775-NPI AMCB linkage;
`artifacts/migration_corroboration_adjudication.csv`, the 1,806-row mover table) is stable
and was correctly re-identified, not recomputed differently by mistake. The adjudication
file's own `any_corroboration`/`independent_corroboration` counts (204/1,806 and 6/1,806)
also matched the published figures exactly.

**One honest discrepancy, chased down rather than ignored.** Re-running `majority` on the
freshly-rebuilt 3-source baseline today gives **691 of 3,556 abstentions (19.4%)**, not the
published **695 (19.5%)** -- a 4-case drift. Traced to real upstream change, not a
methodology bug: the DAC-historical-vintage and PECOS-revalidation source files have been
updated/extended since the original computation (their timestamps are days-old, after the
original ablation findings were published), introducing a handful of newly-ambiguous
single-source NPI-years (15 found) that did not exist in the version originally used. The
auxiliary corroboration sources are not static artifacts the way the frozen cohort itself
is -- this is worth remembering before treating any of these counts as permanently fixed.
**All of the "expanded" results below use today's 691-abstention baseline as the
comparison point, not the originally-published 695**, so the before/after comparison in
this document is internally consistent even though it differs slightly from the earlier doc.

## 1. Facility affiliations: zero claims added to *this* cohort

Per `docs/midwifery-evidence-ledger-design.md`, facility affiliations are dated to their
one 2026 snapshot year only, never extrapolated backward. **This cohort's 6 transitions
only touch calendar years 2016-2019 and 2022-2025 -- 2026 is not among them.** Checked
directly: 240 of the 1,806 cohort's NPIs do appear in the facility-affiliation file, but
zero of those rows land on an NPI-year this cohort actually needs. Net contribution to this
specific 1,806-case ablation: **none**. This does not mean the source is useless --
it's a real, structurally independent source for the *broader* 2013-2026 panel the
`~/midwifery` session is running, which does reach 2026. It is a no-op for this
particular frozen cohort purely because of a year-range mismatch, which is a different
and more specific conclusion than "excludable" (the original, now-rescinded, constraint-3
framing).

## 2. T-MSIS enrollment segments: real coverage, real disagreement

28,423 raw enrollment segments pulled for the cohort's NPIs (1,525 of 1,631 distinct NPIs
have at least one), filtered to qualifying ACTIVE/TERM status codes and valid state codes,
merged into contiguous per-(NPI, state) intervals, then checked against each of the
3,556 touched NPI-years:

| | n | % of 3,556 |
|---|---|---|
| covered by >=1 qualifying interval | 2,396 | 67.4% |
| ...of which, exactly one state (usable as a single-value claim) | 1,339 | 37.7% |
| ...of which, 2+ *different* states overlapping the same calendar year (ambiguous, excluded as a vote) | 1,057 | 29.7% |

The 1,057 ambiguous cases are not noise to apologize for -- they are real, simultaneous
multi-state Medicaid enrollment (a provider enrolled in more than one state's program at
once), exactly the kind of case constraint 4 ("do not treat historical enrollment as proof
of an address in that enrollment year") was written for. Excluding them from voting rather
than guessing one is the conservative, correct call, not a data-quality failure.

**515 of the 3,556 NPI-years had *zero* non-NPPES evidence before T-MSIS and gain a usable
claim from it** -- genuinely new coverage, not just additional corroboration where
evidence already existed.

**T-MSIS agrees with NPPES on the same NPI-year's state only 673 of 1,339 times (50.3%)**
-- within rounding of a coin flip. This is the single most direct empirical test available
of constraint 4's caution, and it says the caution was warranted: enrollment state is a
weak proxy for practice state on this cohort, not a strong independent confirmation of it.

## 3. Does adding T-MSIS make majority vote more decisive, or less?

The honest answer is **less, on net** -- the opposite of what "more evidence should
improve identifiability" would predict, and worth reporting exactly because it isn't the
expected direction.

| | baseline (3 sources) | +T-MSIS (4 sources) |
|---|---|---|
| majority abstentions | 691 / 3,556 (19.4%) | 759 / 3,556 (21.3%) |

Of the baseline's 691 ties, 225 are genuinely resolved by adding T-MSIS (no longer tied).
But total abstention still rose by 68, which means T-MSIS's disagreements created roughly
293 *new* ties elsewhere that weren't there before -- net, it breaks more decisions than it
fixes, under plain majority vote specifically (this says nothing yet about
`dawid_skene`/`truthfinder`, which weight sources rather than count them evenly; see
Limitations below).

**This is not uniform across years -- and the asymmetry lines up with the two spike
transitions in a way worth flagging directly:**

| year_to | baseline abstain % | +T-MSIS abstain % | direction |
|---|---|---|---|
| 2017 | 9.3% | 18.1% | up |
| **2018 (spike)** | **8.0%** | **19.3%** | **up, more than doubles** |
| 2019 | 15.8% | 20.8% | up |
| 2022 | 36.6% | 29.6% | down |
| 2023 | 28.8% | 23.4% | down |
| **2024 (spike)** | **23.7%** | **18.9%** | **down** |
| 2025 | 16.8% | 14.5% | down |

For the **2017->2018 transition**, adding an independent source more than doubles
disagreement -- T-MSIS's claims for that specific transition's NPI-years conflict with the
existing NPPES/DAC/PECOS/Open-Payments picture far more than they corroborate it. For the
**2023->2024 transition**, the opposite: T-MSIS tends to resolve rather than create ties.

This is the same conclusion the `~/midwifery` session reached by a completely different
method -- removing NPPES from the full 2013-2026 panel and watching 2017->2018 collapse
from top-ranked to 12th of 13 transitions, while 2023->2024 held up as top-tier. Two
independent tests (their subtraction of NPPES from the full panel; this addition of
T-MSIS to the 1,806-case cohort) point the same direction: **the 2017-18 migration spike is
weakly corroborated by evidence outside NPPES, and the 2023-24 spike is comparatively
well-corroborated.** Neither test alone would be conclusive; agreeing by two different
routes is stronger than either.

## 4. The weighted methods: run by the `~/midwifery` session, verified here directly

`crowd_kit`/`truthdiscovery` are not configured in the R session this document's other
numbers were computed from. Rather than leave this unrun, the expanded 4-source claims
table (same schema `attribute_claims_validate()` expects) was handed to the `~/midwifery`
session, which has both backends working, and asked to run all 7 remaining methods. Its
output files were read directly here -- not just its summary -- before trusting any of
the following numbers.

**Sanity check passed first:** majority re-run on just the 3 original sources from that
table gives 691/3,556 abstentions, exactly matching §0's baseline; the 4-source majority
run gives 759/3,556, exactly matching §3. Both independently reproduced from a from-scratch
read of the handed-off file, not copy-pasted.

**Every one of the 7 weighted methods abstains 0% of the time** on this cohort (verified
directly from each `predictions_*.csv`) -- the same pattern the original 3-source ablation
found for `dawid_skene`/`truthfinder`, now holding for all 7 with a 4th source added. A
weighted method almost always has enough signal to pick a winner even when plain majority
ties.

**T-MSIS's learned reliability has no consensus across methods** -- verified directly from
each `source_diagnostics_*.csv`, not taken on report:

| method | nppes | dac_pecos_combined | open_payments | tmsis_medicaid_enrollment |
|---|---|---|---|---|
| `dawid_skene` | 0.847 | 0.686 | 0.696 | **0.551 (lowest)** |
| `one_coin_dawid_skene` | 0.785 | 0.770 | 0.824 | **0.693 (lowest)** |
| `sums` | 1.000* | 0.504 | 0.135 | 0.294 |
| `average_log` | 0.822 | 0.972 | 1.000* | 0.847 |
| `investment` | 0* | 1e-4* | 1.000* | 0* |
| `pooled_investment` | 1.000* | 4e-4* | 0.002* | 0* |
| `truthfinder` | 0.614 | 0.641 | 0.674 | 0.635 |

(`*` = degenerate 0/1-style split, consistent with what §2 of
`docs/midwifery-ablation-findings.md` already flagged for `investment`/`pooled_investment`
generally.)

`dawid_skene` and `one_coin_dawid_skene` both rank T-MSIS *lowest* of the 4 sources --
consistent with its measured 50.3% NPPES-agreement rate reading as noise to a method that
actually learns reliability. `average_log` ranks it third but close behind
`dac_pecos_combined` and ahead of `nppes`. `truthfinder` barely distinguishes any of the 4
(a tight 0.61-0.67 cluster). This is the same "different methods learn contradictory
reliability rankings for the same sources" finding `docs/midwifery-ablation-findings.md`
§2 established for the original 3 sources, now extended to a 4th: adding a source doesn't
resolve the disagreement about how much to trust it.

**Does adding a weighted 4th source change how much `dawid_skene` and `truthfinder` agree
with each other on `majority`'s ties, or how much `dawid_skene`'s resolution is just
"trust NPPES"?** The same two checks `docs/midwifery-ablation-findings.md` §3-4 ran on the
original 695 ties, re-run here on the new 759 (verified directly from the prediction
files, joined against the expanded claims table's own `nppes` rows):

| check | 3-source / 695 ties (original) | 4-source / 759 ties (this table) |
|---|---|---|
| `dawid_skene` vs. `truthfinder` agree on resolved value | 46 (6.6%) | 145 (19.1%) |
| `dawid_skene`'s resolution equals NPPES's own claim | 97.6%* | 601 (79.2%) |

(`*` the original figure was of the 678/695 single-remaining-source cases specifically,
not all 695; the right-hand column here is of all 759.) Both numbers move in the direction
"more real synthesis, less pure NPPES-restatement" once a 4th source is added -- agreement
nearly triples, and the trust-NPPES share drops by about 18 points. This is a genuinely
different, more encouraging signal than §3 of this document (which only tested `majority`)
on its own would suggest: a weighted method given a genuinely independent 4th source
does move away from reflexively trusting NPPES, even though that same 4th source
increases plain-majority disagreement. The two results are not in tension -- majority
counts votes, `dawid_skene` weighs them, and a low-weight dissenting source can create a
majority tie while barely moving a Dawid-Skene posterior.

## 5. What this does and does not establish

It establishes: the facility-affiliation source is a real no-op for this specific cohort
(year-range mismatch, not uselessness); T-MSIS adds genuine new coverage (515 NPI-years)
but disagrees with the existing sources about as often as it agrees (50.3%); under plain
majority vote, that disagreement nets out to *more* abstention, not less, with a specific,
asymmetric pattern across years that corroborates -- via an entirely independent method --
the `~/midwifery` session's own finding about the 2017-18 spike's NPPES-dependence; and
under the weighted methods, T-MSIS's low measured reliability is recognized by some
(`dawid_skene`, `one_coin_dawid_skene`) but not others (`truthfinder`'s tight cluster),
the same contradictory-rankings pattern the original 3-source ablation already found,
now extended to a 4th source rather than resolved by adding one.

It does **not** establish that the expanded ledger is simply "better" -- `majority` and
the weighted methods disagree about whether T-MSIS helps (more net disagreement under one,
less NPPES-dependence and more cross-method agreement under the other), and no
independently verified reference location exists for this cohort to adjudicate which
reading is closer to the truth (the same limitation `docs/midwifery-ablation-findings.md`
§0 and `docs/validation-protocol.md` already establish for the original 3-source case).
Step 5 of the full plan (the annual 2013-2026 panel reconstruction comparing NPPES-only
vs. expanded-evidence-inferred transitions) and Step 6's formal before/after table remain
undone on this cohort's side; the `~/midwifery` session's own full-panel leave-one-out
result is the closest existing analogue and is cited directly above rather than duplicated.
