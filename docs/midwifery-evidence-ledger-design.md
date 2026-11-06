# Expanding the midwifery evidence ledger beyond three sources: schema and source verdicts

Status: a design specification plus one new, tested package function
(`attribute_claims_collapse_lineage()`). **No new data has been pulled into a
claims table yet.** The `midwifery` repository has a separate, live,
in-progress build running its own full-panel (216,653 NPI-year, 2013-2026)
fit of the existing three sources; by explicit agreement with that session,
this package stays out of `~/midwifery`'s files entirely while that build is
running. This document is the schema and per-source verdicts the eventual
ledger construction should follow once that's unblocked -- it does the part
of Step 2 (of the six-step expanded-evidence-ledger plan) that doesn't
require touching `midwifery`'s files: building the standardized schema and
the lineage-collapse mechanism in `mysteryroster` itself, and recording,
source by source, what the completed Step 1 audit of `~/midwifery` found.

`docs/midwifery-ablation-findings.md` (§1) built its claims table from three
sources (`nppes`, `dac_pecos_combined`, `open_payments`) for the same 1,806
real NPPES-defined mover transitions. The task now is to decide, for each
*additional* candidate source the Step 1 audit found already built in
`~/midwifery`, whether and how it can be added without violating the
explicit constraints below (originally six; one later rescinded -- see
constraint 3).

## The constraints, restated as what they rule out

These were given verbatim as hard "Do not" rules for this expansion, and
every row of the source-verdict table below cites which one applies.
**Constraint 3 (hospital affiliation != practice address) was explicitly
rescinded by the user on 2026-11-05: hospital facility affiliations are to
be used as practice-location evidence, not walled off into a separate
attribute.** It is kept below, struck through, so the reasoning it used to
encode and the fact that it was a deliberate override -- not an oversight --
both stay visible.

1. **Do not count multiple rows from the same source as independent votes.**
   Already enforced by `attribute_truth_fusion()`'s row-level deduplication
   and `attribute_claims_validate()`'s `duplicate_source_years` check.
2. **Do not count DAC and PECOS as independent when they derive from common
   upstream records.** Generalized by the new
   `attribute_claims_collapse_lineage()` (below) to any declared lineage
   group, not just the one pair the original ablation hardcoded. This now
   has direct bearing on hospital facility affiliations -- see the
   source-verdict row below.
3. ~~Do not treat a hospital affiliation as equivalent to a primary practice
   address.~~ **Rescinded.** The original reasoning: a hospital affiliation
   is a fact about a different entity (the hospital's location), not the
   clinician's own address, so counting it as a `practice_state` vote risks
   conflating "works at/credentialed at this hospital" with "practices in
   this state." The user's override treats that risk as acceptable here --
   plausibly because a midwife's hospital affiliation is a materially
   stronger signal of her practice state than it would be for, say, a
   purely outpatient specialist, and because no other candidate source adds
   comparably direct new coverage. Superseded by the verdict below: loaded
   directly as `practice_state` evidence, `evidence_basis = "affiliation"`
   kept as a descriptive tag (so this is still visible to anyone reading
   the ledger) rather than as a structural exclusion.
4. **Do not treat historical enrollment as proof of an address in that
   enrollment year.** An enrollment interval is evidence the clinician was
   enrolled in a state's program during that interval -- it is not a
   geocoded practice address. Loaded as a claim, but with a schema
   convention (`evidence_basis`, below) that keeps this distinction visible
   to anyone consuming the table, rather than flattening it into a claim
   that looks identical in kind to an NPPES address row.
5. **Do not treat a dated download as a dated observation of practice
   location.** `valid_from`/`valid_to` (already checked by
   `attribute_claims_validate()`) must be the record's own stated effective
   dates. A source with only a scrape/download timestamp and no effective
   date does not get `valid_from`/`valid_to` populated from that timestamp.
6. **Do not propagate a current record backward without an explicit
   sensitivity analysis.** No candidate source below is backdated under this
   design; a current-only source either contributes only at its one true
   year or is excluded.

## Schema convention: `evidence_basis`

`attribute_claims_validate()`'s required columns
(`entity_id`, `attribute`, `source_id`, `claimed_value`, `reference_year`)
are unchanged -- this is an additive convention, not a schema break.
Every claim added under this expansion should carry one more column,
`evidence_basis`, one of:

| value | meaning | can it populate `attribute = "practice_state"`? |
|---|---|---|
| `direct_snapshot` | the source's own record of the clinician's practice address/state, as of a stated date | yes (what all three original sources already are) |
| `enrollment_interval` | a dated interval of program enrollment in a state, not a geocoded address | yes, but see constraint 4 -- treat as weaker, distinct evidence, never silently merged into the same lineage family as a `direct_snapshot` source |
| `affiliation` | a fact about an *affiliated entity* (hospital, facility: a CCN is a real physical facility with a real state) | **yes, per the constraint-3 override above** -- loaded as a `practice_state` claim like any other single-snapshot source; the tag stays only as a descriptive note of what kind of evidence it is, not a structural block |
| `scrape_current_only` | a profile scrape with only a download/observation timestamp, no source-stated effective date | yes, but `valid_from`/`valid_to` must be left `NA` per constraint 5, so a consumer can filter it out of any longitudinal (year-specific) use |

## Source-by-source verdict (from the completed Step 1 audit)

| candidate source | `~/midwifery` artifact (as audited) | evidence_basis | lineage family | verdict |
|---|---|---|---|---|
| Historical DAC/Physician Compare vintages | `artifacts/physician_compare_historical_midwives.csv`, 14 vintages 2013-2025 | `direct_snapshot` | `dac_pecos` (existing) | already in `dac_pecos_combined`; no new information without re-splitting that merge, which is out of scope here |
| PECOS revalidation | `cms_revalidation.duckdb`, 40 monthly vintages 2021-12 to 2026-09 | `direct_snapshot` | `dac_pecos` (existing, per `SHARED_LINEAGE_GROUPS`) | already in `dac_pecos_combined` |
| PPEF / NBER PPEF | single current snapshot | n/a | n/a | **exclude** -- audit found only an enrollment-ID-derived date, no state/address field at all; nothing to add |
| Hospital facility affiliations | `artifacts/dac_facility_affiliations.csv`, current, vintage-matched to 2026-06 DAC | `affiliation` | independent (own family) -- **confirmed, not inferred**: the `~/midwifery` session read `extract_dac_facility_affiliations.R` directly and reports that although the NPI-CCN affiliation *link* is published alongside DAC in the same monthly CMS release (vintage-matched), the `state` column traces to `cms_hospital_info` (CMS Hospital General Information / Care Compare) joined in via CCN -- the *facility's own* independent CMS registration, never DAC/PECOS's clinician-address pipeline. Structurally independent lineage, confirmed from the actual code | **include as a direct `practice_state` claim, dated 2026-only, never extrapolated to earlier years** -- per constraint 3's override; same single-snapshot treatment already applied to Healthgrades/Doximity/state boards elsewhere. **Missingness caveat (from the same script's comments, via the `~/midwifery` session):** only Medicare-enrolled clinicians appear in DAC at all, and of those, only ones with a recorded facility privilege appear in this file -- so a midwife's *absence* from this source means "not Medicare-enrolled" or "no recorded hospital privilege," never "does not practice at a hospital." Unlike the other sources, this source's absence is not informative and must not be read as corroborating or contradicting evidence either way when any downstream corroboration metric (e.g. `any_corroboration`) is computed. |
| Trilliant work sites | `midwife_work_sites_long.csv`, claims-derived, single snapshot (evidence_date 2026-06-25) | `direct_snapshot` for that one date; `scrape_current_only`-like for any other year | independent (own family) | include at its one dated snapshot year only; do not propagate to other years (constraint 6) |
| Open Payments, per-payment history | merged DuckDB, dated 2013-2024 (`Date_of_Payment`); some corrupted dates (year 0002, 3015) flagged by the audit | `direct_snapshot` | independent (already `open_payments`) | include the richer per-payment year coverage, but **filter corrupted dates before loading** -- a malformed date is not a valid `reference_year`, and silently coercing it would violate constraint 5's spirit even though it's a different failure mode |
| Medicaid T-MSIS enrollment segments | `medicaid_provider_enrollment_segments.duckdb`, 66M rows, `ENRL_START_DT`/`ENRL_END_DT` + `STATE_CD`, Jan 2018-Dec 2024 | `enrollment_interval` | independent (own family) | **the one genuinely new, independently-lineaged, properly-dated candidate** -- include, tagged `enrollment_interval`, never merged into a `direct_snapshot` lineage family; NPI-completeness varies wildly by state (audit found states as low as ~0%), so coverage must be measured per state before trusting any null result from it |
| Medicaid T-MSIS spending (claims-level) | billing-claim level, matches on servicing NPI | n/a | n/a | **exclude** -- billing activity is not a practice-address claim at all (closer to constraint 3's logic than constraint 4's: it's evidence about where a claim was billed, not where the clinician practices) |
| Doximity / Healthgrades | scraped profiles, scrape-time dates only | `scrape_current_only` | independent, if added | include only with `valid_from`/`valid_to` left `NA` (constraint 5); likely low marginal value given no effective-date field |
| State boards (BON) | live/current only, per existing BON-harvester project | `scrape_current_only` | independent, if added | same treatment as Doximity/Healthgrades; current-only, no historical dating |

Net of this table: **two clearly new, independent sources** (T-MSIS
enrollment segments, and hospital facility affiliations as of the constraint-3
override above), **one source gaining dated coverage it didn't have before**
(Open Payments' fuller per-payment history, after a date-sanity filter), and
several sources that add nothing beyond what `dac_pecos_combined` already
contains, or that the audit found have no usable effective-date field at all.

## What `attribute_claims_collapse_lineage()` does and does not solve here

The new function (`R/claims_lineage.R`) takes a claims table and a named
list of lineage groups (`list(dac_pecos = c("dac", "pecos_revalidation"))`)
and, per entity-attribute-year: collapses an agreeing group to one vote
cast by the family; drops a disagreeing group entirely (never picks one);
passes through unchanged when only one group member has a claim there. This
is the general form of the rule the original `dac_pecos_combined` merge
already applied once, ad hoc -- it is now reusable for any future
lineage-sharing pair. Facility affiliations turned out not to need it: the
`~/midwifery` session confirmed from `extract_dac_facility_affiliations.R`
that the state evidence traces to CMS's Hospital General Information
registry via CCN, not to DAC/PECOS's clinician pipeline -- structurally
independent, not just coincidentally dated together -- but the function is
ready the day a genuinely lineage-sharing pair among the newer sources
*is* found.

It does **not** decide which sources belong in which family -- that is a
provenance judgment (this document's table), supplied by the caller, not
inferred from how often two sources happen to agree. It also does not solve
constraints 4-6; those are schema-tagging and construction-time discipline
(the `evidence_basis` convention above), not a generic collapse rule, because
"is this an enrollment interval" and "is this date real" are not statistical
properties a lineage-collapse function could detect from the claims table
alone.

## Update: Steps 2-4 actually run against real data

See `docs/midwifery-evidence-ledger-results.md` for the real, aggregate-only
results: the `~/midwifery` session cleared specific static artifacts
file-by-file (coordinated live, cross-session), and the real 1,806-case
cohort was rebuilt, extended with real T-MSIS claims, and re-fit under
`majority`. Headline: facility affiliations turn out to contribute zero
claims to *this specific* cohort (a year-range mismatch, not uselessness --
see that doc's §1); T-MSIS adds genuine new coverage but agrees with NPPES
only ~50% of the time, and *increases* majority-vote abstention on net,
asymmetrically by transition in a way that corroborates the `~/midwifery`
session's own NPPES-removal finding about the 2017-18 spike.

## Explicitly not done yet

- The weighted methods (`dawid_skene`, `truthfinder`, `one_coin_dawid_skene`,
  `sums`, `average_log`, `investment`, `pooled_investment`) have not been run
  against the expanded (+T-MSIS) claims table -- only `majority`, because
  `crowd_kit`/`truthdiscovery` are not configured in the R session this was
  run from. Majority treats every vote equally, so T-MSIS's ~50% NPPES
  agreement rate necessarily reads as noise to it; a weighted method could
  behave very differently. Needs either those backends configured here, or
  the `~/midwifery` session (which has them working) running against this
  same expanded claims table directly.
- Step 5 (the full 2013-2026 annual panel reconstruction, NPPES-only vs.
  expanded-evidence-inferred) has not been attempted on this cohort's side --
  the `~/midwifery` session's own full-panel leave-one-source-out result
  (removing NPPES from the 219,536-entity-year panel) is the closest existing
  analogue and is cited directly in `docs/midwifery-evidence-ledger-results.md`
  §3 rather than duplicated.
- Step 6's formal before/after comparison table and technical appendix are
  not yet written as a standalone deliverable -- the comparison exists in
  prose form in `docs/midwifery-evidence-ledger-results.md`, not yet as the
  structured table Step 6 calls for.
- `attribute_claims_collapse_lineage()`'s tests (`tests/testthat/
  test-claims-lineage.R`) use synthetic fixtures only, by design -- they
  check the function's rule, not any real source's actual agreement rate.
- The `~/midwifery` session's own DAC/PECOS merge (`merge_dac_pecos_sources()`
  in `R/lib/truth_fusion_claims.R`) is hand-rolled and logic-equivalent to
  `attribute_claims_collapse_lineage()`, not yet switched over -- deferred
  deliberately so an already-running full-panel pass isn't interrupted to
  swap in an equivalent implementation. Worth confirming this actually
  happened before anyone treats the two as interchangeable long-term.
