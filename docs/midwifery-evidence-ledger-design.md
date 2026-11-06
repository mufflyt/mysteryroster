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
`~/midwifery`, whether and how it can be added without violating the six
explicit constraints below.

## The six constraints, restated as what they rule out

These were given verbatim as hard "Do not" rules for this expansion, and
every row of the source-verdict table below cites which one applies:

1. **Do not count multiple rows from the same source as independent votes.**
   Already enforced by `attribute_truth_fusion()`'s row-level deduplication
   and `attribute_claims_validate()`'s `duplicate_source_years` check.
2. **Do not count DAC and PECOS as independent when they derive from common
   upstream records.** Generalized by the new
   `attribute_claims_collapse_lineage()` (below) to any declared lineage
   group, not just the one pair the original ablation hardcoded.
3. **Do not treat a hospital affiliation as equivalent to a primary practice
   address.** Not a lineage problem -- a different fact about a different
   entity (a hospital's location, not the clinician's own address). Handled
   by *never* loading a hospital-affiliation extract under
   `attribute = "practice_state"` at all (see the facility-affiliation row
   below), not by a weight or a flag that could later be misread as a vote.
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
| `affiliation` | a fact about an *affiliated entity* (hospital, facility), not the clinician directly | **no** -- load under a different `attribute` value (e.g. `affiliated_facility_state`) so it is structurally incapable of being counted as a `practice_state` vote |
| `scrape_current_only` | a profile scrape with only a download/observation timestamp, no source-stated effective date | yes, but `valid_from`/`valid_to` must be left `NA` per constraint 5, so a consumer can filter it out of any longitudinal (year-specific) use |

## Source-by-source verdict (from the completed Step 1 audit)

| candidate source | `~/midwifery` artifact (as audited) | evidence_basis | lineage family | verdict |
|---|---|---|---|---|
| Historical DAC/Physician Compare vintages | `artifacts/physician_compare_historical_midwives.csv`, 14 vintages 2013-2025 | `direct_snapshot` | `dac_pecos` (existing) | already in `dac_pecos_combined`; no new information without re-splitting that merge, which is out of scope here |
| PECOS revalidation | `cms_revalidation.duckdb`, 40 monthly vintages 2021-12 to 2026-09 | `direct_snapshot` | `dac_pecos` (existing, per `SHARED_LINEAGE_GROUPS`) | already in `dac_pecos_combined` |
| PPEF / NBER PPEF | single current snapshot | n/a | n/a | **exclude** -- audit found only an enrollment-ID-derived date, no state/address field at all; nothing to add |
| Hospital facility affiliations | `artifacts/dac_facility_affiliations.csv`, current, vintage-matched to 2026-06 DAC | `affiliation` | none (own family if ever added) | **include only as `affiliated_facility_state`**, a separate attribute -- per constraint 3, never as `practice_state` |
| Trilliant work sites | `midwife_work_sites_long.csv`, claims-derived, single snapshot (evidence_date 2026-06-25) | `direct_snapshot` for that one date; `scrape_current_only`-like for any other year | independent (own family) | include at its one dated snapshot year only; do not propagate to other years (constraint 6) |
| Open Payments, per-payment history | merged DuckDB, dated 2013-2024 (`Date_of_Payment`); some corrupted dates (year 0002, 3015) flagged by the audit | `direct_snapshot` | independent (already `open_payments`) | include the richer per-payment year coverage, but **filter corrupted dates before loading** -- a malformed date is not a valid `reference_year`, and silently coercing it would violate constraint 5's spirit even though it's a different failure mode |
| Medicaid T-MSIS enrollment segments | `medicaid_provider_enrollment_segments.duckdb`, 66M rows, `ENRL_START_DT`/`ENRL_END_DT` + `STATE_CD`, Jan 2018-Dec 2024 | `enrollment_interval` | independent (own family) | **the one genuinely new, independently-lineaged, properly-dated candidate** -- include, tagged `enrollment_interval`, never merged into a `direct_snapshot` lineage family; NPI-completeness varies wildly by state (audit found states as low as ~0%), so coverage must be measured per state before trusting any null result from it |
| Medicaid T-MSIS spending (claims-level) | billing-claim level, matches on servicing NPI | n/a | n/a | **exclude** -- billing activity is not a practice-address claim at all (closer to constraint 3's logic than constraint 4's: it's evidence about where a claim was billed, not where the clinician practices) |
| Doximity / Healthgrades | scraped profiles, scrape-time dates only | `scrape_current_only` | independent, if added | include only with `valid_from`/`valid_to` left `NA` (constraint 5); likely low marginal value given no effective-date field |
| State boards (BON) | live/current only, per existing BON-harvester project | `scrape_current_only` | independent, if added | same treatment as Doximity/Healthgrades; current-only, no historical dating |

Net of this table: **one clearly new, worthwhile source** (T-MSIS enrollment
segments), **one source addable but structurally separated from
`practice_state`** (hospital facility affiliations, as
`affiliated_facility_state`), **one source gaining dated coverage it didn't
have before** (Open Payments' fuller per-payment history, after a date-sanity
filter), and several sources that add nothing beyond what
`dac_pecos_combined` already contains, or that the audit found have no
usable effective-date field at all.

## What `attribute_claims_collapse_lineage()` does and does not solve here

The new function (`R/claims_lineage.R`) takes a claims table and a named
list of lineage groups (`list(dac_pecos = c("dac", "pecos_revalidation"))`)
and, per entity-attribute-year: collapses an agreeing group to one vote
cast by the family; drops a disagreeing group entirely (never picks one);
passes through unchanged when only one group member has a claim there. This
is the general form of the rule the original `dac_pecos_combined` merge
already applied once, ad hoc -- it is now reusable for any future
lineage-sharing pair (for instance, if facility affiliations and DAC are
ever found to share more lineage than "vintage-matched," they could be
declared a family the same way).

It does **not** decide which sources belong in which family -- that is a
provenance judgment (this document's table), supplied by the caller, not
inferred from how often two sources happen to agree. It also does not solve
constraints 3-6; those are schema-tagging and construction-time discipline
(the `evidence_basis` convention above), not a generic collapse rule, because
"is this an address" and "is this date real" are not statistical properties
a lineage-collapse function could detect from the claims table alone.

## Explicitly not done yet

- No real data from any of the sources above has been loaded into a claims
  table. That requires reading the actual `~/midwifery` artifacts, which is
  on hold while that session's full-panel build is in progress.
- Steps 3-6 of the full plan (information-gain measurement, the repeated
  ablation experiment, the 2013-2026 panel reconstruction, the before/after
  comparison table) all depend on that real ledger existing first and are
  therefore also not started.
- `attribute_claims_collapse_lineage()`'s tests (`tests/testthat/
  test-claims-lineage.R`) use synthetic fixtures only, by design -- they
  check the function's rule, not any real source's actual agreement rate.
