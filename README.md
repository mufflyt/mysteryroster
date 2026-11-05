# mysteryroster <img src="man/figures/logo.png" align="right" height="139" alt="mysteryroster hex sticker: a clipboard roster with two confirmed rows, one invalid, and a magnifying glass" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/mufflyt/mysteryroster/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mufflyt/mysteryroster/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R ≥ 4.5.2](https://img.shields.io/badge/R-%E2%89%A54.5.2-276DC3?logo=r&logoColor=white)](https://cran.r-project.org/)
[![ORCID](https://img.shields.io/badge/ORCID-0000--0002--2044--1693-A6CE39?logo=orcid&logoColor=white)](https://orcid.org/0000-0002-2044-1693)
<!-- badges: end -->

Registry overlap strata and listing-validity analysis for mystery-caller
audits of physician directories.

## The question

National physician directories (NPPES, certifying boards, professional
societies) count different physicians and are rarely checked against reality.
A registry-validity study cross-links them, calls a stratified sample of listed
practices, and asks one thing of each physician-location listing: is the named
physician still practising the indexed specialty at the listed address?

`mysteryroster` is the specialty-agnostic core of that study. It was carved out
of the ENT registry-validity study (Falk et al.) so the OB/GYN subspecialty
study, and any later one, runs the same steps with its own registries and its
own disposition codebook.

## Installation

```r
# install.packages("remotes")
remotes::install_github("mufflyt/mysteryroster")
```

## Vignettes

- `vignette("registry-overlap")` -- cross-linking registries into Euler
  zones, the closure guarantee `overlap_zone_counts()` enforces, `frame_strata()`,
  the area-proportional diagram, and a bridge to `attribute_agreement()`.
- `vignette("attribute-agreement")` -- pairwise attribute agreement and
  record freshness across four simulated sources, with three figures.
- `vignette("listing-validity-study")` -- the rest of the pipeline, end to
  end: sampling, call dispositions, design-weighted estimates, sensitivity
  bounds, the survey-weighted model, and the flow/cascade counts, all
  reconciled against one synthetic call log.
- `vignette("lead-lag-staleness")` -- the three longitudinal approaches
  below, first on a three-NPI toy example and then on a simulated
  1,500-clinician panel with five figures, including the full
  manuscript-paragraph output of `format_no_gold_standard_prose()`.
- `vignette("consensus-and-corroboration")` -- beyond pairwise, with
  three or more sources: how much of the group agrees, whether that
  agreement holds up year over year, which source the group tends
  to converge toward, whether exactly one source is a lone dissenter,
  agreement stratified by any covariate, and Wilson confidence
  intervals on any of the above.
- `vignette("truth-fusion")` -- the one function in this package that
  *does* predict a value rather than only making a relative claim:
  majority-vote inference with explicit abstention on ties or weak
  majorities, and the evidence schema it expects.

## How the pieces fit

```mermaid
flowchart TD
    R1[("NPPES<br/>(frame registry)")] --> OV
    R2[("Certifying board")] --> OV
    R3[("Professional society")] --> OV
    OV["registry_overlap()<br/>one row per NPI, membership flags, overlap_zone"] --> ZC["overlap_zone_counts()<br/>plot_registry_overlap()"]
    OV --> FS["frame_strata()<br/>frame rows only; stratum = overlap zone"]
    FS --> SM["stratified_listing_sample()<br/>allocation per stratum; weight = N / n"]
    CALLS --> LW["listing_weights()<br/>weight = N / n for an existing call log"]
    LW --> DS
    SM --> CALLS[/"telephone audit<br/>disposition, reached, phone_valid"/]
    CALLS --> CL["classify_listing_status()<br/>confirmed_valid | confirmed_invalid | unresolved | not_attempted"]
    CL --> DS["listing_validity_design()<br/>survey::svydesign, strata + weights"]
    DS --> VE["validity_estimates()<br/>weighted % valid, by stratum; outcome = location or workforce"]
    DS --> VM["validity_model()<br/>survey-weighted logistic regression"]
    CL --> SB["validity_sensitivity_bounds()<br/>observed, best case, worst case"]
    CL --> FC["listing_flow_counts()<br/>sampled, attempted, reached, definitive"]
    CL --> VC["validation_cascade()<br/>each stage % of attempted and % of prior stage"]
    classDef fn fill:#1F2A33,color:#F6F3E7,stroke:#1F2A33;
    classDef data fill:#F6F3E7,color:#1F2A33,stroke:#1F2A33;
    class OV,ZC,FS,SM,LW,CL,DS,VE,VM,SB,FC,VC fn;
    class R1,R2,R3,CALLS data;
```

Dark boxes are package functions; light shapes are your inputs. Every arrow
is a data frame handed from one call to the next, so the study is a straight
line from registries to tables.

## The study in six calls

```r
library(mysteryroster)

# 1. Cross-link registries by NPI. The first registry is the sampling frame.
ov <- registry_overlap(list(nppes = nppes_npis, board = board_npis, society = society_npis))
overlap_zone_counts(ov)        # seven Euler zones, closure-checked
plot_registry_overlap(ov)      # area-proportional Euler diagram (eulerr)

# 2. Protocol strata: the frame registry's overlap with the others.
frame <- frame_strata(ov, frame = "nppes")

# 3. A stratified sample that keeps its sampling probabilities.
listings <- stratified_listing_sample(
  frame, allocation = c("nppes" = 100, "nppes+board" = 150, "nppes+board+society" = 150),
  seed = 20261009)

# 4. Call dispositions become four exhaustive statuses.
calls <- classify_listing_status(call_log, map = ent_disposition_map())
table(calls$listing_status)    # confirmed_valid / confirmed_invalid / unresolved / not_attempted

# 5. Design-weighted estimates, bounds, and a survey-weighted model.
design <- listing_validity_design(calls)
validity_estimates(design, by = "stratum")
validity_sensitivity_bounds(calls)
validity_model(design, ~ stratum + rural)

# 6. The flow diagram's numbers, closure-checked.
listing_flow_counts(calls, by = "stratum")
```

## Beyond membership: does a shared attribute agree?

`registry_overlap()` only asks whether an NPI is *present* in each source.
`attribute_agreement()` takes that a step further: for every pair of sources
that share an identifier, does a common attribute (practice state, say)
actually agree? There is still no gold standard here -- the comparison is
always pairwise, never against an assumed truth -- and every source's own
freshness date (its record's last-updated date or snapshot/vintage date,
never the date it was downloaded) is carried through so a disagreement
between a 2015 record and a 2024 one reads differently from one between two
current records.

```r
# Coverage/agreement audit across sources that don't share a gold standard,
# with each source's own freshness date -- not download date -- tuning which
# year a row is actually evidence for.
ag <- attribute_agreement(
  list(nppes = nppes_df, pecos = pecos_df, dac = dac_df, open_payments = op_df),
  attribute = "state", year = "year", observed_at = "last_updated")
attribute_agreement_summary(ag)   # n_compared, n_agree, pct_agree per source pair per year
```

`record_age_years()`/`record_age_band()`/`classify_record_staleness()` are
the same freshness-tuning primitives `mysterymedicaid::mm_record_staleness.R`
ports from isochrones, so a disagreement is never read as "one source is
wrong" -- only as "this side provably cannot reflect information the other
side already had."

<p>
<img src="man/figures/attribute-agreement-fig-pair-agreement-1.png" width="32%" alt="Dot plot of pairwise state agreement by audit year for six simulated source pairs; pairs that include NPPES agree least." />
<img src="man/figures/attribute-agreement-fig-staleness-breakdown-1.png" width="32%" alt="Stacked bars of 2024 disagreements per source pair, split by which side's record is provably older; pairs with an undated source are all no temporal basis." />
<img src="man/figures/attribute-agreement-fig-age-band-1.png" width="32%" alt="Bars of 2024 state agreement by the older record's age band, falling from 99 percent under one year to 75 percent at five or more years." />
</p>

*Simulated sources from `vignette("attribute-agreement")`. Left: agreement
per pair, with no source taken as truth. Middle: for each disagreement,
which side's own record date is provably older. Right: agreement by the
older record's age.*

## When there's no gold standard, but there is time

A single cross-section can only say two sources disagree, not which one
is stale. With repeated annual snapshots, three further approaches (see
`vignette("lead-lag-staleness")`) use timing itself to break that symmetry:

```r
# Approach 1: does Source A or Source B move first, and does the other
# eventually adopt its value, or revert back?
ll <- attribute_lead_lag_matrix(nppes_panel, open_payments_panel,
                                 attributes = c("state", "city", "zip5", "street"),
                                 cross_section = dac_current, cross_section_year = 2026L)

# Approach 2: does agreement decay with how long a source's value has sat unchanged?
panel_b <- attribute_spell_age(nppes_panel, attribute = "street")
attribute_agreement_by_spell_age(dac_current, panel_b, attributes = c("state", "street"))

# Approach 3: do administrative enrollment dates arrive before or after a panel move?
enrollment_move_lead_lag(nppes_panel, medicaid_enrollment_intervals)

# All three, as manuscript-ready paragraphs:
format_no_gold_standard_prose(ll, freshness_strata, enrollment_timing)
```

<p align="center">
<img src="man/figures/lead-lag-staleness-fig-toy-timeline-1.png" width="70%" alt="Timeline of the state reported by Source A and Source B for three NPIs over 2022 to 2024: Source A moves to NM a year before Source B for NPI 1; Source B corrects NJ to NY a year after Source A for NPI 2; Source A corrects TX to CO for NPI 3, matching Source B from 2023." />
</p>

*The three-NPI toy example: who moves first?*

<p>
<img src="man/figures/lead-lag-staleness-fig-concordance-breaks-1.png" width="49%" alt="Grouped bars: among concordant provider-years, Source A changes first far more often than Source B at state, city, ZIP, and street level." />
<img src="man/figures/lead-lag-staleness-fig-discordance-outcomes-1.png" width="49%" alt="Stacked bars of what happens one year after a disagreement, for each attribute: B adopts A, A reverts to B, both persist, or other." />
</p>
<p>
<img src="man/figures/lead-lag-staleness-fig-spell-age-decay-1.png" width="49%" alt="Lines of percent agreement with a 2026 cross-section by Source B's street spell-age stratum, dropping sharply at 11 or more years." />
<img src="man/figures/lead-lag-staleness-fig-enrollment-timing-1.png" width="49%" alt="Bars of enrollment timing against panel moves: destination enrollment usually starts before the panel move, and origin enrollment usually ends after it or is still open." />
</p>

*Simulated 1,500-clinician panel from `vignette("lead-lag-staleness")`.
Top row (Approach 1): which source breaks concordance first, and what
happens to a disagreement one year later. Bottom left (Approach 2):
agreement by how long Source B's value has sat unchanged. Bottom right
(Approach 3): enrollment arrival and departure dates relative to the
panel move.*

None of the three claims a source is correct -- only that it moved first,
that disagreement tracks a measurable staleness clock, or that an
administrative date landed before or after a panel caught up.

`format_no_gold_standard_prose()` fills in numbers, but its wording (study
years, source-specific mechanisms, verbs like "confirming") is fixed to the
OB/GYN midwifery study. Treat its output as a first draft; see
`?format_no_gold_standard_prose` for what it requires and what it fixes.

## Beyond pairwise: consensus and corroboration

With three or more sources, further relative claims become available --
see `vignette("consensus-and-corroboration")`:

```r
# How much of the group agrees, per identifier-year (never which value is right):
tier <- attribute_consensus_tier(list(nppes = nppes_df, dac = dac_df, op = op_df, pecos = pecos_df),
                                  attribute = "state")

# Does that consensus hold up year over year, or churn?
attribute_consensus_tier_flow(tier)

# Which source does the rest of the comparison set tend to converge toward?
attribute_source_corroboration(list(nppes = nppes_panel, op = op_panel, dac = dac_panel),
                                attributes = "state")

# Is exactly one source a lone dissenter, distinct from a tied split with no majority?
attribute_odd_one_out_rate(attribute_odd_one_out(
  list(nppes = nppes_df, dac = dac_df, op = op_df, pecos = pecos_df), attribute = "state"))

# Does agreement differ by rurality, provider type, or any other covariate?
attribute_agreement_by_covariate(nppes_df, dac_df, rurality_lookup,
                                  attribute = "state", covariate = "setting")

# How much should a percentage above actually be trusted, given its n?
attribute_agreement_add_ci(attribute_agreement_summary(attribute_agreement(sources, attribute = "state")))
```

`attribute_consensus_tier()`, `attribute_consensus_tier_flow()`, and
`wilson_ci()` were ported from a one-off script written for a midwifery
workforce audit outside this package, generalized past that script's
hardcoded assumptions (exactly four sources; one fixed agreement table).
None of these functions ever picks a value as the group's answer --
only the size of the largest agreeing group, whether that size holds up
over time, which source others tend to adopt, whether one source sits
alone outside a shared value, or how much any of the above should be
trusted given how many identifier-years back it.

## Predicting a value: truth fusion

Everything above makes only relative claims -- agreement, timing,
stability -- and deliberately never names a value as correct (see
Design rules, below). `attribute_truth_fusion()` is the one exception,
introduced as the first phase of a larger truth-discovery extension:
it predicts a value, with an explicit score, and an explicit option to
abstain rather than guess. See `vignette("truth-fusion")`.

```r
claims <- attribute_claims_validate(long_format_claims)  # entity_id/attribute/source_id/claimed_value/reference_year
fusion_result <- attribute_truth_fusion(claims, method = "majority", abstain_threshold = 0.6)
fusion_result$predictions        # predicted_value, model_score, abstained, abstention_reason
fusion_result$candidate_scores   # every candidate value's vote share, ranked
attribute_truth_backends()       # which methods are actually implemented right now -- only "majority"
```

Votes are counted per *source*, not per row, so a source that
legitimately reports two values (two real practice locations, say)
votes for both rather than being forced to pick one. An exact tie
always abstains; `attribute_truth_fusion()` never silently substitutes
a different method or backend than the one requested, and never claims
a method is available before it is actually implemented.

## From protocol to function

| Protocol step | Function |
|---|---|
| Cross-link registries into mutually exclusive overlap zones | `registry_overlap()`, `overlap_zone_counts()`, `plot_registry_overlap()` |
| Restrict to the frame registry; define strata | `frame_strata()` |
| Stratified sample with preserved sampling probabilities | `stratified_listing_sample()` |
| Design weights for a call log sampled elsewhere (N / n from a population table) | `listing_weights()` |
| Dispositions to confirmed valid, confirmed invalid, unresolved, not attempted | `classify_listing_status()`, `ent_disposition_map()`, `listing_status_levels()` |
| Design-weighted validity, overall and by stratum; complete-case estimate; location-valid or workforce-valid outcome | `listing_validity_design()`, `validity_estimates()` |
| Extreme-case bounds for unresolved listings | `validity_sensitivity_bounds()` |
| Survey-weighted logistic regression | `validity_model()` |
| Study-flow counts | `listing_flow_counts()` |
| Cascade stages with both denominators (of attempted, of prior stage), weighted or not | `validation_cascade()` |
| Pairwise cross-source agreement on a shared attribute, no gold standard, freshness-tuned by each source's own date | `attribute_agreement()`, `attribute_agreement_summary()`, `record_age_years()`, `record_age_band()`, `classify_record_staleness()` |
| Empirical staleness (consecutive-year spell age) from a multi-year panel alone | `attribute_spell_age()` |
| Longitudinal lead-lag: which source moves first, adopts, or reverts, with an optional cross-sectional back-check | `attribute_lead_lag_matrix()` |
| Agreement stratified by empirical spell age / career tenure | `attribute_agreement_by_spell_age()` |
| Administrative enrollment arrival/departure timing around a panel move | `enrollment_move_lead_lag()` |
| Manuscript-ready paragraphs from the three lead-lag approaches above | `format_no_gold_standard_prose()` |
| N-way consensus tier (unanimous / majority / split) across 3+ sources at once | `attribute_consensus_tier()` |
| Does a consensus tier persist across years, or churn? | `attribute_consensus_tier_flow()` |
| Per-source ranking: which source does the comparison set converge toward? | `attribute_source_corroboration()` |
| Is exactly one source a lone dissenter (distinct from a tied split with no majority)? | `attribute_odd_one_out()`, `attribute_odd_one_out_rate()` |
| Agreement stratified by any covariate, not just spell age | `attribute_agreement_by_covariate()` |
| Wilson confidence interval on an agreement/concordance rate | `wilson_ci()`, `attribute_agreement_add_ci()` |
| Validate the long-format multi-source claim schema | `attribute_claims_validate()` |
| Predict a value by majority vote, with explicit abstention on ties/weak majorities | `attribute_truth_fusion()` |
| Which truth-fusion methods/backends are actually implemented right now | `attribute_truth_backends()` |

## Design rules

- **One listing is the unit.** Nothing here aggregates to the physician; do
  that before sampling with your own one-listing-per-NPI rule.
- **Presence is never inferred.** A reached office, a non-missing new-patient
  answer, or board certification does not make a listing valid; only the
  disposition categories in the map do.
- **Unresolved is explicit.** It is reported as its own category, and the
  bounds table shows what the estimate would be if every unresolved listing
  were valid, or invalid.
- **Counts must partition.** Every count-producing function asserts that its
  categories sum to the input, and registry totals must close on their zones.
- **Codebook drift stops the run.** A disposition string outside the map is an
  error unless you ask for `unmapped = "unresolved"`.
- **Attribute agreement is pairwise, never against an assumed truth.**
  `attribute_agreement()` compares every pair of sources that cover the same
  identifier-year; it never picks a "correct" value. A disagreement is read
  against each side's own freshness date only to say one side provably
  predates the other, not to say which is right.
- **`attribute_truth_fusion()` is the one deliberate exception to the
  rule above.** Every other function in this package stops at a
  relative claim; this one predicts a value, on purpose, with an
  explicit score and an explicit abstention path -- it does not pretend
  to follow the "never pick a value" rule it is the exception to.

## Disposition maps

`ent_disposition_map()` is the ENT study's REDCap vocabulary. A new study
supplies the same four categories with its own strings:

```r
obgyn_map <- list(
  at_location   = c("Yes, sees patients here", "Yes, but not accepting new patients"),
  relocated     = "Moved to another practice",
  retired       = c("Retired", "No longer practising"),
  not_specified = "No, reason not given")
classify_listing_status(call_log, map = obgyn_map)
```

## Related packages

- [`mysterynpi`](https://github.com/mufflyt/mysterynpi): name keys and
  agreement rules for linking person rosters to NPI (build the registries here
  first).
- [`mysterycall`](https://github.com/mufflyt/mysterycall): appointment-access
  outcomes, wait-time models, power, and study tooling.
- [`mysterymaps`](https://github.com/mufflyt/mysterymaps): maps for access studies.
- [`mysterymedicaid`](https://github.com/mufflyt/mysterymedicaid): Medicaid MCO
  provider-directory acquisition and audit; the source of the
  `record_age_years()`/`record_age_band()`/`classify_record_staleness()`
  freshness primitives this package ports and makes symmetric.

## Citation

Muffly TM. mysteryroster: registry overlap strata and listing-validity analysis
for mystery-caller audits of physician directories. R package version 0.3.0.
https://github.com/mufflyt/mysteryroster

## License

MIT, Tyler Muffly.
