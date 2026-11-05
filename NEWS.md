# mysteryroster (development version)

* Added three more relative-claim functions (and their pair/summary
  companions) to the same `R/consensus_corroboration.R` family:
  `attribute_odd_one_out()`/`attribute_odd_one_out_rate()` (is exactly
  one of 3+ sources a lone dissenter, distinct from a tied split with
  no majority -- `attribute_consensus_tier()` reports both the same way
  as `"split"`); `attribute_agreement_by_covariate()` (generalizes
  `attribute_agreement_by_spell_age()`'s stratification to any
  covariate, from any data frame, not just empirical spell-age bins);
  and `wilson_ci()`/`attribute_agreement_add_ci()` (a Wilson score
  confidence interval for an agreement rate, ported from the same
  midwifery-workforce-audit script's `wilson_ci()` helper -- handles
  small `n` and extreme rates better than a normal approximation, and
  defaults to the proportion scale to match
  `attribute_agreement_summary()`'s `pct_agree`, with an `as_percent`
  toggle for `attribute_consensus_tier()`-style tables instead).
  `vignette("consensus-and-corroboration")` extended with worked
  examples of all three. 297 tests passing; 0 errors / 0 warnings /
  0 notes on `R CMD check` with vignettes.
* Added `attribute_consensus_tier()` and `attribute_consensus_tier_flow()`,
  ported and generalized from a one-off midwifery-workforce-audit script
  (`build_cross_source_comparison.R`) that hardcoded its consensus rule to
  exactly four sources and a fixed 2021-2025 window. The generalized rule
  (unanimous when everyone agrees, majority when the largest group is a
  strict majority, split otherwise) reduces to that script's exact
  4-source behavior as a special case. Also added
  `attribute_source_corroboration()`, aggregating
  `attribute_lead_lag_matrix()` across every pair of 3+ sources into a
  per-source ranking of which one the comparison set tends to converge
  toward. New vignette, `consensus-and-corroboration`. Fixed a real bug
  found while writing its examples: `attribute_consensus_tier()`'s
  `interaction()` + `split()` grouping silently dropped every row when
  `year = NULL` (an all-`NA` grouping column has zero factor levels by
  default) -- switched to a plain pasted string key.
* New vignette, `attribute-agreement`. It covers `attribute_agreement()`,
  `attribute_agreement_summary()`, and the freshness functions
  `record_age_years()`, `record_age_band()`, and
  `classify_record_staleness()`, using four simulated sources. Three
  figures show agreement per source pair, which side of each disagreement
  is provably older, and agreement by the older record's age.
* `vignette("lead-lag-staleness")` gains a timeline figure for the
  three-NPI example. A new section runs all three approaches on a
  simulated 1,500-clinician panel and adds four figures: which source
  breaks concordance first, what happens to a disagreement one year
  later, agreement by spell age, and enrollment arrival and departure
  timing.
* The README shows all eight figures. `data-raw/no_gold_standard_figures.R`
  regenerates them in `man/figures/` from the vignette chunks.
* Rewrote the help for the ten no-gold-standard functions:
  - every output column of `attribute_lead_lag_matrix()` is defined, along
    with which counts are not exhaustive and each percentage's denominator;
  - the age bins of `attribute_agreement_by_spell_age()` and the interval
    matching rules of `enrollment_move_lead_lag()` are written out;
  - `attribute_agreement()` now explains how it pairs sources, normalizes
    values, and handles years and `as_of`;
  - `record_age_band()` notes that its bands are right-closed, and now
    correctly describes its result as an unordered factor;
  - each function has `@examples` and a shared "no-gold-standard
    comparison" `@family` cross-reference.
* `?format_no_gold_standard_prose` now lists the wording that is fixed to
  the OB/GYN midwifery study: study years, source-specific mechanisms, and
  interpretive verbs. It also lists the input rows it requires. Behaviour
  is unchanged.

# mysteryroster 0.3.0

* Added `@examples` to the 22 exported functions that had none -- only
  `registry_overlap()`, `attribute_spell_age()`, and `attribute_agreement()`
  carried a runnable example before this, so `?function` and a pkgdown
  reference page showed nothing for the other 22. Every example is
  executed (not just asserted) by `R CMD check`, including the two most
  elaborate ones: `validity_model()`'s survey-weighted logistic
  regression and `format_no_gold_standard_prose()`'s full three-paragraph
  manuscript-text generation.
* Fixed a real `R CMD check` NOTE in `attribute_spell_age()`: it called
  base `ave()` unqualified, which `R CMD check` flags as an undefined
  global function since the package never declares a bare `importFrom`
  for it. Qualified to `stats::ave()`, matching every other `stats`
  call already in this package (`stats::confint()`, `stats::update()`,
  `stats::setNames()`) rather than adding a one-off `importFrom`.
* Added a third vignette, `lead-lag-staleness`, covering
  `attribute_spell_age()`, `attribute_lead_lag_matrix()`,
  `attribute_agreement_by_spell_age()`, `enrollment_move_lead_lag()`,
  and `format_no_gold_standard_prose()` -- previously documented only
  in roxygen `@param`/`@return` and a 50-case adversarial test file,
  with no worked example anywhere of what these functions are actually
  for or what their output looks like end to end.
* Added `attribute_spell_age()`, `attribute_lead_lag_matrix()`,
  `attribute_agreement_by_spell_age()`, `enrollment_move_lead_lag()`, and
  `format_no_gold_standard_prose()`: three longitudinal approaches for
  determining which registry leads vs. lags when no gold-standard address
  database exists (1-year and multi-year lead-lag panel convergence,
  agreement stratified by empirical spell age / career tenure, and
  date-stamped enrollment interval arrival/departure timing around panel
  moves), plus programmatic manuscript paragraph generation for each
  approach.
* Added `attribute_agreement()`/`attribute_agreement_summary()`: pairwise
  cross-source agreement on a shared attribute (e.g. practice state), one
  step beyond `registry_overlap()`'s membership-only comparison. There is
  still no gold standard -- every pair of sources is compared only against
  each other, by identifier and year, never against an assumed truth.
* Ported `record_age_years()`/`record_age_band()`/
  `classify_record_staleness()` from isochrones' `e1_last_update_staleness.R`
  (via `mysterymedicaid::mm_record_staleness.R`), made symmetric for
  pairwise comparison: a disagreement is read against each side's own
  freshness date (last-updated or vintage/snapshot date, never download
  date) only to say one side provably predates the other, not to say which
  value is correct.
* Added the package's first vignette, `registry-overlap` -- there was
  previously no `vignettes/` directory, no `knitr`/`rmarkdown` in
  `Suggests`, and no `VignetteBuilder` at all. Walks through
  `registry_overlap()`, `overlap_zone_counts()`'s closure guarantee
  (including the negative control that triggers it), `frame_strata()`,
  and `plot_registry_overlap()`, then bridges to `attribute_agreement()`
  for readers who want to compare an attribute once identifiers are
  cross-linked.
* Added a second vignette, `listing-validity-study`, covering the 13
  exported functions `registry-overlap` doesn't touch -- the package's
  actual core purpose, previously demonstrated nowhere but the
  README's terse "six calls" code block: `stratified_listing_sample()`,
  `listing_weights()`, `classify_listing_status()` and its disposition
  maps, `listing_validity_design()`, `validity_estimates()`,
  `validity_sensitivity_bounds()`, `validity_model()`,
  `listing_flow_counts()`, and `validation_cascade()`, run end to end
  on one synthetic study with a reconciled call log.
