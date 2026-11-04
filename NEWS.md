# mysteryroster 0.3.0

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
