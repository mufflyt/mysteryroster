# mysteryroster (development version)

* The weighted methods flagged as unrun in the bullet below (dawid_skene,
  one_coin_dawid_skene, sums, average_log, investment, pooled_investment,
  truthfinder) were subsequently run by the `~/midwifery` session against
  the same expanded claims table (crowd_kit/truthdiscovery still aren't
  configured in this R session) and verified here directly from its output
  files, not taken on report. A sanity check reproduced both the 691 and
  759 majority baselines exactly from a from-scratch read of the handed-off
  file. All 7 weighted methods abstain 0% of the time. T-MSIS's learned
  reliability has no cross-method consensus: lowest-ranked of the 4 sources
  by dawid_skene (0.551) and one_coin_dawid_skene (0.693), mid-pack by
  average_log and truthfinder -- the same contradictory-reliability-ranking
  finding the original 3-source ablation established, now extended to a 4th
  source rather than resolved by adding one. Adding the 4th source also
  nearly triples dawid_skene/truthfinder's agreement on majority's ties
  (6.6% -> 19.1%) and drops dawid_skene's trust-NPPES share by about 18
  points, a more encouraging signal than the majority-only result alone
  suggested -- full detail in `docs/midwifery-evidence-ledger-results.md`
  §4.
* Added `docs/midwifery-evidence-ledger-results.md`: Steps 2-4 of the
  evidence-ledger expansion plan, actually run against real data (all
  aggregate counts, no person-level values) rather than left as a design
  spec. The exact frozen 1,806-case/3,556-NPI-year cohort was independently
  re-derived from `~/midwifery`'s own extraction scripts and verified to
  match the published ablation numbers (small drift noted and explained:
  the auxiliary DAC/PECOS/Open-Payments source files have been updated
  since the original computation). Real T-MSIS Medicaid enrollment claims
  were added (1,339 usable, 515 NPI-years gaining evidence that had none
  before) and found to agree with NPPES only ~50% of the time -- direct
  empirical support for not treating enrollment as a practice address.
  Adding T-MSIS *increases* majority-vote abstention on net (691 -> 759 of
  3,556), asymmetrically by transition: abstention roughly doubles for the
  2017-18 spike and decreases for 2023-24, corroborating -- via a completely
  independent method -- the `~/midwifery` session's own finding (from
  removing NPPES out of its full 219,536-entity-year panel) that the 2017-18
  spike is substantially NPPES-dependent while 2023-24 is not. Facility
  affiliations turned out to add zero claims to this specific cohort (its
  6 transitions never touch 2026, the source's only dated year) -- a
  year-range mismatch, not the uselessness the rescinded constraint 3
  originally assumed. Weighted methods (dawid_skene, truthfinder, etc.)
  remain unrun against the expanded table: crowd_kit/truthdiscovery aren't
  configured in this R session.
* Added `attribute_claims_collapse_lineage()`, generalizing the
  `dac_pecos_combined` merge rule from `docs/midwifery-ablation-findings.md`
  (agree -> one vote; disagree -> no vote; one present -> use it) from one
  hardcoded pair to any declared group of same-lineage `source_id`s, so a
  future lineage-sharing source doesn't need the rule re-implemented ad hoc.
  Added alongside `docs/midwifery-evidence-ledger-design.md`, a design spec
  and source-by-source verdict table (grounded in an audit of
  `~/midwifery`'s existing extraction infrastructure) for expanding the
  midwifery evidence ledger beyond the original three sources -- CMS
  T-MSIS Medicaid enrollment segments are the one clearly new,
  independently-lineaged, properly-dated candidate found; hospital facility
  affiliations are addable only under a separate attribute, never as a
  practice-address vote. No real data has been loaded under this yet: that
  requires reading `~/midwifery`'s artifacts directly, on hold while a
  separate, live, in-progress build in that repository is running.
* The hospital-facility-affiliation exclusion above (constraint 3) was
  rescinded by the user immediately after: a CCN is a real physical
  facility with a real state, so `docs/midwifery-evidence-ledger-design.md`
  now calls for loading it directly as `practice_state` evidence (dated to
  its one snapshot year, never extrapolated), the same single-snapshot
  treatment already given to Healthgrades/Doximity/state boards. The
  `~/midwifery` build reached the identical call independently and is
  wiring the same source in as its 5th truth-fusion source.
* Strengthened `vignette("truth-fusion-case-study-midwifery")` with a
  second real example (NPPES, a combined DAC/PECOS source, and CMS Open
  Payments -- public transparency data by design, so real NPIs are shown,
  not placeholders) drawn from an unrelated `midwifery` project prototype
  (`diagnose_truth_discovery_migration_states.R`) that independently
  wrote its own one-coin Dawid-Skene EM to solve the same problem this
  package already implements, for a cohort of 1,806 NPI-years where
  NPPES itself reported a state change. A first pass through this
  example overclaimed what it showed; corrected after actually reading
  the cohort-construction code (full account in the new
  `docs/midwifery-ablation-findings.md`): these are NPPES-anchored
  apparent relocations, not independently-established moves, and the
  `confirmed` flag used as "external validation" turned out to be built
  entirely from NPPES's own adjacent years -- not independent of the
  source being validated. The corrected, more rigorous finding that
  replaced it: `dawid_skene` and `truthfinder`, fit on identical real
  claims, resolve the same 695 majority-tied NPI-years but agree on the
  resolved value only 6.6% of the time, and `dawid_skene`'s resolutions
  turn out to be 97.6% attributable to simply trusting NPPES over one
  disagreeing source (confirmed by removing NPPES and refitting: only
  7.2% of its resolutions survive, versus 92.5% surviving removal of
  DAC/PECOS instead). `truthdiscovery`'s other four methods each learn a
  different reliability ranking for the same three sources, not
  clustering by backend family. DAC and PECOS, despite a documented
  shared upstream dependency, still disagree 26.6% of the time both are
  observed -- the one finding from this example that held up unchanged --
  the same "shared upstream feed" phenomenon as the Crowd-Kit vignette's
  copied-source example, independently handled in unrelated real code by
  excluding disagreements rather than picking one, arriving at this
  package's own `on_source_conflict = "single_value"` policy.
* Added `docs/validation-protocol.md`, a design specification for a
  held-out benchmark comparing `attribute_truth_fusion()`'s methods
  against real, verified physician-location data: two separate schemas
  (evidence claims vs. a verified reference table, never unioned),
  stable/relocation/discordant cohort eligibility rules, a physician-level
  (never physician-year-level) train/test partition to prevent
  source-reliability leakage, a temporal-holdout alternative, primary
  outcome (city-state classification) and secondary outcomes (geographic
  distance, abstention, coverage, discordant-record accuracy), and paired
  comparison against majority voting. A search of the isochrones/
  midwifery research ecosystem for existing ground-truth data found none
  that qualifies (an AMCB-NPI linkage panel is a real, dated, but
  uncorroborated administrative crosswalk, not phone/board-verified; an
  isochrones test file's `GOLD_STANDARD_NPIS` is an explicit, unusable
  placeholder) -- documented in the protocol, which holds Phase 4 until a
  genuinely independent reference standard is identified. No benchmark
  accuracy claim is made anywhere in this release.
* Added a new vignette, `truth-fusion-case-study-midwifery`, running
  Phase 1/2/3 together on a real (de-identified, 50-row sample)
  excerpt of the midwifery project's AMCB-NPI linkage panel -- two NPPES
  practice-state extractions nine years apart, 10.75% of 10,507 shared
  certificants disagreeing between them. Demonstrates a real structural
  limitation of exactly-two-source majority voting (it can only ever
  agree or tie, never actually resolve a disagreement) and a real caveat
  for the probabilistic backends (both "sources" here are the same
  underlying registry at different times, so a confident Dawid-Skene/
  TruthFinder score reflects learned agreement patterns, not evidence of
  which snapshot is actually current) -- then shows that this particular
  disagreement is a staleness problem, not a reliability problem, and
  that `classify_record_staleness()` (already in this package, Phase
  0-era) answers it more simply and more honestly than any truth-fusion
  backend. No claim is made about which snapshot is actually correct for
  any certificant; see `docs/validation-protocol.md` for why.
* Added Phase 3 of the truth-discovery extension: `attribute_truth_fusion()`
  now implements `method = "sums"`, `"average_log"`, `"investment"`,
  `"pooled_investment"`, and `"truthfinder"` via a second *optional* Python
  backend, `backend = "truthdiscovery"` (`joesingo/truthdiscovery`). Same
  safety contract as `crowd_kit`: never required, never auto-installed,
  never silently substituted for `"majority"` when unavailable.
  `truthdiscovery` is **GPL-3.0** licensed (unlike `crowd-kit`'s
  Apache-2.0) -- mysteryroster (MIT) calls it at arm's length via
  `reticulate` and vendors none of its source; see `vignette
  ("truth-fusion-truthdiscovery")`'s licensing note. Its last release also
  predates Python's removal of `distutils`, so it currently needs a
  separate, older (< 3.12) Python environment from `crowd_kit` --
  `attribute_truth_backends()` reports this as the exact `status` reason
  when unavailable, not just a generic "not found." Belief scores from
  this backend are `score_type = "uncalibrated_belief"`, never
  `"posterior_probability"` -- verified directly against the installed
  package that, unlike `crowd-kit`'s `probas_`, a task's belief scores do
  not sum to 1. `implication_function` (TruthFinder's claim-implication
  mechanism, e.g. for geographic similarity) is wired through to
  `truthdiscovery`'s `Dataset`, evaluated and tested, but left `NULL`
  (disabled) by default and not shipped with any built-in geographic
  implication -- per the project's own phase plan, ordinary TruthFinder is
  evaluated on its own before any geographic extension. A real limitation
  found and documented rather than hidden: with this package's shared
  default `max_iterations = 100`, `"pooled_investment"` genuinely fails to
  converge on a 16-task simulated fixture (`truthdiscovery` raises rather
  than returning a partial result, so every prediction abstains with the
  new `"model_did_not_converge"` reason) -- it needs 109 iterations on
  that data; always check `model_diagnostics$converged`. The
  `crowd_kit`/`truthdiscovery` backends' shared task-building,
  single-value-conflict exclusion, and worker-task-connectivity logic was
  refactored out of `R/truth_fusion_crowdkit.R` into
  `R/truth_fusion_backends_shared.R` rather than duplicated. New vignette,
  `truth-fusion-truthdiscovery`, demonstrates all five methods, the
  uncalibrated-belief property, the convergence limitation, and a
  cross-language equivalence check (calling `truthdiscovery` directly via
  `reticulate`, bypassing this package's wrapper, and confirming matching
  output) is part of the test suite. 414 tests passing when
  `truthdiscovery` is configured (386 passing, 19 skipping cleanly without
  any Python backend); 0 errors / 0 warnings / 0 notes on `R CMD check`
  with vignettes in all three states (no Python, `crowd_kit` only,
  `truthdiscovery` only -- the two Python backends currently cannot share
  one Python environment). Geographic implications remain disabled by
  default; a held-out validation dataset comparing any of these methods
  against `"majority"` on real data is explicitly not yet built.
* Added Phase 2 of the truth-discovery extension: `attribute_truth_fusion()`
  now implements `method = "dawid_skene"` and `"one_coin_dawid_skene"`, an
  *optional* Python backend (`backend = "crowd_kit"`) via `reticulate`.
  `reticulate` moves from nothing to `Suggests`; this package still never
  requires Python to be installed, never installs Python or any Python
  package itself, and never modifies a user's global Python environment --
  `attribute_truth_backends()` now actually probes whether a user's own
  configured Python has `crowd-kit` installed (`"r_native"`/`"majority"`
  remains always available, with no dependency), and
  `attribute_truth_fusion()` errors with that exact reason rather than
  silently falling back to `"majority"` when it isn't. Unlike `"majority"`,
  which votes independently within each entity-year, these methods fit
  *once*, jointly, across every entity-year for one attribute, estimating
  one confusion matrix (`"dawid_skene"`) or one scalar skill
  (`"one_coin_dawid_skene"`) per source -- `source_diagnostics` is now
  populated for them (empty for `"majority"`, as before).
  `on_source_conflict = "multi_value"` is rejected for this backend
  (Dawid-Skene assumes one label per worker-task). `model_diagnostics` gains
  `backend_version` and `n_disconnected_components` (both `NA` for
  `"majority"`); a disconnected worker-task graph or a fit that used every
  one of `max_iterations` without converging now triggers an explicit
  `warning()`, never a silent "it worked." New vignette,
  `truth-fusion-crowdkit`, demonstrates all of this on a simulated
  physician active/inactive status panel, including two concrete ways this
  backend converges confidently on a *wrong* answer: a source's rare error
  coinciding with another source's permanent bias, and two sources that
  share an upstream feed (literal duplicates), which Dawid-Skene's
  independence assumption cannot detect from the claims table alone.
  Reproducibility and the model's own confusion-matrix output were checked
  directly against the installed `crowd-kit` Python package (not merely
  asserted); a real gap found along the way -- `crowd-kit`'s sparse
  confusion-matrix output omits a (worker, label) row entirely for a label
  that worker never reports at all, which would have let a constant-output,
  zero-information source look reliable by only ever being scored on the
  one diagonal cell it happened to have -- is fixed by completing the full
  source x true-label x observed-label grid and filling genuinely-absent
  combinations with an exact `0`, never leaving the gap or an `NA`. 405
  tests passing when `crowd-kit` is configured (378 passing, 10 skipping
  cleanly without it); 0 errors / 0 warnings / 0 notes on `R CMD check` with
  vignettes in both states. Phase 3 (`truthdiscovery`'s TruthFinder and
  friends), geographic implications, and held-out validation against real
  data are explicitly out of scope for this phase.
* Hardened `attribute_truth_fusion()` before starting any Phase 2 work:
  added an explicit `on_source_conflict = c("single_value", "multi_value")`
  policy (default `"single_value"`) governing a source that claims more
  than one distinct value for the same entity-attribute-year. Previously
  such a source always voted for every value it claimed, which let one
  source cast competing votes in an ordinary categorical majority vote;
  under the new default, a conflicted source's votes are excluded
  entirely (reported via a new `n_sources_excluded` column, and a new
  `"no_single_value_sources"` abstention reason if every source for an
  entity-year is excluded) until the conflict is adjudicated elsewhere.
  `"multi_value"` preserves the original behavior for cases where more
  than one value can legitimately be true at once, with `model_score`
  documented as a share of votes cast rather than a probability that one
  categorical value is correct in that mode. Also replaced
  `attribute_claims_validate()`'s and `attribute_truth_fusion()`'s
  internal grouping, which pasted identifiers together with no
  separator (a latent collision risk, e.g. entity `"1"`/year `23` and
  entity `"12"`/year `3` both pasting to `"123"`), with `split()` on the
  actual columns; added a regression test with exactly that collision
  shape, plus tests for multiple attributes per entity-year, duplicate
  rows, and the new conflict policy. 369 tests passing; 0 errors / 0
  warnings / 0 notes on `R CMD check` with vignettes.
* Added `attribute_claims_validate()`, `attribute_truth_fusion()`,
  `attribute_truth_backends()`, and a `print.mysteryroster_truth()`
  method -- Phase 1 of a larger truth-discovery extension (see
  `vignette("truth-fusion")`). Every other function in this package's
  no-gold-standard family deliberately never names a value as correct;
  `attribute_truth_fusion()` is the first, explicit exception --
  majority-vote prediction with abstention on ties or (via
  `abstain_threshold`) weak majorities. Votes are counted per source,
  not per row, so a source reporting two legitimate values (e.g. two
  real practice locations) votes for both rather than being forced to
  pick one. `method`/`backend` are validated against a fixed,
  currently-implemented set (`attribute_truth_backends()` reports it
  honestly: only `"majority"`, entirely in R) rather than silently
  substituting a different algorithm than the one requested. Later
  phases (not implemented here) are expected to add probabilistic
  backends -- `crowd-kit`'s Dawid-Skene, `truthdiscovery`'s TruthFinder
  and friends -- via optional Python integration; this phase adds no
  Python dependency at all. 336 tests passing; 0 errors / 0 warnings /
  0 notes on `R CMD check` with vignettes.
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
