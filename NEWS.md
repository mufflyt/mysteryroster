# mysteryroster (development version)

* Added `attribute_set_agreement()`, a set-based sibling to `attribute_agreement()` for
  attributes where a source can legitimately report more than one value for the same
  identifier-year -- for example a physician with two practice locations from the same
  registry in the same year. `attribute_agreement()` merges on `(id, year)` alone, so a
  source with 2+ rows for one identifier-year silently produces an uncontrolled cartesian
  product rather than a comparison; `attribute_set_agreement()` instead compares the full
  *set* of values each source reports, returning both `any_shared` (OIG's own looser
  directory-audit standard -- the two sources overlap on at least one value) and
  `same_set` (a stricter extension -- the sets are exactly equal) rather than picking one
  standard silently. First piece of a staged integration of OIG-style physician-directory
  audit methodology into the package; population separation, record-presence tracking,
  enrollment-to-location linkage, and a per-claim verification questionnaire with
  structured nonresponse are follow-on pieces, not yet landed.
* Rewrote `?mysteryroster` (the package-level help page) and the top of `README.md`:
  both previously described only the original listing-validity protocol (~16 functions),
  leaving the entire no-gold-standard comparison and truth-fusion side of the package
  (~30 functions, roughly two-thirds of it by count, and this session's entire focus)
  completely unmentioned. A reader typing `?mysteryroster` or skimming the README's
  opening got no signal that capability existed at all. `?mysteryroster` now orients to
  both workflows explicitly, with the second broken into its four natural sub-workflows
  (single cross-section, longitudinal pairwise, N-way consensus, truth fusion) and links
  to every relevant vignette; the README's opening does the same, with a skip-link into
  the body section where the second workflow is actually covered (which was already
  thorough, ~230 lines -- the gap was specifically in the first-five-seconds framing, not
  the content). Every function referenced in the new package doc verified against the
  actual exported-function list before publishing, to avoid broken cross-references.
* `attribute_catch_up_curve()` and `attribute_lead_lag_duration()` now return a classed
  result (`mysteryroster_catch_up_curve`, `mysteryroster_lead_lag_duration`) with its own
  `print()` method, matching the existing convention `attribute_truth_fusion()` already
  had. Typing a catch-up-curve result at the console previously dumped seven raw list
  elements, several of them multi-row tibbles; a duration result dumped every field of two
  deeply nested nested summary lists, one value per line, 40+ lines for what reads as two
  sentences. Both now print a compact summary (median years to catch up with its
  confidence interval and risk-set size, ranked fastest-first, for `attribute_catch_up_
  curve()`; the two adoption/reversion sentences for `attribute_lead_lag_duration()`) while
  every list element (`$curve`, `$median_horizon`, `$b_adopts`, etc.) remains fully
  accessible exactly as before -- the class only changes what happens when the object is
  printed, not what `$` access returns.
* Added `attribute_lead_lag_duration()` to `vignette("lead-lag-staleness")` as a new
  "Approach 4," immediately before the catch-up-curve section it precedes conceptually.
  Like `attribute_catch_up_curve()` before it, this function had no narrative
  documentation at all despite being exported and referenced by name in that other
  function's own docs. The new section uses a dedicated 3-identifier example (not the
  minimal 1-identifier one in the function's own `@examples`) specifically chosen to make
  its real limitation visible in the rendered output: `disc_base_n` is 3, but `b_adopts$n`
  is 2, with the median computed from `years = c(1, 3)` alone -- the identifier that never
  resolves is not in that vector, and nothing in the sentence marks its absence. This is
  exactly the gap `attribute_catch_up_curve()`'s censoring-aware estimate exists to close,
  which the vignette can now show side by side instead of asserting in prose.
* Added a confidence interval to `attribute_catch_up_curve()`'s `median_horizon`:
  `median_ci_low`/`median_ci_high` (the Brookmeyer-Crowley interval, via the log-log
  transform of Greenwood's formula, the same method `survival::survfit()` uses by
  default) and `median_n_at_risk` (the risk-set size at the horizon where the point
  estimate crosses 50%). `curve` itself also gains a pointwise `ci_low`/`ci_high` at
  every horizon. Found while demonstrating `attribute_catch_up_curve_prose()`'s new
  clinical register on real data: the Medicaid enrollment file's reported median of 16
  years sits on a risk set of just 49 events, hovering within one point of the 50%
  threshold from horizon 13 through 15 before a single additional event tips it to 16 --
  a one-case difference that would have reported 13 instead. The number was not wrong,
  but reporting it bare, with no interval, hid how little it should be trusted; the
  correct interval is (12, not estimable). Verified both against an independent hand
  calculation and against `survival::survfit()` fed the equivalent left-truncated,
  right-censored individual-level data, matching to the decimal at every horizon tested
  and for the median and its interval. `attribute_catch_up_curve_prose()` now reports
  this alongside each resolved source's median in both registers, rather than a bare
  point estimate.
* Added `register = c("technical", "clinical")` to `format_ranking_prose()` and
  `attribute_catch_up_curve_prose()`. `"clinical"` produces plain-language narrative
  sentences with no abbreviations and no em dashes ("Among the 2 groups compared, the
  lowest median years to confirmation was observed for..." instead of "Ranked by median
  years to catch up, lowest to highest: ..."), intended for a clinical-journal Methods/
  Results section rather than a technical report. As with the existing `display_label`
  parameter, the caller supplies audience-appropriate group names (e.g. "the national
  provider directory" rather than "nppes"); `register` controls sentence structure, not
  domain vocabulary the package has no way to know. `table`/`highest`/`lowest`/`ties` are
  unaffected by `register`, only `sentence` changes. Default remains `"technical"`,
  preserving all prior behavior exactly.
* Fixed a real bug surfaced while building the above: `attribute_catch_up_curve_prose()`'s
  `n_events` was computed as the largest single source's own `n_at_risk` at `horizon = 0`,
  which only equals the true pooled total under `reference = "pooled"`. Under
  `reference = "exclude_self"`, each source's own risk set is already reduced by its
  unanchored and uncovered exclusions, so that maximum understates the real count (seen on
  real data: 2,245 reported instead of the true 3,029). Fixed to use `attribute_catch_up_
  curve()`'s own `n_tracked` field directly.
* Added `lineage_groups` to `attribute_catch_up_curve()`'s `reference = "exclude_self"`
  mode (same convention as `attribute_claims_collapse_lineage()`'s own `lineage_groups`):
  two sources derived from the same underlying file cannot independently confirm each
  other's events just because their file names differ. Excluding "self" now excludes a
  source's whole named lineage group. On the real midwifery data, grouping the facility-
  affiliation extract with the DAC/PECOS source it is literally derived from (both come
  from the same underlying CMS Doctors & Clinicians file) raised facility affiliation's
  unconfirmed-event count from 18 to 1,533 of 3,029 (51%) -- its apparent independent
  confirmation had been almost entirely circular. Medians were unaffected (the events that
  remain genuinely anchored resolve at the same speed), but confidence in which events are
  anchored at all drops sharply once shared provenance is accounted for.
  Also added a synthetic regression test for a clinician with two separate round trips
  through the same two values (CO -> TX -> CO -> TX): confirms, explicitly rather than by
  inference, that only 2 distinct events are tracked (not the 3 genuine transitions that
  occurred) -- the documented "repeated arrival" collapse limitation, now with a test
  naming the exact scenario rather than only a simpler one-value case.
* Fixed a third real validity problem in `attribute_catch_up_curve()`, found while
  investigating why Open Payments' `exclude_self` median catch-up (13 years) was still
  implausibly high after the censoring-window and circularity fixes below: a source's own
  observation *window* (when the source as a whole was operating) says nothing about
  whether a *given identifier* ever appears in it at all. An identifier a source has zero
  records for, in any year, cannot show any value for it -- this is structural
  non-coverage, not "watched and still pending" -- yet it was being kept in that source's
  risk set for the source's entire operating window regardless, dragging the curve down
  for a reason with nothing to do with update speed. On the real data this was the
  dominant effect, far larger than the window fix: facility affiliation never observes 69%
  of its tracked identifiers at all (2,104 of 3,029), Open Payments 44% (1,322), T-MSIS 22%
  (653). Fixed: such identifiers are now excluded from a source's risk set entirely
  (reported via the new `n_no_coverage`, the same way `n_unanchored` already reported
  circularity exclusions). Effect on the real data: Open Payments' median fell further,
  10 -> 4 years (`pooled`) and 13 -> 5 years (`exclude_self`); T-MSIS and facility
  affiliation, which previously never crossed 50% catch-up at all, now resolve at medians
  of 13 and 5 years respectively once the identifiers they structurally cannot observe
  stop being counted against them. One new regression test; all prior tests for this
  function still pass unchanged.
* Fixed two real validity problems in `attribute_catch_up_curve()`, found by running it
  against a real five-source midwifery comparison rather than only hand-built test
  fixtures:
  (1) Censoring used the *panel-wide* latest/earliest year for every source, conflating
  "this source was watched and hadn't shown the value yet" (real loss to follow-up) with
  "this source's own data collection hadn't started or had already ended" (structural
  non-coverage). On the real data this inflated a sparse source's apparent median
  catch-up time with years it was never able to observe anything in at all -- concretely,
  Open Payments' reported median dropped from a non-sensical 16 years to 10 once censoring
  used that source's own 2015-2025 observation window instead of the full panel's
  2007-2026 span. Fixed: a source's risk set now enters (is left-truncated) at that
  source's own earliest observed year and is censored at its own latest, never a different
  source's.
  (2) An event's reference time was pooled across every source's own detected arrivals,
  so the source that happened to detect an event earliest got credited with "catching up"
  to it immediately -- not because it is actually fast, but because it supplied the
  timestamp it is being measured against. On the real data this was severe: 66-85% of the
  events a source resolved at `horizon = 0` were ones that source itself had helped define.
  Added `reference = "exclude_self"` (alongside the existing `"pooled"` default): scores
  each source only against events *other* sources independently detected, dropping (and
  reporting via the new `n_unanchored`) any event no other source confirms. Running both
  modes on the real data: NPPES's median rose from 0 to 1 year and DAC/PECOS's from 0 to 2
  once self-detected events no longer counted in their own favor; nearly half of
  DAC/PECOS's tracked arrivals (1,485 of 3,029) turned out to have no independent
  confirmation from any other source at all.
  Separately checked and found negligible in the real data (0 of several thousand
  multi-value identifiers across all five sources): a value an identifier returns to a
  second time still collapses into its first-ever arrival, a known, documented limitation
  left as-is since fixing it would not have changed any real number. Six new regression
  tests cover both fixes with hand-traced examples; all 22 pre-existing tests for this
  function still pass unchanged.
* Added `attribute_catch_up_curve_prose()`: manuscript-ready prose for an
  `attribute_catch_up_curve()` result, ranking sources whose curve crosses 50% via
  `format_ranking_prose()` and writing a separate, honest sentence (percent still not
  reflected, immediately and at a chosen horizon) for any source whose curve never does,
  rather than forcing every source through the same ranking or fabricating an unbounded
  catch-up time.
* Added `attribute_catch_up_curve()`: a proper right-censored Kaplan-Meier
  survival estimate of how long each source takes to show a value another
  source already has, across 2+ sources at once. Ported and generalized
  from a `tyler.muffly`/Devin-AI co-authored exploratory analysis
  (`vignette("lead-lag-staleness")`'s "How many years ahead or behind?"
  section, commit `5892254` on branch
  `devin/1793847201-lead-lag-years-ahead`) into a tested, documented,
  reusable function. Fixes a real limitation `attribute_lead_lag_duration()`
  has: its median/IQR is computed only over identifiers that *eventually*
  resolve within the observed panel, so one that never catches up simply
  disappears from the summary, understating how long a slow source
  actually takes. `attribute_catch_up_curve()` instead treats an
  unresolved identifier as correctly "at risk" for as long as its own
  follow-up window lasts, then censors it out once that window closes --
  never silently dropped, never miscounted as "never". `median_horizon`
  is `NA`, not a fabricated number, when a source's curve never crosses
  50% within the observed horizon. Composes with `format_ranking_prose()`
  to rank sources by median catch-up time. Verified against two
  hand-traced Kaplan-Meier calculations, including a dedicated censoring
  test confirming an event near the panel's end correctly drops out of
  the risk set rather than permanently dragging a source's curve down.
* Fixed a real bug in `attribute_catch_up_curve()`: whether a source had
  "shown" a tracked value was decided using the same strict consecutive-year
  rule used to *detect* arrivals in the first place. That rule is right for
  discovering genuine arrivals, but wrong for deciding whether another
  source has caught up -- it silently discarded a real match whenever that
  source's own prior record for the identifier was more than one year
  earlier, which is the normal case for any source sampled less often than
  annually (enrollment- or claims-derived data with multi-year gaps between
  records). Found by running the function against real, full midwifery
  cohort data across five genuinely heterogeneous sources (NPPES,
  DAC/PECOS, T-MSIS, Open Payments, CCN facility affiliation): three of the
  five appeared to "never catch up" at ~95-99.5% unresolved, which turned
  out to be the bug discarding 80-95% of each source's real matches, not a
  finding about those sources. A source is now credited as soon as *any* of
  its own records carries the matching value at or after the event's
  pooled start year, regardless of the gap to its prior record; a match
  that occurred only *before* the event started still correctly does not
  count, and is retained in the risk set (not silently dropped) rather than
  miscounted as resolved. Two new regression tests cover both corrected
  behaviors.
* Ran 20 more adversarial saboteurs (Part E of `docs/saboteur-test-matrix.md`)
  against the prose generators, covering different attack vectors than Part
  D plus `attribute_churn_rate()`/`attribute_coverage()` (built this
  session, not previously saboteur-tested). Found and fixed three real
  bugs, each now a permanent `[red-team]` regression test: (1)
  `format_ranking_prose()` ranked and printed a literal `Inf` if handed one
  (e.g. from an upstream division-by-zero) -- `value` now requires
  `finite = TRUE`; (2) `fmt = "%s"` let a value's full floating-point
  precision leak into a sentence uncontrolled -- `fmt` is now restricted to
  floating-point-safe sprintf conversions (`f`/`e`/`E`/`g`/`G`/`a`/`A`;
  `%d`-family is deliberately excluded too, since R's `sprintf()` requires
  those to receive a true integer and errors cryptically on a fractional
  `value`); (3) `attribute_truth_fusion_prose()` compared two fits'
  abstention rates with no disclosure when `result` and `compare_to` used
  *different* fusion methods (whose abstention criteria aren't the same
  quantity) -- the comparison sentence now names both methods whenever they
  differ. While fixing (2), a fourth, self-referential bug was caught in
  the fix's own error message: passing several separate string literals to
  one `sprintf()` call doesn't concatenate them, silently producing a
  garbled message -- fixed with `paste0()` before the single substitution,
  in the same pass, not shipped. Seventeen of the twenty saboteurs were
  already correctly defended, including the numeric-looking-group-name
  ranking order (driven by value, never a string sort) and the
  zero-overlap-universe coverage case (a real, honest 0% with a wide CI,
  not a bug).
* Added `format_ranking_prose()`, the N-way counterpart to
  `format_proportion_comparison_prose()`'s one-or-two-group case: given a
  value per named group (coverage, lead rate, odd-one-out rate, learned
  reliability -- anything), returns a ranked table and a manuscript sentence,
  with same-displayed-value ties explicitly flagged rather than presented as
  a false strict order. `attribute_truth_fusion_prose()`'s reliability
  ranking now uses it internally instead of duplicating the same logic.
  Added `vignette("source-comparison-midwifery")`, which runs all eight
  no-gold-standard comparison dimensions this package now supports --
  timing, agreement (pooled and covariate-stratified), staleness, churn,
  coverage, convergence, independence, and weight -- end to end on one
  simulated, midwifery-calibrated panel, demonstrating every prose
  generator together rather than one function at a time.
* Ran 20 new adversarial saboteurs (Part D of
  `docs/saboteur-test-matrix.md`) against `format_proportion_comparison_prose()`,
  `format_ranking_prose()`, `attribute_lead_lag_duration()`, and
  `attribute_truth_fusion_prose()` for grammatical and statistical
  correctness -- not just unit-tested in isolation, each one actually run
  against the real functions before any fix. Found and fixed five real
  bugs, each now a permanent `[red-team]` regression test: (1) `n` counts
  in a ranking sentence could render with stray padding (`"n =     0"`)
  from `format()`'s default width-alignment; (2) a degenerate two-proportion
  test (e.g. 0/1 vs. 0/1) correctly marked its p-value "not computable" but
  still rendered a nonsensical `"95% CI -0.0 to -0.0%"` from the same
  untrustworthy fit; (3) `conf_level = 1` propagated a literal `NaN` into
  rendered sentence text via `wilson_ci()`'s own `qnorm(1) = Inf` -- fixed
  at the root in `wilson_ci()` itself (an existing, widely-used function),
  so every caller is protected, not just the new prose functions;
  (4) a whitespace-only label (`"   "`) passed `checkmate::assert_string(x,
  min.chars = 1)` and produced a visibly broken sentence ("In   , 10
  of..."); (5) a source with indeterminate reliability (e.g. isolated in a
  disconnected graph component) used to vanish from a reliability ranking
  with no trace, rather than being disclosed as excluded.
* Added `attribute_churn_rate()` and `attribute_coverage()`: two more
  dimensions for comparing sources, distinct from timing and agreement.
  `attribute_churn_rate()` measures how often a single source's own value
  changes year to year, independent of whether the change is "right" --
  overall, by transition year, and by identifier (with a 0/1/2/3+
  change-count distribution, so a source whose churn is driven by a
  handful of volatile identifiers is distinguishable from one that churns
  evenly). Its `overall$n_changed`/`overall$n_transitions` feed directly
  into `format_proportion_comparison_prose()` for a two-source comparison
  with a p-value -- no separate wrapper needed. `attribute_coverage()`
  measures which source actually has a non-missing value for more
  entity-years, against an explicit (or, with a documented caveat,
  inferred) universe -- ranked across any number of sources at once, with
  a by-year breakdown. Both follow this package's existing conventions
  (Wilson CIs throughout, "presence is never inferred" -- a missing value
  is never counted as coverage, named-list-of-sources shape matching
  `attribute_consensus_tier()`/`attribute_source_corroboration()`).
* Added `attribute_lead_lag_duration()`: extends
  `attribute_lead_lag_matrix()`'s multi-year convergence tracking (which only
  answers *whether* a source eventually adopts the other's disputed value)
  with *how long* that takes -- the earliest later year each identifier's
  adoption/reversion first occurs, summarized as n/median/IQR/mean/SD/CI per
  direction, plus a manuscript sentence for each (`"NPPES adopted CMS DAC's
  2020 value after a median of 2.0 years (IQR 1.0-3.0; mean 2.1, 95% CI
  1.7-2.5 years)"`). Deliberately reports years, not months: this package's
  longitudinal sources are annual snapshots, so years is the finest unit the
  data can honestly support -- a month-level lag between two sources that
  both carry real calendar dates (not just annual snapshots) is a different,
  separate kind of comparison, out of scope here and documented as such.
* Reworked `format_proportion_comparison_prose()`'s sentences (and
  `attribute_truth_fusion_prose()`'s, which builds on it) to be clearer and
  more complete, per explicit feedback on the first version below. Two
  changes: (1) `quantity_label` (one free-text phrase) is replaced by
  `unit_label` (a plural noun, e.g. `"entity-years"`) and `condition_label`
  (a verb phrase, e.g. `"were abstained"`), because a single label read as a
  sentence fragment whenever it was phrased as a noun rather than a
  predicate -- the old `"this cohort: 204 of 1,806 NPI-years with any
  external corroboration (11.3%...)"` had no verb; splitting the two always
  produces a complete sentence (`"In this cohort, 204 of 1,806 NPI-years had
  any external corroboration (11.3%...)."`). (2) The comparison sentence now
  reports the difference in percentage points with its own 95% CI (signed
  consistently, from the same `prop.test()` call, at no extra computation
  cost) and the full chi-squared statistic with degrees of freedom and total
  N (`"chi-squared(1, N = 7,112) = 3.89, p = 0.049"`), not just a bare
  p-value -- the standard form a manuscript expects. Caught a real bug while
  rewriting the template (one extra, unused `%s` placeholder caused
  `sprintf()` to error on every two-group call) before it shipped.
  `attribute_truth_fusion_prose()`'s reliability sentence now lists the full
  ranking of every source, not just the highest and lowest. This is a
  breaking change to `format_proportion_comparison_prose()`'s signature
  (`quantity_label` no longer exists); the function was added in the
  previous entry below and has no outside callers yet.
* Added `format_proportion_comparison_prose()` and
  `attribute_truth_fusion_prose()`: manuscript-ready sentences (count, %,
  Wilson CI, and -- for two proportions -- a two-proportion test with a
  p-value) generated directly from the numbers, instead of hand-writing
  `sprintf()`/`prop.test()` the way every case-study doc in this package so
  far has. `format_proportion_comparison_prose()` works on any count/total
  pair (abstention rate, corroboration rate, agreement rate, anything);
  `attribute_truth_fusion_prose()` is a thin wrapper that pulls the
  abstention count and, when present, a reliability-ranking sentence
  directly from one or two `attribute_truth_fusion()` results. Verified
  against this package's own already-published numbers before shipping:
  reproduces `docs/midwifery-ablation-findings.md`'s 204/1,806 (11.3%, CI
  9.9-12.8%) and `docs/midwifery-evidence-ledger-results.md`'s 691 vs. 759
  of 3,556 comparison exactly.
* Added `man/figures/truth-discovery-abstention-vs-reliability.png` -- a
  conceptual diagram contrasting abstention (a per-entity-year decision)
  with source reliability (a per-source property learned across many
  records), including the warning that dependent sources can agree with
  each other and still be wrong together -- to README.md's "Predicting a
  value: truth fusion" section.
* Added two base-R figures to `vignette("truth-fusion-evidence-ledger-cross-check")`:
  a grouped bar chart of majority-vote abstention by transition year (baseline vs.
  +T-MSIS), visually showing the asymmetry between the 2018 and 2024 spike years, and
  a grouped bar chart of each weighted method's learned reliability for all four
  sources, visually showing T-MSIS has no cross-method consensus (lowest-ranked under
  the two Dawid-Skene variants, mid-pack and barely distinguished under
  average_log/truthfinder). Same base-R/no-ggplot2-dependency convention as this
  package's other vignettes' figures.
* Added `vignette("truth-fusion-evidence-ledger-cross-check")`, packaging the
  evidence-ledger expansion work (`docs/midwifery-evidence-ledger-design.md`,
  `docs/midwifery-evidence-ledger-results.md`) as a proper case-study
  vignette: a runnable demonstration of `attribute_claims_collapse_lineage()`,
  a small invented illustration (verified against an actual run, not just
  described) of how adding a fourth source can resolve some majority ties
  and create others at once, and the real cohort's aggregate findings --
  T-MSIS agreeing with NPPES only ~50% of the time, majority abstention
  rising on net while `dawid_skene`'s cross-method agreement nearly triples,
  and the independent cross-check against a separately-built full-panel
  analysis (NPPES-removal) landing on the same conclusion about which of two
  migration spikes is NPPES-dependent. No person-level or NPI-level data
  appears in the vignette; only the invented illustration and the already-
  published aggregate numbers.
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
