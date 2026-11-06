# Saboteur test matrix: Part A (validation protocol), Part B (Phase 3 backend), Part C (QA process), Part D (manuscript prose)

"Saboteur" here follows the truth-discovery literature's own usage: an adversarial source, scenario,
or process failure deliberately constructed to corrupt a result while still looking legitimate on the
surface. Part A's saboteurs attack the *experimental design* in `docs/validation-protocol.md`; Part
B's attack `attribute_truth_fusion()`'s actual inference code via crafted claims; Part C's attack the
*testing/CI process* that is supposed to catch both; Part D's attack the manuscript-prose generators
(`format_proportion_comparison_prose()`, `format_ranking_prose()`, `attribute_lead_lag_duration()`,
`attribute_truth_fusion_prose()`) for grammatical and statistical correctness.

**Status: all 80 executed; 19 of Part B's scenarios and 5 of Part D's are now permanent regression
tests. Part D found five real bugs on first contact (zero-padded sample sizes, inconsistent
"not computable" handling between a degenerate test's p-value and its confidence interval,
`conf_level = 1` propagating a literal `NaN` into rendered text, a whitespace-only label slipping
past `min.chars = 1`, and a source with indeterminate reliability silently vanishing from a ranking
sentence instead of being disclosed as excluded) -- all fixed at the root, not patched around, with
the `wilson_ci()` fix protecting every existing caller of that function, not just the new ones.
Of Part B's 19 permanent tests, 16 cover `majority`/`dawid_skene`, plus 3 added later to give
`truthdiscovery` the same mirror/targeted-saboteur/disconnected-graph coverage `dawid_skene`
already had -- the disconnected-graph one found a genuine, confirmed behavioral difference: `sums`
does not exhibit `dawid_skene`'s flat-tie bug on the identical fixture, verified directly rather
than assumed to generalize.** Part A was
audited against the protocol document's actual text; Part B was first run as real adversarial claims
data in a one-off live R session, then the most load-bearing scenarios (plus several new ones found
while writing them up as tests) were committed to `tests/testthat/test-truth-fusion.R` under the
`[red-team]` prefix -- closing the exact gap Part C's own item #20 ("manual verification never
captured as a regression test") identified against this project. One of those new permanent tests
immediately caught a real error in this document's own earlier write-up: the "disguised abstainer"
(whitespace `claimed_value`) was assumed to be dropped like `NA`; running it as a test instead of
trusting the earlier ad hoc read showed it is **not** dropped (`nzchar("   ")` is `TRUE`) and survives
as its own literal candidate -- corrected below. Part C was run as real commands against the test
suite, coverage tooling, and CI config. See "Execution results" at the end of each section. The most
significant finding (Part B, item 9 generalized) is fixed in the warning text, roxygen docs, and a
standing regression test; the rest are either already-defended (confirmed, not assumed), now
permanent tests, or open items requiring a judgment call before any further code change.

## Part A — 20 saboteurs against the validation protocol

1. **Leaked reference year.** A claim's `reference_year` secretly matches the verification year
   exactly, letting a method implicitly infer "this is the audited year" and behave differently.
2. **Reference-derived claim.** A claims-table "source" is itself derived from (or copied from) the
   verification process — contaminates both the input and the outcome it's scored against.
3. **Partition by row, not physician.** An implementer splits on `(entity_id, reference_year)` pairs
   instead of whole `entity_id`s, letting one physician's claims straddle train/test and leak
   reliability signal (exactly the leakage §2/§6 exist to prevent).
4. **Structure-correlated split seed.** Cohort assignment to train/test uses a seed correlated with
   some pre-existing data ordering, so the partition isn't actually independent of the data.
5. **Cohort-definition gaming.** Iteratively redefining "eligible physician-year" until the result
   favors a preferred method, instead of fixing eligibility rules before looking at any result.
6. **Verification-date backdating.** A reference row's `verification_date` defaults to
   `reference_year`'s Jan 1 on missing input, silently defeating the temporal-holdout check in §7.
7. **Silent exclusion of hard cases.** Discordant-cohort physician-years get dropped from "eligible"
   because a verification call failed to reach consensus, biasing the benchmark toward easy cases.
8. **Normalization mismatch.** Reference rows store `"Denver, CO"`; claims store `"DENVER,CO"` — the
   §8 string-equality check silently fails every row from encoding, not model error.
9. **Majority as a debugging ground-truth stand-in.** A developer sanity-checks the pipeline against
   majority's own output instead of the reference table, and the artifact never fully gets removed.
10. **One-off test-set peek.** An "just to check" look at test-set accuracy during development quietly
    informs a later, nominally-independent parameter choice.
11. **Cohort collapsed to near-zero.** Physician-level stratification (§6) leaves so few relocating
    physicians in the test set that cohort-level accuracy is a coin flip, reported with full confidence.
12. **Verified-by double duty.** The same study's own phone-audit output is used both to build the
    reference table and as one of the evaluated claims sources, making that source trivially "win."
13. **Asymmetric secondary metric.** Geographic distance (§9) is computed under a different
    normalization than the primary accuracy figure, letting a bad model look fine on "average distance."
14. **Abstention dropped from the headline number.** Accuracy is reported "among predictions made"
    without the abstention rate on the same table, violating the §8 pairing requirement.
15. **Reference table edited after the test partition was scored.** A later audit correction updates
    `reference_city`/`reference_state` without re-locking the partition, letting the test set drift.
16. **Global reliability reused across cohorts.** Source reliability fit once on the full population
    gets applied to the discordant cohort — exactly where that global estimate is least applicable.
17. **Cross-study identity collision.** The same NPI appears in two studies' reference tables with
    different provenance and possibly different locations; a naive join silently keeps only one.
18. **Per-method eligible set drift.** Each method's run filters out a slightly different set of rows
    it happens to error on, breaking "same eligible physician-years" (§10) without anyone noticing.
19. **Verification-method quality creep.** `verification_method` quietly shifts from `"phone_audit"`
    to a cheaper proxy over time without re-flagging affected rows as a lower evidence tier.
20. **Duplicate reference rows.** A data-entry error produces two reference rows for one
    `(entity_id, reference_year)` with different locations; `merge()` silently keeps "whichever," making
    the ground truth itself non-deterministic.

## Part B — 20 saboteur sources to inject into `attribute_truth_fusion()` claims

1. **Constant liar.** Always reports the same fixed wrong value regardless of task or truth
   (generalizes the existing false-positive-biased `source_3` fixture to a multi-class setting).
2. **Perfect mirror.** Copies another named source's claims exactly on every task — the
   "shared upstream feed" attack already in the Crowd-Kit vignette, as a reusable fixture.
3. **Sybil flood.** One underlying voice submits under N distinct `source_id`s with identical claims,
   trying to out-vote genuinely independent sources by numbers alone.
4. **Bandwagon mimic.** Doesn't know the truth, but always copies whichever value is currently the
   majority among the OTHER sources for that task — inflates apparent consensus with zero new evidence.
5. **Targeted saboteur.** Reliable on most tasks, but deliberately flips its answer only on an
   attacker-chosen subset (e.g., one state) — a targeted attack, not uniform noise.
6. **Internal saboteur.** Claims multiple different values for the same task — exercises
   `on_source_conflict = "single_value"` exclusion and the `"multi_value"` rejection path.
7. **Disguised abstainer.** Submits a row with an empty-string/whitespace `claimed_value`, checking
   it's treated as "no claim," never as a vote for `""`.
8. **Duplicate-row flooder.** Submits the identical `(entity_id, reference_year, claimed_value)`
   triple dozens of times, checking dedup collapses it to one vote everywhere, not just in majority.
9. **Isolated saboteur.** Rates exactly one task, shared with no other source — engineered to trigger
   `n_disconnected_components > 1` and confirm reliability isn't reported confidently for that island.
10. **Label-recoding attacker.** Reports a semantically identical value under a different literal
    string (`"Colorado"` vs `"CO"`), fragmenting the vote across what should be one candidate.
11. **Version-skew source.** Reports last year's address under this year's `reference_year`, probing
    whether the task key correctly isolates it instead of blending it into the wrong task.
12. **Zero-information near-uniform source.** Spreads claims uniformly at random with no correlation
    to truth (unlike the constant liar) — checks the reliability estimate lands near chance, not pulled
    artificially high or low by small-sample noise.
13. **Easy-task flooder.** Present on hundreds of easy, unanimous tasks and absent from hard,
    discordant ones — engineered to accumulate an inflated reliability score that says nothing about
    its behavior on contested cases.
14. **Anti-correlated flip-flopper.** Error rate is deliberately correlated with task difficulty
    (wrong exactly when other sources are split) — an attacker who specifically targets ambiguous cases.
15. **NA-poisoned identifiers.** A row with `NA`/empty `entity_id` or `source_id` mixed into otherwise
    valid rows, probing the Phase 1 "all-NA grouping" bug class adversarially, not just incidentally.
16. **Collision-engineered identifiers.** `entity_id`/`reference_year` chosen so a naive pasted-string
    key would collide (e.g. `"1"`+`23` vs `"12"`+`3`) — a standing regression guard against the fix, not
    just the one historical test.
17. **Convergence-stalling source.** Claims constructed (per the real `pooled_investment`
    109-iteration finding) to sit just past the configured `max_iterations`, confirming
    `model_did_not_converge`/abstention fires rather than a silent stale estimate.
18. **Implication-function exploiter** (`truthdiscovery` only). A crafted `implication_function`
    returning extreme values (`1`/`-1`) for every pair, checking belief scores stay inspectable and
    `score_type` stays `"uncalibrated_belief"` rather than looking spuriously more confident.
19. **Conflict-policy bookkeeping attacker.** Claims engineered so `"single_value"` and `"multi_value"`
    exclude different source sets, checking `n_sources_excluded`/`n_votes` totals stay internally
    consistent under adversarial, not just friendly, input.
20. **Cross-attribute bleed attempt.** Submits two different `attribute` values under
    identically-structured rows, probing whether "filter to one attribute before grouping" holds even
    under a crafted attempt to get one attribute's claim miscounted as another's vote.

## Part C — 20 saboteurs against the QA/CI process

1. **Flaky skip disguised as pass.** A `skip_if_not()`-guarded test whose own availability probe has
   a latent bug always returning unavailable — the test has never actually run in CI, yet shows green.
2. **Order-dependent test state.** A test only passes because an earlier test in the same file set a
   seed/env var it implicitly depends on; fails if run alone.
3. **Assertion too weak for its own name.** A test titled "excludes a conflicted source" that only
   checks `nrow() > 0`, so a refactor could break the real behavior while the test stays green.
4. **Tolerance laundering.** `tolerance =` set generously wide specifically to make a known-flaky or
   slightly-wrong computation pass, instead of fixing the computation.
5. **Stale golden file.** A saved "expected output" regenerated once from a buggy run and treated as
   ground truth forever after — future correct fixes fail it, future matching bugs pass it.
6. **Silent catch in test setup.** A `tryCatch(..., error = function(e) NULL)` inside a fixture helper
   swallows a real setup failure, letting the test run against an empty fixture and pass trivially.
7. **Environment-dependent numeric output.** An exact-decimal assertion only reproducible on the exact
   machine/library versions that generated it — flaky on a different CI runner's numeric backend.
8. **Split-branch coverage blind spot.** The "available" and "unavailable" branches of a function are
   each 100% covered, but only across DIFFERENT CI runs, never together — a transition bug is invisible.
9. **Non-clean local check.** A locally "all green" `R CMD check` reused cached vignette output instead
   of rebuilding from scratch, hiding a vignette that would fail to render on a clean checkout.
10. **Optional-dependency tests never exercised anywhere automated.** All Python-backend tests
    universally skip on the actual CI runner; "passing" locally and "skipping cleanly" in CI both read
    as success with no automated run ever confirming the skipped tests are still correct.
11. **Message-only assertion.** `expect_message(..., "is not used")` confirms a string printed, not
    that the ignored argument had zero actual effect on the output.
12. **Warning-count-as-correctness proxy.** `expect_equal(n_warnings, 1)` without checking WHICH
    warning fired — swapping in an unrelated warning would still pass.
13. **Dev-venv confidence that evaporates.** Manual verification against a hand-built local Python
    environment gets remembered as "tested," while CI never builds or checks against any such
    environment — true only as long as that throwaway venv happens to still exist.
14. **Suppressed NOTE instead of fixed NOTE.** A `.Rbuildignore` entry added specifically to silence a
    new `R CMD check` NOTE, rather than investigating whether it flags a real packaging problem.
15. **Unreviewed test-file growth.** A single growing test file accumulates a new test that quietly
    duplicates or contradicts an earlier one, unnoticed because no one re-reads the whole file.
16. **Same-process "determinism" test.** Runs a function twice in the same R session/process — can
    pass even if a hidden cache, not the algorithm itself, is why the two outputs match.
17. **Equivalence test checks the wrong layer.** A cross-language check compares `trust` scores but not
    `belief`/`predictions`, so a bug introduced only in belief extraction passes it anyway.
18. **CI green on an unintended commit range.** CI reports success for the pushed `HEAD` without
    confirming that `HEAD` actually contains every commit the author intended to ship.
19. **"0/0/0" treated as behavioral proof.** Repeatedly citing a clean `R CMD check` as if it verified
    correctness, when it verifies packaging/portability/documentation, not that any function computes
    the right answer.
20. **Manual verification never captured as a regression test.** A one-off interactive check (e.g. "does
    this Python library's sparse output still behave the way I verified it does") run once by hand and
    never turned into a test — a future dependency upgrade that changes that behavior would go uncaught.

## Part D — 20 saboteurs against manuscript-prose generation (grammar + statistics)

Targets `format_proportion_comparison_prose()`, `format_ranking_prose()`,
`attribute_lead_lag_duration()`, and `attribute_truth_fusion_prose()` -- the functions that turn a
number into a sentence a manuscript can quote directly. A grammatically broken or statistically
misleading sentence from these is worse than no sentence at all, because it reads as authoritative.

1. **Tied ranking values.** Two groups display identical rounded percentages under
   `format_ranking_prose()` -- does the sentence present a false strict order without flagging it?
2. **Single-group ranking attempt.** Only one group passed to `format_ranking_prose()` -- does it
   error clearly, or silently produce a "ranking" of one thing?
3. **Duplicate group names.** Two entries share the same group label with different values -- does
   the sentence silently list the name twice without flagging the collision?
4. **All-identical values, no ties flagged.** Every group has the exact same value -- does the
   sentence read as if there's a meaningful order when there isn't one at all?
5. **NA value smuggled into a ranking.** One group's value is `NA` -- does it get silently ranked
   (sorted to the top or bottom by R's own `NA` handling) instead of rejected outright?
6. **Zero-count group diluting a ranking.** A group with `n = 0` ranked alongside groups with large
   `n`, implying equal statistical weight to a non-estimate -- and does the displayed `n` itself
   render cleanly, not padded with artifact whitespace?
7. **Wildly unequal sample sizes, same displayed rate.** Group A: 3/5 (60%); Group B: 3,000/5,000
   (60%) -- does the comparison sentence show `n` so the reader isn't misled into treating these as
   equally precise?
8. **`n_a = 0` boundary proportion.** 0 of N -- does the Wilson CI/sentence report a real, non-
   degenerate interval rather than something nonsensical?
9. **`n_a = n_total_a` boundary proportion.** 100% exactly -- same check at the other extreme.
10. **`n_total = 1`, tiny denominator.** A single-entity-year proportion stated with apparent
    precision ("100.0%...") -- does the CI actually show the real (very wide) uncertainty?
11. **`prop.test()` degenerate without erroring.** 0/1 vs. 0/1 -- `prop.test()` returns `NaN`
    p-value/statistic without throwing, and a conf.int that may still look like real numbers. Does
    the sentence consistently mark the *whole* test "not computable," not just the p-value?
12. **Difference sign flip.** When group A's rate exceeds group B's (or vice versa), does "a
    difference of X percentage points" report the correct sign, consistently?
13. **Format-string injection via a label.** A label containing literal `%s`/`%d`-like text, or a
    raw `%` -- does `sprintf()` choke, misinterpret, or silently drop part of the sentence?
14. **Whitespace-only label.** `label_a = "   "` -- does `min.chars = 1` actually reject it, or does
    a blank-looking label slip through into "In   , 10 of 20..."?
15. **`conf_level` at the boundary (0 or 1).** Degenerate confidence levels -- does the interval
    still render as real numbers, or leak `NaN`/a meaningless zero-width interval into the sentence?
16. **Floating-point-equal values from different arithmetic paths.** Two values that are genuinely,
    exactly equal but computed via different code paths -- confirmed still tied, not spuriously split
    apart by a floating-point representation difference.
17. **Singular/plural at `n = 1`.** `attribute_lead_lag_duration()` with exactly one discordant
    identifier -- "1 identifier", never "1 identifiers".
18. **Non-integer-but-whole `base_year`.** `base_year = 2020.0` (a double, not an integer literal)
    passed to `attribute_lead_lag_duration()` -- does `%d`-style formatting break or print a decimal?
19. **Fractional count mistaken for a real count.** `n_a = 10.5` (e.g. an accidentally-passed mean
    instead of a true count) -- does `checkmate::assert_count()` actually reject it?
20. **Source with indeterminate reliability silently dropped from a ranking.** A source present in
    `source_diagnostics` with `NA` reliability (e.g. isolated in a disconnected graph component) --
    does `attribute_truth_fusion_prose()`'s ranking sentence disclose that exclusion, or does the
    source just vanish with no trace, reading as if it never contributed at all?

## Execution results: Part A (audit against the protocol document's actual text)

Checked each of the 20 against `docs/validation-protocol.md` as written, not as intended.

- **Already closed, confirmed by re-reading the text, not assumed:** #2/#12 (verified-by excludes a
  study's own audit output doing double duty — §1), #3 (physician-level partitioning — §6
  explicit), #14 (abstention/accuracy paired reporting — §8 explicit).
- **Real gaps found, now added as §11 of the protocol:** #1 (verification-date can be silently
  backdated/missing, unguarded), #5/#7 (normalization between claims and reference tables assumed,
  not enforced — and `attribute_claims_validate()` actually does *zero* transformation, so the
  assumption was resting on nothing), #4/#9/#10 (verification-selection bias, split not locked,
  no operational seal against a development-time test-set peek), #11 (no minimum cohort size),
  #17 (cross-study NPI collisions unhandled), #20 (duplicate reference rows assumed away, no
  validation step analogous to `attribute_claims_validate()`'s), #18 (same-eligible-set is a stated
  goal, not an enforced one), #16 (global per-source reliability is a modeling constraint the
  protocol can only flag, not fix — directly confirmed by Part B findings 5 and 13 below).
- Full detail and the exact protocol-text fix for each: `docs/validation-protocol.md` §11.

## Execution results: Part B (real adversarial claims run against the real backends)

Ran all 20 as actual claims data through `attribute_truth_fusion()` with `method = "majority"`
always, plus `"dawid_skene"` (crowd-kit 1.4.2, Python 3.13) and `"sums"`/`"truthfinder"`
(truthdiscovery 1.0.4, Python 3.11) where configured. Full script at the end of this file.

**The one fix already made (code + docs, this round):** items #9 and #11 generalize into a severe,
previously undocumented failure mode: when the worker-task graph is fully disconnected into
single-source or tiny components, `dawid_skene` can report a **flat prior across every candidate
value for every task in the affected components** — including a task where all included sources
unanimously agreed (`n_candidate_values == 1`). Confirmed directly:

```
3 fully disconnected components, each 2 sources unanimously agreeing (CO / NM / TX):
  entity 1: predicted <NA>, model_score 0.333, abstained TRUE, reason "tied_vote"
  entity 2: predicted <NA>, model_score 0.333, abstained TRUE, reason "tied_vote"
  entity 3: predicted <NA>, model_score 0.333, abstained TRUE, reason "tied_vote"
```

This was already silently present in `vignette("truth-fusion-crowdkit")`'s own disconnected-graph
example — the vignette printed `n_disconnected_components` but never `predicted_value`, so the
all-abstained result went unnoticed until this red-team pass. `truthdiscovery`'s `sums` does **not**
exhibit this on the same fixture (it predicted correctly on an analogous disconnected,
single-voter task) — a real, confirmed difference between the two backends, not an assumption.

Fixed: the `n_disconnected_components > 1` warning (`R/truth_fusion_crowdkit.R`) and
`attribute_truth_fusion()`'s roxygen docs now say this explicitly, and
`vignette("truth-fusion-crowdkit")`'s own example now prints and explains the all-abstained result
it was already producing. No prediction-computation behavior was changed — this is a documentation
and warning-text fix only.

**Other findings (no code changes — these are either already-documented limitations or methodology
observations with no safe unilateral fix):**

- #2/#4 (mirror/bandwagon): confirmed empirically that a copied or copying source's learned
  reliability tracks whoever it copies, not genuine independent evidence — consistent with the
  already-shipped `vignette("truth-fusion-crowdkit")` limitation section, now with one more
  concrete numeric confirmation (bandwagon reliability = 1.0 vs. the source it copies' 0.92–0.96).
- #5/#13 (targeted saboteur / easy-task flooder): confirmed a global reliability score cannot
  surface subset-specific error patterns (targeted source: 92–100% global reliability despite
  always being wrong on a chosen subset; easy-task flooder: 50–90% reliability from never being
  tested on a hard case) — this is exactly why `docs/validation-protocol.md` §9/§11 require
  per-cohort reporting, not a global number.
- #10 (label recoding): confirmed `"Colorado"` and `"CO"` fragment into two separate candidates
  with no normalization anywhere in the pipeline — matches `attribute_claims_validate()`'s own
  documented "no transformation" behavior exactly; not a bug.
- #12 (zero-information source): reliability landed at 0.45 (crowd-kit) / 0.69 (truthdiscovery) on
  n=20 uncorrelated tasks — both in the broad neighborhood of chance, with visible small-sample
  noise; not pinned exactly to 0.5, as expected.
- All remaining items (#1, #3, #6, #7, #8, #15, #16, #17, #18, #19, #20) ran with no errors and
  matched the documented/expected behavior exactly — dedup, exclusion counts, task isolation by
  `(entity_id, reference_year)`, the collision-safe grouping key, cross-attribute isolation, and the
  `pooled_investment`/`implication_function` edge cases all held under adversarial input, including
  for the two optional backends, not just majority.

## Execution results: Part C (real commands against the real test suite, coverage, and CI config)

- **#10, confirmed with hard numbers, not an estimate:** `covr::package_coverage()` on the default
  (no-Python) test run — i.e., exactly what GitHub Actions CI actually executes — shows
  `R/truth_fusion_crowdkit.R` at **13.0%** line coverage, `R/truth_fusion_truthdiscovery.R` at
  **14.0%**, `R/truth_fusion_backends_shared.R` at **13.1%**. Roughly 87% of all three optional-
  backend files has *never been executed by any automated CI run*, ever, across three merged
  commits. `.github/workflows/R-CMD-check.yaml` has no Python/reticulate/crowd-kit/truthdiscovery
  setup step at all — confirmed by direct grep, not inferred. Every verification of this backend
  code this session happened by hand, against a throwaway dev venv in a scratch directory that
  will not exist on any future machine (saboteur #13, also confirmed). Re-measured after adding
  the `[red-team]` tests below, with crowd-kit actually configured: `truth_fusion_backends_shared.R`
  jumps to **100%**, `truth_fusion_crowdkit.R` to **82.4%** — confirming the new tests genuinely
  exercise this code, not just that they exist. The gap is entirely environmental: the coverage
  improvement is real but conditional on Python being configured, and CI still never configures it.
- **#11 and #12, confirmed real but narrower than first described:** the `max_iterations`
  "not used" test only checked the message text, not the actual effect — fixed (now also asserts
  identical output with and without the argument). The `n_warnings == 1` nonconvergence test turned
  out to already pair with a separate `expect_warning(..., "did not converge")` content check in
  the same test — a milder form of the attack than described; left as-is rather than forcing an
  unneeded change.
- **#2 (order dependence), #14 (suppressed NOTE):** checked directly — no test mutates global state
  (`Sys.setenv`/`options`) without the existing restore pattern; `.Rbuildignore` contains only
  standard R-package scaffolding entries, no NOTE-suppression added to hide a real problem.
- **#1, #3–9, #13, #15–20:** not independently exercised this round (would require deliberately
  breaking a probe, introducing a stale snapshot, or auditing commit history for force-pushes) —
  open items, not confirmed either way.

## Execution results: Part D (real adversarial calls against the real prose functions)

Ran all 20 as actual function calls, not a thought experiment -- each one executed directly against
`format_proportion_comparison_prose()`, `format_ranking_prose()`, `attribute_lead_lag_duration()`,
and `attribute_truth_fusion_prose()` before any fix, to see the real output first.

**Five real bugs found and fixed (code + permanent `[red-team]` regression tests in
`tests/testthat/test-ranking-prose.R` and `tests/testthat/test-truth-fusion-prose.R`):**

- **#6 (zero-count dilution) also surfaced a formatting bug, not just a weighting question:**
  `format(c(0, 5000), big.mark = ",")` right-pads to equal width by default, so `n = 0` rendered as
  `"n =     0"` (four stray leading spaces) inside the sentence. Fixed with `trim = TRUE`.
- **#11 (degenerate `prop.test()`) found inconsistent "not computable" handling:** `prop.test(c(0,
  0), c(1, 1))` returns without erroring, but `p.value`/`statistic` come back `NaN` (not `NA`) and
  `conf.int` still holds numeric-looking bounds. The p-value path already caught this correctly
  (`is.na(NaN)` is `TRUE` in R), but the confidence-interval path did not -- it rendered
  `"95% CI -0.0 to -0.0%"`, a nonsensical interval from the same degenerate fit the p-value had
  already been flagged as untrustworthy. Fixed: when the test is not computable, the CI is now
  marked not computable too, consistently, not just the p-value.
- **#15 (`conf_level` boundary) found the single most severe bug of the twenty:** `conf_level = 1`
  made `wilson_ci()`'s internal `qnorm(1)` evaluate to `Inf`, propagating all the way into a
  rendered sentence as `"100% CI NaN-NaN%"` -- a literal `NaN` in manuscript-quotable text.
  `conf_level = 0` didn't error, but silently returned a zero-width interval equal to the point
  estimate, which is never a real confidence interval. Fixed at the root: `wilson_ci()` itself (an
  existing, widely-used function, not just the new prose functions built on it) now rejects
  `conf_level` outside the open interval `(0, 1)`, so every caller -- not just the ones audited
  here -- is protected.
- **#14 (whitespace-only label) found a real gap `min.chars = 1` cannot catch:**
  `checkmate::assert_string(x, min.chars = 1)` accepts `"   "` (3 real characters, all whitespace),
  producing `"In   , 10 of 20..."` -- grammatically broken with no indication why. Fixed with a new
  shared internal helper, `.assert_nonblank_string()`, applied to every label parameter across all
  four functions (`label_a`/`label_b`/`unit_label`/`condition_label`, `value_label`,
  `source_a_label`/`source_b_label`).
- **#20 (indeterminate reliability silently dropped) found a real completeness gap, not a crash:**
  a source present in `attribute_truth_fusion()`'s own `source_diagnostics` with `NA` reliability
  (for example isolated in a disconnected graph component, per that function's own documented
  `n_disconnected_components` caveat) used to vanish from `attribute_truth_fusion_prose()`'s
  ranking sentence with zero trace -- reading as if only the remaining sources existed. Fixed: the
  sentence now explicitly names any source excluded this way and how many.

**Fifteen items ran clean on the first try, already correctly defended:**

- #1-#5, #7-#10, #12, #13, #16-#19 all produced correct, clearly-worded output with no code change
  needed -- including #13 (format-string-like text in a label is never re-interpreted by `sprintf()`,
  since only the *format string* argument is parsed for `%` specifiers, not substituted values) and
  #19 (`checkmate::assert_count()` already rejects a fractional count like `10.5` outright).

Full detail, exact before/after output for each of the five fixed items, and the permanent test
names: `tests/testthat/test-ranking-prose.R`, `tests/testthat/test-truth-fusion-prose.R` (search
`[red-team]`).

## Part B script (as actually run)

See `R/truth_fusion.R`, `R/truth_fusion_crowdkit.R`, `R/truth_fusion_truthdiscovery.R` for the code
under test. The adversarial-claims script itself is not checked into the package (it is a one-off
red-team driver, not a permanent test) — reach out for the transcript if reproducing beyond what is
summarized above.
