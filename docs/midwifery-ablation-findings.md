# Source-ablation and cross-method stability on a real 1,806-case cohort

Status: a completed empirical investigation, run once, reported in full including the parts that
complicate the earlier, more optimistic framing in `vignette("truth-fusion-case-study-midwifery")`.
This is **not** a validated accuracy benchmark -- no independently verified reference location exists
for this cohort (see `docs/validation-protocol.md`). It is a stability/robustness check: do these
methods agree with themselves under source removal, and with each other, on the same real data.

## 0. What the cohort actually is -- read this before the rest

The 1,806 cases come from an unrelated project's prototype (`diagnose_truth_discovery_migration_
states.R`, in the `midwifery` repository) that defines a "mover" as an NPI whose **NPPES** reported
state differs between two snapshot years:

```r
valid_pairs <- inner_join(from, to, by = "npi")   # from/to are both NPPES-year slices
movers <- valid_pairs %>% filter(state_from != state_to)
```

This means **every one of the 1,806 cases is, by construction, an apparent NPPES relocation** --
not an independently-identified set of real moves. Describing them as "1,806 adjudicated
relocations" without this caveat overstates what was actually established. Everything below should
be read with that in mind: this cohort is already anchored to NPPES before any truth-discovery
method sees it.

The `confirmed` flag used in earlier framing is **also NPPES-internal**: it checks only whether
NPPES's own state the year *before* `year_from` and the year *after* `year_to` match an A,A,B,B
pattern (`state_pre == state_from & state_to == state_post`), using no other source at all. Treating
"`dawid_skene`'s posterior confidence in NPPES's claim is higher when `confirmed` is TRUE" as
external validation is circular: both quantities are fundamentally measures of whether NPPES's own
narrative is self-consistent. The adjudication script also computes `any_corroboration` (does at
least one *other* source's before/after state match NPPES's, including a single DAC-only or
PECOS-only match) and `independent_corroboration` (requires 2+ sources that are not both members of
the known DAC/PECOS shared-lineage group). `any_corroboration` is TRUE for 204/1,806 cases, **11.3%
(95% Wilson CI 9.9-12.8%)**, but is still anchored to matching NPPES's claim, not an independent read
of the true state. `independent_corroboration` is the only flag with a real claim to independence,
and it is TRUE for only **6 of 1,806 cases, 0.3% (95% CI 0.2-0.7%)** -- too rare to use as a
comparison group. None of the three flags in this dataset constitutes a clean, independent reference
standard. (All intervals in this document are 95% Wilson score intervals, computed with this
package's own `wilson_ci()`.)

## 1. Setup

Real claims: `attribute_truth_fusion()`'s schema (`entity_id` = NPI, `attribute = "practice_state"`,
`source_id`, `claimed_value`, `reference_year`), built from the prototype's own three sources for
every NPI-year touched by the 1,806 movers (`year_from` and `year_to` for each):

| source | n claims | provenance |
|---|---|---|
| `nppes` | 3,556 | NPPES snapshot, one row per cohort NPI-year (verified: 0 of 215,078 cohort NPI-years carry >1 distinct state) |
| `dac_pecos_combined` | 1,897 | DAC (Doctors and Clinicians) and PECOS revalidation, merged by the prototype's own rule: agree -> one vote; disagree -> no vote; one present -> use it |
| `open_payments` | 521 | CMS Open Payments, state of the recipient on a payment record with `payment_amount > 0` |

3,556 NPI-years, 1,631 distinct real NPIs, 2,000 NPI-years (56.2%) with 2+ source claims. NPPES,
PECOS-derived data, and CMS Open Payments are all public federal transparency datasets by design.

## 2. Baseline fit: all three sources

| method | backend | reliability (nppes / dac_pecos_combined / open_payments) |
|---|---|---|
| `dawid_skene` | crowd_kit | 0.881 / 0.644 / 0.712 |
| `one_coin_dawid_skene` | crowd_kit | 0.844 / 0.717 / 0.836 |
| `truthfinder` | truthdiscovery | 0.597 / 0.620 / 0.657 |
| `sums` | truthdiscovery | 1.000 / 0.506 / 0.135 |
| `average_log` | truthdiscovery | 0.748 / 0.907 / 1.000 |
| `investment` | truthdiscovery | 0.000 / 0.000 / 1.000 |
| `pooled_investment` | truthdiscovery | 1.000 / 0.000 / 0.001 |

**These seven methods do not agree on which of the three sources is most reliable.**
`dawid_skene`, `one_coin_dawid_skene`, `sums`, and `pooled_investment` rank `nppes` highest.
`truthfinder`, `average_log`, and `investment` rank it lowest (`investment` ties it with
`dac_pecos_combined` at exactly 0). This split does not track backend family cleanly --
`truthdiscovery`'s own five methods disagree with each other as much as they disagree with
`crowd_kit`'s two.

Plain `majority` ties (abstains) on 695 of the 3,556 NPI-years -- every one of those is a case where
exactly two sources disagreed and neither outnumbered the other.

## 3. Do `dawid_skene` and `truthfinder` actually resolve the same 695 ties the same way?

Both report 0% abstention and a predicted value for all 695. Checked directly, not assumed:

**They agree on the predicted state for only 46 of 695: 6.6% (95% CI 5.0-8.7%).** On the other 649
(93.4%, CI 91.3-95.0%), they pick *different* states for the same NPI-year, using the identical input
claims. In every sampled disagreement, `dawid_skene` sides with `nppes` and `truthfinder` sides with
whichever other source was present -- a direct, mechanical consequence of their opposite-direction
reliability rankings in §2. "Majority ties, truth discovery resolves it" is not a well-defined single
answer here; it is two different, confidently-converged answers depending which algorithm is asked.

## 4. Source-ablation: how much of "resolving" a tie is really just trusting NPPES?

For the 695 NPI-years `dawid_skene` resolves in the full (3-source) fit, refit with one source
removed and check whether the predicted state is unchanged. All proportions out of n = 695 unless
noted; 95% Wilson CIs in parentheses.

| ablation | `dawid_skene` | `one_coin_dawid_skene` | `truthfinder` |
|---|---|---|---|
| remove `nppes` | **7.2%** unchanged, 50/695 (5.5-9.4%) | 24.2% unchanged, 168/695 (21.1-27.5%) | **100%** unchanged, 695/695 (99.5-100%) |
| remove `dac_pecos_combined` | 92.5% unchanged, 643/695 (90.3-94.2%) | 74.5% unchanged, 518/695 (71.2-77.6%) | not run |

Breaking down the `dawid_skene`/remove-`nppes` result further: of the 695, **678 (97.6%, CI
96.1-98.5%) had only one source remaining** once `nppes` was removed -- these were never a genuine
three-way comparison, they were `nppes` against exactly one other source, and removing `nppes`
trivially leaves "whatever that other source said" as the only possible answer. Only **17 of 695
(2.4%, CI 1.5-3.9%) had both other sources present** after removing `nppes`; in **all 17, the
prediction changed** (0/17 unchanged, 0%, CI 0.0-18.4% -- wide because n = 17, but the point
estimate itself is exact). `dawid_skene`'s apparent "resolution" of 695 ties is, on this cohort, almost entirely a
restatement of "trust `nppes` over one disagreeing source" -- not a genuine synthesis of three
sources' evidence. `one_coin_dawid_skene` shows the same direction, less extremely (consistent with
its coarser, scalar-skill model). `truthfinder`, having already learned to discount `nppes`, is of
course unaffected by removing it.

## 5. Freshness filtering: does not apply to this dataset

The source-ablation plan also called for comparing results with and without freshness filtering.
Checked before attempting it: every claim here was pulled via `lookup_state()`'s exact-year match
(`src_df$year == year_val`), with no nearest-year fallback -- every claim is already exactly at the
target year by construction. There is no staleness dimension to filter on within this cohort; this
item does not apply here, not because it was skipped, but because the data has no "older" vs
"fresher" claim for the same NPI-year to choose between.

## 6. Do the 695 resolved ties have weaker external corroboration than the rest of the cohort?

No NPI-year was abstained by `dawid_skene` in the full fit (0% abstention; see §2), so there is no
"still-unresolved" group to compare the 695 against *within this method's own output*. The
comparison that is well-defined instead: among the cohort's 1,806 mover transitions, split into
those whose `year_to` is one of `dawid_skene`'s 695 resolved ties (n = 290; the other 405 of the 695
are a transition's `year_from`, where `any_corroboration` isn't the relevant endpoint) versus every
other transition (n = 1,516) -- two non-overlapping groups, not a subset-vs-superset comparison.

| group | n | `any_corroboration` TRUE | % | 95% CI |
|---|---|---|---|---|
| tie (one of the 695 resolved, as the destination year) | 290 | 3 | **1.0%** | 0.4-3.0% |
| everything else | 1,516 | 201 | 13.3% | 11.6-15.1% |

Two-proportion test: p < 0.001 (p ≈ 3.1 x 10^-9). This is a large, real, well-powered difference --
but it does not say `dawid_skene` is doing well on the hard cases. It says the hard cases were
already the ones with almost no external corroboration *before* any truth-discovery method touched
them: a transition where another source happens to match NPPES's claimed before/after state is
much less likely to produce a tie for plain majority in the first place, since that matching source
is also likely to agree with NPPES at the specific year being contested. The 695 ties are, by this
measure, close to the least-evidenced transitions in the whole cohort -- exactly where §2's and §3's
cross-method disagreement would be expected, not a surprise alongside it.

*An earlier version of this section reported a materially different, incorrect figure (stale 11.0%
vs. 11.3%, "not meaningfully different") from a computation that silently mis-joined the comparison
groups. Recomputed with the non-overlapping groups above before trusting it this time.*

## 7. What this does and does not establish

It establishes, directly and reproducibly: plain majority vote ties on 695 of 3,556 real NPI-years;
different truth-discovery methods -- including different methods within the *same* optional
backend -- learn contradictory reliability rankings for the same three real sources; `dawid_skene`
and `truthfinder` agree on the resolved value for those ties only 6.6% of the time; `dawid_skene`'s
resolutions are, for 97.6% of the cases, actually just "trust NPPES" restated, not a three-source
synthesis; and the 695 tied NPI-years are themselves drawn from the least externally-corroborated
part of the cohort (1.0% vs. 13.3% `any_corroboration`, §6) -- so the cases where these methods'
disagreement matters most are exactly the cases with the least outside evidence to adjudicate it.
It does **not** establish which source, or which method, is actually correct for any of these real
NPIs -- no independently verified reference location exists for this cohort, and the cohort itself
is anchored to NPPES by its own construction (§0). Read together with `docs/validation-protocol.md`,
the honest conclusion is closer to the sobering interpretation than the encouraging one: on this
specific real administrative-data problem, with only three source families and partial coverage,
*which* source to trust is not a settled question any of these methods can answer on their own, a
confident, fully-converged fit from any one of them is not evidence that it answered it correctly,
and the subset of cases most in need of a correct answer is precisely the subset with the thinnest
external evidence to check it against.
