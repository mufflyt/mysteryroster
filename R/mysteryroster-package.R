#' mysteryroster: registry overlap strata, listing-validity analysis, and
#' no-gold-standard source comparison
#'
#' This package supports two largely independent workflows, built around two
#' different questions. Neither requires the other.
#'
#' **Is a physician listing valid?** (roughly 16 functions, no `@family` tag
#' -- see `vignette("listing-validity-study")`.) The analytic unit is one
#' physician-location listing. The functions follow the order of a
#' registry-validity protocol: cross-link registries ([registry_overlap()]),
#' restrict to the frame registry and define strata ([frame_strata()]), draw
#' a stratified sample that keeps its sampling probabilities
#' ([stratified_listing_sample()]), turn call dispositions into four
#' exhaustive statuses ([classify_listing_status()]), then estimate
#' ([listing_validity_design()], [validity_estimates()],
#' [validity_sensitivity_bounds()], [validity_model()]) and report the flow
#' ([listing_flow_counts()]).
#'
#' **When two or more sources disagree and neither is a gold standard, which
#' one is right, or at least which is leading and which is lagging?**
#' (roughly 30 functions, tagged `@family no-gold-standard comparison` or
#' `@family truth fusion` -- see `vignette("attribute-agreement")`,
#' `vignette("lead-lag-staleness")`, `vignette("consensus-and-corroboration")`,
#' and `vignette("truth-fusion")`.) The analytic unit is one identifier's
#' value for one attribute in one source-year. Four sub-workflows, in the
#' order a comparison typically grows into:
#' \describe{
#'   \item{Single cross-section}{Do two sources agree right now, and how
#'     stale is each record ([attribute_agreement()],
#'     [attribute_agreement_summary()], [record_age_years()],
#'     [record_age_band()], [classify_record_staleness()])?}
#'   \item{Longitudinal, pairwise}{Using repeated annual panels, which source
#'     moves first, how long until the other adopts or reverts
#'     ([attribute_lead_lag_matrix()], [attribute_lead_lag_duration()]), or
#'     the same question as a right-censored survival curve
#'     ([attribute_catch_up_curve()], [attribute_catch_up_curve_prose()]) --
#'     plus [attribute_spell_age()], [attribute_churn_rate()],
#'     [attribute_coverage()], [attribute_agreement_by_spell_age()], and
#'     [enrollment_move_lead_lag()] for the staleness and timing evidence
#'     these two lean on.}
#'   \item{3+ sources at once}{Is there a unanimous value, a majority, or a
#'     genuine split ([attribute_consensus_tier()],
#'     [attribute_consensus_tier_flow()]); is exactly one source the lone
#'     dissenter ([attribute_odd_one_out()], [attribute_odd_one_out_rate()]);
#'     which source does the group converge toward
#'     ([attribute_source_corroboration()])? [attribute_agreement_by_covariate()],
#'     [attribute_agreement_add_ci()], and [wilson_ci()] support any of the
#'     above with a confidence interval or an arbitrary stratifying variable.}
#'   \item{Fusing sources into one predicted value}{Validate the long-format
#'     claims schema ([attribute_claims_validate()]), collapse a declared
#'     same-lineage group to one vote first ([attribute_claims_collapse_lineage()]),
#'     then predict by majority vote or an optional probabilistic backend,
#'     with explicit abstention ([attribute_truth_fusion()],
#'     [attribute_truth_backends()]).}
#' }
#' Manuscript-ready prose for any of the above, rather than hand-written
#' `sprintf()`, comes from [format_no_gold_standard_prose()],
#' [format_proportion_comparison_prose()], [format_ranking_prose()],
#' [attribute_catch_up_curve_prose()], and [attribute_truth_fusion_prose()].
#' None of these functions ever assumes either source is correct --
#' agreement is concordance, not accuracy, throughout.
#'
#' @keywords internal
"_PACKAGE"

#' @importFrom stats as.formula coef quasibinomial setNames
NULL
