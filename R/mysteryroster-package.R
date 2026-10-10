#' mysteryroster: registry overlap strata and listing-validity analysis
#'
#' The analytic unit throughout is one physician-location listing. The
#' functions follow the order of a registry-validity protocol: cross-link
#' registries ([registry_overlap()]), restrict to the frame registry and
#' define strata ([frame_strata()]), draw a stratified sample that keeps its
#' sampling probabilities ([stratified_listing_sample()]), turn call
#' dispositions into four exhaustive statuses ([classify_listing_status()]),
#' then estimate ([listing_validity_design()], [validity_estimates()],
#' [validity_sensitivity_bounds()], [validity_model()]) and report the flow
#' ([listing_flow_counts()]).
#'
#' @keywords internal
"_PACKAGE"

#' @importFrom rlang .data %||%
#' @importFrom stats as.formula quasibinomial setNames
NULL
