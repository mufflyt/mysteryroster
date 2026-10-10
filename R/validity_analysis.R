#' Survey design for a stratified listing sample
#'
#' Wraps `survey::svydesign()` with the columns [stratified_listing_sample()]
#' writes. Clustering (for example by caller or by practice) is optional.
#'
#' @param df Sampled listings with `stratum` and `weight` columns.
#' @param strata,weights Column names. Defaults `"stratum"`, `"weight"`.
#' @param ids Optional cluster column name; `NULL` means independent listings.
#' @return A `survey.design` object.
#' @export
listing_validity_design <- function(df, strata = "stratum", weights = "weight", ids = NULL) {
  checkmate::assert_data_frame(df, min.rows = 1)
  for (col in c(strata, weights, ids)) {
    if (!col %in% names(df)) stop(sprintf("listing_validity_design: no column '%s'", col), call. = FALSE)
  }
  if (any(is.na(df[[weights]]) | df[[weights]] <= 0)) {
    stop("listing_validity_design: every weight must be positive and non-missing", call. = FALSE)
  }
  survey::svydesign(
    ids     = if (is.null(ids)) ~1 else as.formula(paste0("~", ids)),
    strata  = as.formula(paste0("~", strata)),
    weights = as.formula(paste0("~", weights)),
    data    = df,
    nest    = TRUE
  )
}

#' Design-weighted proportion of listings in a given status
#'
#' @param design Output of [listing_validity_design()].
#' @param status Column holding [listing_status_levels()] values. Default
#'   `"listing_status"`.
#' @param level Which status to estimate. Default `"confirmed_valid"`.
#' @param by Optional grouping column (for example `"stratum"`).
#' @param definitive_only If `TRUE`, restrict the design to listings whose
#'   status is `confirmed_valid` or `confirmed_invalid` (the complete-case
#'   estimand); otherwise all sampled listings form the denominator.
#' @param level_ci Confidence level. Default 0.95.
#' @return A tibble with `estimate`, `se`, `ci_low`, `ci_high` and the
#'   unweighted `n`, one row overall or per `by` group.
#' @export
validity_estimates <- function(design, status = "listing_status", level = "confirmed_valid",
                               by = NULL, definitive_only = FALSE, level_ci = 0.95) {
  checkmate::assert_class(design, "survey.design")
  vars <- design$variables
  if (!status %in% names(vars)) stop(sprintf("validity_estimates: no column '%s'", status), call. = FALSE)
  if (definitive_only) {
    keep <- vars[[status]] %in% c("confirmed_valid", "confirmed_invalid")
    design <- subset(design, keep)
    vars <- design$variables
  }
  design$variables$.y <- as.integer(vars[[status]] == level)
  z <- stats::qnorm(1 - (1 - level_ci) / 2)
  if (is.null(by)) {
    m <- survey::svymean(~.y, design, na.rm = TRUE)
    est <- as.numeric(m); se <- as.numeric(survey::SE(m))
    out <- tibble::tibble(estimate = est, se = se, n = nrow(design$variables))
  } else {
    if (!by %in% names(vars)) stop(sprintf("validity_estimates: no column '%s'", by), call. = FALSE)
    m <- survey::svyby(~.y, as.formula(paste0("~", by)), design, survey::svymean, na.rm = TRUE)
    out <- tibble::tibble(group = as.character(m[[by]]), estimate = as.numeric(m$.y),
                          se = as.numeric(survey::SE(m)))
    names(out)[1] <- by
    out$n <- as.integer(table(design$variables[[by]])[out[[by]]])
  }
  out$ci_low  <- pmax(0, out$estimate - z * out$se)
  out$ci_high <- pmin(1, out$estimate + z * out$se)
  out
}

#' Extreme-case bounds for unresolved listings
#'
#' Three unweighted scenarios. Observed: unresolved excluded (definitive
#' determinations only). Best case: unresolved counted as valid. Worst case:
#' unresolved counted as invalid. Not-attempted listings are excluded from
#' every denominator and reported separately. The bounds are asserted to be
#' ordered worst <= observed <= best.
#'
#' @param df Data frame with a status column.
#' @param status Column name. Default `"listing_status"`.
#' @return A tibble with `scenario`, `n_valid`, `denominator`, `pct_valid`.
#' @export
validity_sensitivity_bounds <- function(df, status = "listing_status") {
  checkmate::assert_data_frame(df)
  if (!status %in% names(df)) stop(sprintf("validity_sensitivity_bounds: no column '%s'", status), call. = FALSE)
  s <- as.character(df[[status]])
  n_valid <- sum(s == "confirmed_valid"); n_invalid <- sum(s == "confirmed_invalid")
  n_unres <- sum(s == "unresolved"); n_notatt <- sum(s == "not_attempted")
  if (n_valid + n_invalid + n_unres + n_notatt != nrow(df)) {
    stop("validity_sensitivity_bounds: status column holds values outside listing_status_levels()", call. = FALSE)
  }
  denom_obs <- n_valid + n_invalid
  denom_rng <- n_valid + n_invalid + n_unres
  out <- tibble::tibble(
    scenario    = c("observed_definitive_only", "best_case_unresolved_valid", "worst_case_unresolved_invalid"),
    n_valid     = c(n_valid, n_valid + n_unres, n_valid),
    denominator = c(denom_obs, denom_rng, denom_rng)
  )
  out$pct_valid <- ifelse(out$denominator > 0, 100 * out$n_valid / out$denominator, NA_real_)
  attr(out, "not_attempted") <- n_notatt
  if (all(out$denominator > 0) && !(out$pct_valid[3] <= out$pct_valid[1] && out$pct_valid[1] <= out$pct_valid[2])) {
    stop("validity_sensitivity_bounds: bounds are not ordered worst <= observed <= best", call. = FALSE)
  }
  out
}

#' Survey-weighted logistic regression of listing validity
#'
#' Fits `survey::svyglm()` with a quasibinomial logit link on the definitive
#' listings only (unresolved and not-attempted are excluded, as in the
#' complete-case estimand) and returns odds ratios with confidence intervals.
#'
#' @param design Output of [listing_validity_design()].
#' @param formula Right-hand side as a formula, for example
#'   `~ stratum + rural + record_age_years`.
#' @param status,level As in [validity_estimates()].
#' @return A tibble from `broom::tidy()` with exponentiated estimates.
#' @export
validity_model <- function(design, formula, status = "listing_status", level = "confirmed_valid") {
  checkmate::assert_class(design, "survey.design")
  checkmate::assert_formula(formula)
  vars <- design$variables
  keep <- vars[[status]] %in% c("confirmed_valid", "confirmed_invalid")
  design <- subset(design, keep)
  design$variables$.y <- as.integer(design$variables[[status]] == level)
  f <- stats::update(formula, .y ~ .)
  fit <- survey::svyglm(f, design = design, family = quasibinomial(link = "logit"))
  tibble::as_tibble(broom::tidy(fit, conf.int = TRUE, exponentiate = TRUE))
}
