#' Survey design for a stratified listing sample
#'
#' Wraps `survey::svydesign()` with the columns [stratified_listing_sample()]
#' writes. Clustering (for example by caller or by practice) is optional.
#'
#' @param df Sampled listings with `stratum` and `weight` columns.
#' @param strata,weights Column names. Defaults `"stratum"`, `"weight"`.
#' @param ids Optional cluster column name; `NULL` means independent listings.
#' @return A `survey.design` object.
#' @examples
#' calls <- data.frame(
#'   stratum = c("a", "a", "b", "b"), weight = c(2, 2, 1, 1),
#'   listing_status = c("confirmed_valid", "confirmed_invalid", "confirmed_valid", "unresolved"))
#' listing_validity_design(calls)
#' @export
listing_validity_design <- function(df, strata = "stratum", weights = "weight", ids = NULL) {
  checkmate::assert_data_frame(df, min.rows = 1)
  for (col in c(strata, weights, ids)) {
    if (!col %in% names(df)) stop(sprintf("listing_validity_design: no column '%s'", col), call. = FALSE)
  }
  if (!is.numeric(df[[weights]]) ||
      any(!is.finite(df[[weights]]) | df[[weights]] <= 0)) {
    stop("listing_validity_design: every weight must be positive, finite and non-missing", call. = FALSE)
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
#' @param status Column holding [listing_status_levels()] values.
#' @param level Which status to estimate. Default `"confirmed_valid"`.
#' @param by Optional grouping column.
#' @param definitive_only Restrict to confirmed valid or invalid listings.
#'   The default uses all sampled listings as the denominator.
#' @param level_ci Confidence level, strictly between zero and one.
#' @param outcome Optional logical outcome column. When supplied, status and
#'   level are ignored and missing outcomes are excluded.
#' @param ci_method `"beta"` (default) uses [survey::svyciprop()] with the
#'   Korn-Graubard effective-sample-size interval. `"mean"` reproduces the
#'   legacy normal interval from [survey::svymean()], clipped to [0, 1].
#' @return A tibble with estimate, se, unweighted n, ci_low, ci_high and
#'   ci_status, overall or per group. Empty domains have n = 0 and missing
#'   estimates and intervals. Groups present before outcome exclusion remain.
#' @details
#' Beta intervals are missing at exactly zero or one (ci_status = "boundary")
#'   and when the estimated variance is zero or non-finite
#'   ("undefined_variance"); these cases do not support this interval.
#'   The point estimate is retained. Empty domains have status "empty".
#'   Otherwise status is "estimated", or "legacy_mean" for the mean method.
#'   The legacy method can produce zero-width intervals at the boundaries.
#'   Survey design assumptions and variance settings still apply; these
#'   intervals describe sampling uncertainty, not unresolved-status bounds.
#' @examples
#' calls <- data.frame(
#'   stratum = c("a", "a", "b", "b"), weight = c(2, 2, 1, 1),
#'   listing_status = c("confirmed_valid", "confirmed_invalid",
#'                      "confirmed_valid", "unresolved"))
#' design <- listing_validity_design(calls)
#' validity_estimates(design)
#' validity_estimates(design, by = "stratum", ci_method = "mean")
#' @export
validity_estimates <- function(
    design, status = "listing_status", level = "confirmed_valid",
    by = NULL, definitive_only = FALSE, level_ci = 0.95,
    outcome = NULL, ci_method = c("beta", "mean")) {
  checkmate::assert_class(design, "survey.design")
  checkmate::assert_flag(definitive_only)
  checkmate::assert_number(level_ci, lower = 0, upper = 1, finite = TRUE)
  if (level_ci <= 0 || level_ci >= 1) {
    base::stop("level_ci must be strictly between zero and one.",
               call. = FALSE)
  }
  ci_method <- base::match.arg(ci_method)
  vars <- design$variables
  if (!base::is.null(by)) {
    checkmate::assert_string(by, min.chars = 1)
    if (!by %in% base::names(vars)) {
      base::stop("validity_estimates: no column '", by, "'", call. = FALSE)
    }
  }
  if (!base::is.null(outcome)) {
    checkmate::assert_string(outcome, min.chars = 1)
    if (!outcome %in% base::names(vars)) {
      base::stop("validity_estimates: no column '", outcome, "'",
                 call. = FALSE)
    }
    if (!base::is.logical(vars[[outcome]])) {
      base::stop("validity_estimates: '", outcome, "' is not logical",
                 call. = FALSE)
    }
    keep <- !base::is.na(vars[[outcome]])
    design$variables$.y <- base::as.integer(vars[[outcome]])
  } else {
    statuses <- .validity_statuses(vars, status)
    checkmate::assert_choice(level, listing_status_levels())
    keep <- if (definitive_only) {
      statuses %in% c("confirmed_valid", "confirmed_invalid")
    } else {
      base::rep(TRUE, base::nrow(vars))
    }
    design$variables$.y <- base::as.integer(statuses == level)
  }
  .validity_check_weights(stats::weights(design, type = "sampling"))

  estimate_domain <- function(domain) {
    summary <- tibble::tibble(
      estimate = NA_real_, se = NA_real_, n = base::nrow(domain$variables),
      ci_low = NA_real_, ci_high = NA_real_, ci_status = "empty"
    )
    if (summary$n == 0L) return(summary)
    mean <- survey::svymean(~.y, domain)
    summary$estimate <- base::as.numeric(mean)
    summary$se <- base::as.numeric(survey::SE(mean))
    if (ci_method == "mean") {
      interval <- stats::confint(mean, level = level_ci)
      summary$ci_status <- "legacy_mean"
      summary$ci_low <- base::max(0, interval[1])
      summary$ci_high <- base::min(1, interval[2])
    } else if (summary$estimate %in% c(0, 1)) {
      summary$ci_status <- "boundary"
    } else if (!base::is.finite(summary$se) || summary$se <= 0) {
      summary$ci_status <- "undefined_variance"
    } else {
      proportion <- survey::svyciprop(
        ~.y, domain, method = "beta", level = level_ci
      )
      interval <- stats::confint(proportion)
      summary$ci_low <- base::as.numeric(interval[1])
      summary$ci_high <- base::as.numeric(interval[2])
      summary$ci_status <- "estimated"
    }
    summary
  }

  if (base::is.null(by)) {
    return(estimate_domain(base::subset(design, keep)))
  }
  groups <- base::sort(base::unique(base::as.character(vars[[by]])))
  summaries <- base::lapply(groups, function(group) {
    selected <- keep & !base::is.na(vars[[by]]) & vars[[by]] == group
    estimate_domain(base::subset(design, selected))
  })
  if (base::length(groups) == 0L) {
    estimates <- estimate_domain(base::subset(design, keep))[FALSE, ]
  } else {
    estimates <- base::do.call(base::rbind, summaries)
  }
  estimates[[by]] <- groups
  estimates[, c(by, base::setdiff(base::names(estimates), by))]
}

.validity_statuses <- function(sample, status) {
  checkmate::assert_string(status, min.chars = 1)
  if (!status %in% base::names(sample)) {
    base::stop("validity: no column '", status, "'", call. = FALSE)
  }
  statuses <- base::as.character(sample[[status]])
  if (base::anyNA(statuses) ||
      !base::all(statuses %in% listing_status_levels())) {
    base::stop(
      "validity: status column holds values outside listing_status_levels()",
      call. = FALSE
    )
  }
  statuses
}

.validity_check_weights <- function(weights) {
  checkmate::assert_numeric(weights, any.missing = FALSE, finite = TRUE)
  if (base::any(weights <= 0)) {
    base::stop("Every sampling weight must be positive.", call. = FALSE)
  }
  invisible(weights)
}

#' Extreme-case bounds for unresolved listings
#'
#' Identification bounds obtained by assigning every uncertain listing valid
#'   or invalid. These are not confidence intervals and do not measure
#'   sampling uncertainty. The observed rate uses definitive listings only.
#' @param df Data frame with a status column, or a survey.design object.
#' @param status Column name. Default `"listing_status"`.
#' @param weights Optional sampling-weight column name or numeric vector.
#'   NULL uses unit weights for data frames. A survey design uses its own
#'   sampling weights and must not also receive this argument.
#' @param denominator `"attempted"` (default) excludes not-attempted
#'   listings from bounds. `"all_sampled"` treats unresolved and
#'   not-attempted listings as uncertain.
#' @return A tibble with scenario, n_valid, denominator and pct_valid.
#'   With weights, n_valid and denominator are sums of sampling weights,
#'   not row counts; the weighted attribute is TRUE. Without weights the
#'   original counts, scenarios and output structure are preserved.
#'   The not_attempted attribute always reports the unweighted row count.
#'   When all eligible listings are uncertain the definitive rate is NA
#'   and bounds are 0 to 100 percent. Empty eligible populations have NA
#'   bounds.
#' @examples
#' calls <- data.frame(
#'   listing_status = listing_status_levels(), weight = c(8, 2, 6, 4))
#' validity_sensitivity_bounds(calls)
#' validity_sensitivity_bounds(
#'   calls, weights = "weight", denominator = "all_sampled")
#' @export
validity_sensitivity_bounds <- function(
    df, status = "listing_status", weights = NULL,
    denominator = c("attempted", "all_sampled")) {
  denominator <- base::match.arg(denominator)
  sample <- df
  weighted <- !base::is.null(weights)
  if (base::inherits(sample, "survey.design")) {
    if (weighted) {
      base::stop("Supply a survey design or weights, not both.", call. = FALSE)
    }
    weights <- stats::weights(sample, type = "sampling")
    sample <- sample$variables
    weighted <- TRUE
  }
  checkmate::assert_data_frame(sample)
  statuses <- .validity_statuses(sample, status)
  if (base::is.character(weights)) {
    checkmate::assert_string(weights, min.chars = 1)
    if (!weights %in% base::names(sample)) {
      base::stop("validity: no weight column '", weights, "'", call. = FALSE)
    }
    weights <- sample[[weights]]
  }
  if (base::is.null(weights)) weights <- base::rep(1, base::nrow(sample))
  .validity_check_weights(weights)
  checkmate::assert_numeric(weights, len = base::nrow(sample))
  valid <- base::sum(weights[statuses == "confirmed_valid"])
  invalid <- base::sum(weights[statuses == "confirmed_invalid"])
  uncertain <- base::sum(weights[statuses == "unresolved"])
  label <- "unresolved"
  if (denominator == "all_sampled") {
    uncertain <- uncertain +
      base::sum(weights[statuses == "not_attempted"])
    label <- "unresolved_or_unattempted"
  }
  definitive <- valid + invalid
  eligible <- definitive + uncertain
  bounds <- tibble::tibble(
    scenario = c(
      "observed_definitive_only",
      base::paste0("best_case_", label, "_valid"),
      base::paste0("worst_case_", label, "_invalid")
    ),
    n_valid = c(valid, valid + uncertain, valid),
    denominator = c(definitive, eligible, eligible)
  )
  bounds$pct_valid <- base::ifelse(
    bounds$denominator > 0, 100 * bounds$n_valid / bounds$denominator,
    NA_real_
  )
  base::attr(bounds, "not_attempted") <-
    base::sum(statuses == "not_attempted")
  if (weighted) base::attr(bounds, "weighted") <- TRUE
  bounds
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
#' @return A tibble with `term`, `estimate` (odds ratio), `conf.low`, `conf.high`.
#' @examples
#' calls <- data.frame(
#'   stratum = c("a", "a", "b", "b", "a", "b"), weight = c(2, 2, 1, 1, 2, 1),
#'   listing_status = c("confirmed_valid", "confirmed_invalid", "confirmed_valid",
#'                       "confirmed_invalid", "confirmed_valid", "confirmed_valid"),
#'   rural = c(TRUE, TRUE, FALSE, FALSE, FALSE, TRUE))
#' design <- listing_validity_design(calls)
#' validity_model(design, ~ rural)
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
  ci <- exp(stats::confint(fit))
  tibble::tibble(term = names(coef(fit)), estimate = unname(exp(coef(fit))),
                 conf.low = unname(ci[, 1]), conf.high = unname(ci[, 2]))
}
