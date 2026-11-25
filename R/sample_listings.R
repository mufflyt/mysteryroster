#' Draw a stratified listing sample that preserves sampling probabilities
#'
#' Samples without replacement within each stratum and records, on every
#' sampled row, the stratum population `pop_N`, the stratum sample size
#' `n_sampled`, the sampling fraction `samp_frac` and the design weight
#' `weight = pop_N / n_sampled`. The allocation is checked before any draw: an
#' allocation larger than a stratum's population, or naming a stratum the
#' frame does not contain, stops the call.
#'
#' @param frame Data frame of listings, one row per listing.
#' @param allocation Named integer vector: sample size per stratum. Every name
#'   must be a level of `stratum`; strata not named are not sampled.
#' @param stratum Name of the stratum column. Default `"stratum"`.
#' @param seed Integer seed, required so the draw is reproducible.
#' @return The sampled rows with the four design columns added by
#'   [listing_weights()]; attribute `allocation` holds the per-stratum table.
#' @examples
#' frame <- data.frame(id = 1:10, stratum = rep(c("a", "b"), 5))
#' stratified_listing_sample(frame, allocation = c(a = 2, b = 2), seed = 20261104)
#' @export
stratified_listing_sample <- function(frame, allocation, stratum = "stratum", seed) {
  checkmate::assert_data_frame(frame, min.rows = 1)
  checkmate::assert_string(stratum)
  if (!stratum %in% names(frame)) stop(sprintf("stratified_listing_sample: no column '%s'", stratum), call. = FALSE)
  checkmate::assert_integerish(allocation, lower = 1, names = "unique", min.len = 1, any.missing = FALSE)
  checkmate::assert_int(seed)
  strata <- as.character(frame[[stratum]])
  pop <- table(strata)
  missing <- setdiff(names(allocation), names(pop))
  if (length(missing)) {
    stop(sprintf("stratified_listing_sample: allocation names strata absent from the frame: %s",
                 paste(missing, collapse = ", ")), call. = FALSE)
  }
  over <- names(allocation)[allocation > pop[names(allocation)]]
  if (length(over)) {
    stop(sprintf("stratified_listing_sample: allocation exceeds the stratum population for: %s",
                 paste(sprintf("%s (%d > %d)", over, allocation[over], pop[over]), collapse = ", ")),
         call. = FALSE)
  }
  rows <- local({
    old <- if (exists(".Random.seed", globalenv())) get(".Random.seed", globalenv()) else NULL
    on.exit(if (is.null(old)) rm(".Random.seed", envir = globalenv()) else assign(".Random.seed", old, globalenv()))
    set.seed(seed)
    unlist(lapply(names(allocation), function(s) {
      idx <- which(strata == s)
      idx[sample.int(length(idx), allocation[[s]])]
    }), use.names = FALSE)
  })
  listing_weights(frame[rows, , drop = FALSE],
                  pop_N = setNames(as.integer(pop[names(allocation)]), names(allocation)),
                  stratum = stratum)
}

#' Design a stratified verification sample with Neyman, cost-optimal, or proportional allocation
#'
#' Before drawing a sample with [stratified_listing_sample()] for an independent
#' practice-location verification audit (such as a telephone secret-shopper
#' survey, institutional directory check, or medical-record verification),
#' investigators must decide how many listings to sample overall and how to
#' allocate that sample across administrative strata (for example, the
#' specificity tiers from [attribute_location_specificity()] or the consensus
#' tiers from [attribute_consensus_tier()]).
#'
#' This function computes the required total sample size (from a target margin
#' of error `target_moe` or a fixed `total_n`), allocates the sample across
#' strata using finite-population-corrected **Neyman**, **cost-optimal**,
#' **proportional**, or **equal** allocation, inflates the fielded sample size
#' for anticipated nonresponse (`response_rate`), and projects the expected
#' width of the worst-case/best-case Manski sensitivity bounds under
#' [validity_sensitivity_bounds()].
#'
#' @param frame Either a listing-level data frame containing `stratum`, or a
#'   stratum-level summary data frame containing `stratum` and `pop_N`.
#' @param stratum Name of the stratum column in `frame`. Default `"stratum"`.
#' @param pop_col Optional name of the population count column if `frame` is
#'   already aggregated to one row per stratum. Default `"pop_N"`.
#' @param allocation_method One of `"neyman"` (minimizes variance for fixed
#'   sample size), `"cost_optimal"` (minimizes variance for fixed budget /
#'   weighted by `1 / sqrt(unit_cost)`), `"proportional"` (self-weighting
#'   proportional to `pop_N`), or `"equal"` (equal allocation across strata).
#' @param target_moe Target half-width of the `conf_level` confidence interval
#'   for the overall stratified validity proportion (e.g. `0.03` for `+/- 3.0`
#'   percentage points). Ignored when `total_n` is supplied explicitly.
#' @param total_n Optional fixed total number of *completed* verifications
#'   across all strata. When `NULL` (default), `total_n` is solved from
#'   `target_moe` with finite-population correction.
#' @param prior_p Numeric scalar or named numeric vector (by stratum) of
#'   anticipated validity proportions in `(0, 1)`. Default `0.50` (conservative
#'   maximum-variance prior).
#' @param unit_cost Numeric scalar or named numeric vector (by stratum) of
#'   per-listing verification cost (in dollars or relative effort units).
#'   Default `1.0`.
#' @param response_rate Numeric scalar or named numeric vector (by stratum) of
#'   anticipated verification completion rates in `(0, 1]`. Default `0.80`.
#'   Used to inflate the fielded sample (`n_fielded = ceiling(n_complete / response_rate)`)
#'   and to compute the expected width of worst/best-case unresolved-outcome
#'   sensitivity bounds.
#' @param min_stratum_n Minimum number of completed verifications per stratum
#'   (capped at the stratum population `pop_N`). Default `5L`.
#' @param conf_level Confidence level in `(0, 1)`. Default `0.95`.
#' @return A list with three elements:
#'   \describe{
#'     \item{`strata`}{Tibble with one row per stratum: `stratum`, `pop_N`,
#'       `pop_share`, `prior_p`, `prior_sd`, `unit_cost`, `response_rate`,
#'       `n_complete`, `n_fielded`, `sampling_fraction`, `design_weight`, and
#'       `stratum_moe`.}
#'     \item{`design_summary`}{One-row tibble summarizing the overall survey
#'       design: `allocation_method`, `N_total`, `n_complete_total`,
#'       `n_fielded_total`, `expected_total_cost`, `expected_overall_p`,
#'       `expected_se`, `expected_moe`, `design_effect_deff`,
#'       `expected_manski_lower`, `expected_manski_upper`, and
#'       `expected_manski_width`.}
#'     \item{`allocation_vector`}{Named integer vector of fielded sample sizes
#'       (`n_fielded` named by `stratum`), ready to pass directly to
#'       [stratified_listing_sample()].}
#'   }
#' @examples
#' frame <- data.frame(
#'   npi = as.character(1:1000),
#'   stratum = c(rep("tier1_pinpoint", 600), rep("tier2_multisite", 300), rep("tier3_discordant", 100))
#' )
#' des <- design_verification_sample(
#'   frame,
#'   allocation_method = "cost_optimal",
#'   target_moe = 0.04,
#'   prior_p = c(tier1_pinpoint = 0.90, tier2_multisite = 0.60, tier3_discordant = 0.30),
#'   unit_cost = c(tier1_pinpoint = 1, tier2_multisite = 2, tier3_discordant = 4),
#'   response_rate = 0.85
#' )
#' des$strata
#' des$design_summary
#' samp <- stratified_listing_sample(frame, allocation = des$allocation_vector, seed = 42)
#' @seealso [stratified_listing_sample()], [listing_weights()],
#'   [validity_estimates()], [validity_sensitivity_bounds()].
#' @export
design_verification_sample <- function(frame,
                                       stratum = "stratum",
                                       pop_col = "pop_N",
                                       allocation_method = c("neyman", "cost_optimal", "proportional", "equal"),
                                       target_moe = 0.03,
                                       total_n = NULL,
                                       prior_p = 0.50,
                                       unit_cost = 1.0,
                                       response_rate = 0.80,
                                       min_stratum_n = 5L,
                                       conf_level = 0.95) {
  checkmate::assert_data_frame(frame, min.rows = 1)
  checkmate::assert_string(stratum, min.chars = 1)
  if (!stratum %in% names(frame)) {
    stop(sprintf("design_verification_sample: frame is missing column '%s'.", stratum), call. = FALSE)
  }
  allocation_method <- match.arg(allocation_method)
  checkmate::assert_number(conf_level, lower = 0, upper = 1)
  if (conf_level <= 0 || conf_level >= 1) {
    stop("design_verification_sample: conf_level must be strictly between 0 and 1.", call. = FALSE)
  }
  checkmate::assert_int(min_stratum_n, lower = 1L)

  if (!is.null(pop_col) && (pop_col %in% names(frame)) && !anyDuplicated(frame[[stratum]])) {
    s_names <- trimws(as.character(frame[[stratum]]))
    N_h <- as.integer(frame[[pop_col]])
    names(N_h) <- s_names
  } else {
    s_vec <- trimws(as.character(frame[[stratum]]))
    s_vec <- s_vec[!is.na(s_vec) & nzchar(s_vec)]
    tab <- table(s_vec)
    N_h <- as.integer(tab)
    names(N_h) <- names(tab)
  }
  if (any(is.na(N_h) | N_h <= 0L)) {
    stop("design_verification_sample: stratum population counts must be positive integers.", call. = FALSE)
  }
  strata_names <- names(N_h)
  H <- length(strata_names)
  N_tot <- sum(N_h)
  W_h <- N_h / N_tot

  expand_param <- function(x, label, lower, upper, strict_lower = FALSE, strict_upper = FALSE) {
    if (length(x) == 1L && is.null(names(x))) {
      res <- rep(as.numeric(x), H)
      names(res) <- strata_names
    } else if (!is.null(names(x))) {
      miss <- setdiff(strata_names, names(x))
      if (length(miss) > 0L) {
        stop(sprintf("design_verification_sample: %s is missing value(s) for strata: %s.",
                     label, paste(miss, collapse = ", ")), call. = FALSE)
      }
      res <- as.numeric(x[strata_names])
      names(res) <- strata_names
    } else if (length(x) == H) {
      res <- as.numeric(x)
      names(res) <- strata_names
    } else {
      stop(sprintf("design_verification_sample: %s must be length 1 or named by stratum.", label), call. = FALSE)
    }
    if (anyNA(res) ||
        (strict_lower && any(res <= lower)) || (!strict_lower && any(res < lower)) ||
        (strict_upper && any(res >= upper)) || (!strict_upper && any(res > upper))) {
      stop(sprintf("design_verification_sample: %s values are out of bounds.", label), call. = FALSE)
    }
    res
  }

  p_h <- expand_param(prior_p, "prior_p", 0, 1, strict_lower = TRUE, strict_upper = TRUE)
  c_h <- expand_param(unit_cost, "unit_cost", 0, Inf, strict_lower = TRUE, strict_upper = TRUE)
  r_h <- expand_param(response_rate, "response_rate", 0, 1, strict_lower = TRUE, strict_upper = FALSE)

  # Finite population variance S_h = sqrt(N_h / (N_h - 1) * p_h * (1 - p_h))
  S_h <- ifelse(N_h > 1L, sqrt((N_h / (N_h - 1)) * p_h * (1 - p_h)), sqrt(p_h * (1 - p_h)))
  S_h <- pmax(S_h, 1e-4)

  raw_shares <- switch(
    allocation_method,
    neyman = W_h * S_h,
    cost_optimal = (W_h * S_h) / sqrt(c_h),
    proportional = W_h,
    equal = rep(1 / H, H)
  )
  alloc_weights <- raw_shares / sum(raw_shares)

  z_crit <- stats::qnorm(1 - (1 - conf_level) / 2)

  if (is.null(total_n)) {
    checkmate::assert_number(target_moe, lower = 1e-4, upper = 0.50, finite = TRUE)
    V_target <- (target_moe / z_crit)^2
    # Stratified variance: V = (1 / n) * sum(W_h^2 * S_h^2 / a_h) - (1 / N) * sum(W_h * S_h^2)
    num_term <- sum((W_h^2 * S_h^2) / alloc_weights)
    fpc_term <- sum(W_h * S_h^2) / N_tot
    n_req <- ceiling(num_term / (V_target + fpc_term))
    n_req <- max(as.integer(n_req), H * min_stratum_n)
    total_n <- min(n_req, N_tot)
  } else {
    checkmate::assert_int(total_n, lower = 1L)
    total_n <- min(as.integer(total_n), N_tot)
  }

  # Allocate across strata with floor at min(min_stratum_n, N_h) and cap at N_h
  n_comp <- pmax(as.integer(round(total_n * alloc_weights)), pmin(as.integer(min_stratum_n), N_h))
  n_comp <- pmin(n_comp, N_h)

  # Iterative Neyman/proportional overflow redistribution if any stratum hit N_h cap
  rem <- total_n - sum(n_comp)
  if (rem > 0L && any(n_comp < N_h)) {
    free <- which(n_comp < N_h)
    add_w <- alloc_weights[free] / sum(alloc_weights[free])
    n_comp[free] <- pmin(N_h[free], n_comp[free] + as.integer(round(rem * add_w)))
  }
  names(n_comp) <- strata_names

  # Fielded sample size inflated for anticipated nonresponse, capped at N_h
  n_field <- pmin(N_h, as.integer(ceiling(n_comp / r_h)))
  names(n_field) <- strata_names
  # Realized expected completes given cap at N_h
  n_comp_eff <- pmin(n_comp, as.integer(floor(n_field * r_h)))
  n_comp_eff <- pmax(1L, n_comp_eff)

  f_h <- n_comp_eff / N_h
  var_h <- ifelse(N_h > 1L, (1 - f_h) * (S_h^2) / n_comp_eff, 0)
  stratum_moe <- z_crit * sqrt(var_h)

  overall_p <- sum(W_h * p_h)
  overall_var <- sum((W_h^2) * var_h)
  overall_se <- sqrt(overall_var)
  overall_moe <- z_crit * overall_se

  # Compare to simple random sampling variance of same total n_comp_eff
  n_tot_comp <- sum(n_comp_eff)
  p_srs <- overall_p
  var_srs <- (1 - n_tot_comp / N_tot) * (p_srs * (1 - p_srs)) / max(1L, n_tot_comp)
  deff <- if (var_srs > 0) overall_var / var_srs else 1.0

  # Expected Manski worst-case (all nonrespondents invalid) and best-case (all nonrespondents valid)
  overall_r <- sum(W_h * r_h)
  manski_low <- sum(W_h * (r_h * p_h))
  manski_high <- sum(W_h * (r_h * p_h + (1 - r_h)))
  manski_width <- manski_high - manski_low

  strata_tbl <- tibble::tibble(
    stratum = strata_names,
    pop_N = as.integer(N_h),
    pop_share = round(as.numeric(W_h), 4),
    prior_p = round(as.numeric(p_h), 4),
    prior_sd = round(as.numeric(S_h), 4),
    unit_cost = round(as.numeric(c_h), 2),
    response_rate = round(as.numeric(r_h), 4),
    n_complete = as.integer(n_comp_eff),
    n_fielded = as.integer(n_field),
    sampling_fraction = round(as.numeric(n_field / N_h), 4),
    design_weight = round(as.numeric(N_h / n_field), 2),
    stratum_moe = round(as.numeric(stratum_moe), 4)
  )

  summary_tbl <- tibble::tibble(
    allocation_method = allocation_method,
    N_total = as.integer(N_tot),
    n_complete_total = as.integer(sum(n_comp_eff)),
    n_fielded_total = as.integer(sum(n_field)),
    expected_total_cost = round(sum(n_field * c_h), 2),
    expected_overall_p = round(overall_p, 4),
    expected_se = round(overall_se, 4),
    expected_moe = round(overall_moe, 4),
    design_effect_deff = round(deff, 3),
    expected_response_rate = round(overall_r, 4),
    expected_manski_lower = round(manski_low, 4),
    expected_manski_upper = round(manski_high, 4),
    expected_manski_width = round(manski_width, 4)
  )

  list(
    strata = strata_tbl,
    design_summary = summary_tbl,
    allocation_vector = n_field
  )
}
