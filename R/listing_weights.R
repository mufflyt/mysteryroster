#' Attach design weights to an already-collected listing sample
#'
#' For a sample that was not drawn with [stratified_listing_sample()] (for
#' example a completed call log), attaches the same four design columns from
#' a stratum population table: `pop_N`, `n_sampled` (rows per stratum in
#' `df`), `samp_frac` and `weight = pop_N / n_sampled`. A stratum in `df`
#' that `pop_N` does not name, or a stratum whose sample exceeds its
#' population, stops the call.
#'
#' @param df Sampled listings, one row per listing.
#' @param pop_N Named integer vector: population size per stratum, for
#'   example the four-zone table from [overlap_zone_counts()].
#' @param stratum Name of the stratum column. Default `"stratum"`.
#' @return `df` as a tibble with the four design columns added; attribute
#'   `allocation` holds the per-stratum table.
#' @export
listing_weights <- function(df, pop_N, stratum = "stratum") {
  checkmate::assert_data_frame(df, min.rows = 1)
  checkmate::assert_integerish(pop_N, lower = 1, names = "unique", min.len = 1, any.missing = FALSE)
  if (!stratum %in% names(df)) stop(sprintf("listing_weights: no column '%s'", stratum), call. = FALSE)
  strata <- as.character(df[[stratum]])
  n <- table(strata)
  missing <- setdiff(names(n), names(pop_N))
  if (length(missing)) {
    stop(sprintf("listing_weights: pop_N has no population for strata: %s",
                 paste(missing, collapse = ", ")), call. = FALSE)
  }
  over <- names(n)[as.integer(n) > pop_N[names(n)]]
  if (length(over)) {
    stop(sprintf("listing_weights: sample exceeds the stratum population for: %s",
                 paste(sprintf("%s (%d > %d)", over, n[over], pop_N[over]), collapse = ", ")),
         call. = FALSE)
  }
  alloc <- tibble::tibble(stratum = names(n), pop_N = as.integer(pop_N[names(n)]),
                          n_sampled = as.integer(n))
  alloc$samp_frac <- alloc$n_sampled / alloc$pop_N
  alloc$weight    <- alloc$pop_N / alloc$n_sampled
  key <- match(strata, alloc$stratum)
  df$pop_N     <- alloc$pop_N[key]
  df$n_sampled <- alloc$n_sampled[key]
  df$samp_frac <- alloc$samp_frac[key]
  df$weight    <- alloc$weight[key]
  attr(df, "allocation") <- alloc
  tibble::as_tibble(df)
}
