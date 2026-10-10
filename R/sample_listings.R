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
