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
#' @return The sampled rows with the four design columns added. Attribute
#'   `allocation` holds the per-stratum table (`stratum`, `pop_N`,
#'   `n_sampled`, `samp_frac`, `weight`).
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
  alloc <- tibble::tibble(
    stratum   = names(allocation),
    pop_N     = as.integer(pop[names(allocation)]),
    n_sampled = as.integer(allocation)
  )
  alloc$samp_frac <- alloc$n_sampled / alloc$pop_N
  alloc$weight    <- alloc$pop_N / alloc$n_sampled
  rows <- withr::with_seed(seed, {
    unlist(lapply(seq_len(nrow(alloc)), function(i) {
      idx <- which(strata == alloc$stratum[i])
      idx[sample.int(length(idx), alloc$n_sampled[i])]
    }), use.names = FALSE)
  })
  out <- frame[rows, , drop = FALSE]
  key <- match(as.character(out[[stratum]]), alloc$stratum)
  out$pop_N     <- alloc$pop_N[key]
  out$n_sampled <- alloc$n_sampled[key]
  out$samp_frac <- alloc$samp_frac[key]
  out$weight    <- alloc$weight[key]
  attr(out, "allocation") <- alloc
  attr(out, "seed") <- seed
  tibble::as_tibble(out)
}
