#' Validation cascade with both denominators
#'
#' Each stage of a verification cascade (attempted, reached, location
#' confirmed, workforce confirmed, ...) is reported two ways: as a share of
#' the first stage (the attempted-listing denominator) and conditional on the
#' stage before it. Stages are logical columns given in order; a `TRUE` at a
#' stage must imply `TRUE` at every earlier stage, otherwise the call stops.
#' Counts are unweighted unless `weights` names a column, in which case the
#' counts are weighted sums (the unweighted `n` is kept alongside).
#'
#' @param df Sampled listings.
#' @param stages Character vector of logical column names, in cascade order.
#'   `NA` counts as `FALSE`.
#' @param by Optional grouping column (for example `"stratum"`).
#' @param weights Optional weight column (for example `"weight"`).
#' @return A tibble with `group`, `stage`, `n`, `count` (weighted if
#'   `weights` given, otherwise equal to `n`), `pct_of_first` and
#'   `pct_of_prior` (percent; `NA` where the denominator is zero).
#' @export
validation_cascade <- function(df, stages, by = NULL, weights = NULL) {
  checkmate::assert_data_frame(df)
  checkmate::assert_character(stages, min.len = 1, unique = TRUE)
  for (col in c(stages, by, weights)) {
    if (!col %in% names(df)) stop(sprintf("validation_cascade: no column '%s'", col), call. = FALSE)
  }
  flags <- lapply(stages, function(s) {
    v <- df[[s]]
    if (!is.logical(v)) stop(sprintf("validation_cascade: '%s' is not logical", s), call. = FALSE)
    v %in% TRUE
  })
  for (i in seq_along(flags)[-1]) {
    if (any(flags[[i]] & !flags[[i - 1]])) {
      stop(sprintf("validation_cascade: '%s' is TRUE where '%s' is not; stages must nest",
                   stages[i], stages[i - 1]), call. = FALSE)
    }
  }
  w <- if (is.null(weights)) rep(1, nrow(df)) else df[[weights]]
  one <- function(idx, label) {
    n     <- vapply(flags, function(f) sum(f[idx]), numeric(1))
    count <- vapply(flags, function(f) sum(w[idx][f[idx]]), numeric(1))
    prior <- c(NA_real_, count[-length(count)])
    pct <- function(num, den) { den <- rep_len(den, length(num)); ifelse(!is.na(den) & den > 0, 100 * num / den, NA_real_) }
    tibble::tibble(group = label, stage = stages, n = as.integer(n), count = count,
                   pct_of_first = pct(count, count[1]), pct_of_prior = pct(count, prior))
  }
  res <- one(seq_len(nrow(df)), "all")
  if (!is.null(by)) {
    g <- as.character(df[[by]])
    res <- dplyr::bind_rows(res, dplyr::bind_rows(lapply(sort(unique(g)), function(k) one(which(g == k), k))))
  }
  res
}
