#' Study-flow counts by stratum, with closure checks
#'
#' Counts sampled, attempted, reached, definitive, confirmed valid, confirmed
#' invalid, unresolved and not attempted, overall and per group, and stops if
#' the categories do not partition the sample.
#'
#' @param df Sampled listings with a status column and a reached column.
#' @param status Status column, values from [listing_status_levels()].
#' @param reached Logical column: `TRUE` reached, `FALSE` attempted but not
#'   reached, `NA` not attempted.
#' @param by Optional grouping column.
#' @return A tibble, one row overall (group `"all"`) plus one per group.
#' @export
listing_flow_counts <- function(df, status = "listing_status", reached = "reached", by = NULL) {
  checkmate::assert_data_frame(df)
  for (col in c(status, reached, by)) {
    if (!col %in% names(df)) stop(sprintf("listing_flow_counts: no column '%s'", col), call. = FALSE)
  }
  one <- function(d, label) {
    s <- as.character(d[[status]]); r <- d[[reached]]
    out <- tibble::tibble(
      group             = label,
      sampled           = nrow(d),
      not_attempted     = sum(s == "not_attempted"),
      attempted         = sum(!is.na(r)),
      reached           = sum(r %in% TRUE),
      confirmed_valid   = sum(s == "confirmed_valid"),
      confirmed_invalid = sum(s == "confirmed_invalid"),
      unresolved        = sum(s == "unresolved")
    )
    out$definitive <- out$confirmed_valid + out$confirmed_invalid
    if (out$confirmed_valid + out$confirmed_invalid + out$unresolved + out$not_attempted != out$sampled) {
      stop(sprintf("listing_flow_counts: statuses do not partition the sample in group '%s'", label), call. = FALSE)
    }
    if (out$attempted + sum(is.na(r)) != out$sampled) {
      stop(sprintf("listing_flow_counts: attempted + not attempted != sampled in group '%s'", label), call. = FALSE)
    }
    out
  }
  res <- one(df, "all")
  if (!is.null(by)) {
    groups <- split(df, as.character(df[[by]]))
    res <- dplyr::bind_rows(res, dplyr::bind_rows(lapply(names(groups), function(g) one(groups[[g]], g))))
  }
  res
}
