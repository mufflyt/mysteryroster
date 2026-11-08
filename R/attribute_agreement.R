#' A record's age, in years, as of a given date
#'
#' Ported from isochrones' `e1_last_update_staleness.R` (commit `48dff9a27`)
#' via `mysterymedicaid::mm_record_age_years()`. A record's own last-edited
#' date is a property of the record alone -- it takes no independent
#' corroborating source, and by itself is a weak staleness signal. Use it to
#' characterize how far back a dataset's evidence reaches, not to accept or
#' reject one record.
#'
#' @param last_updated The record's own last-edited date (`Date`, or an
#'   ISO-8601 character vector -- only the first 10 characters are read, so a
#'   full timestamp like `"2022-12-15T22:10:57.818+00:00"` works directly).
#' @param as_of The date(s) to measure age against. Defaults to today; pass
#'   the date the record was actually *used* (an audit year's end, say) when
#'   reconstructing historical applicability rather than current staleness.
#' @return Numeric years, possibly fractional. `NA` where either input is
#'   `NA` or unparseable. A `last_updated` after `as_of` gives a negative age.
#' @examples
#' record_age_years(c("2024-01-01", "2019-06-30T08:00:00Z", NA), as_of = "2025-01-01")
#' @family no-gold-standard comparison
#' @export
record_age_years <- function(last_updated, as_of = Sys.Date()) {
  date_part <- substr(as.character(last_updated), 1, 10)
  date_part[!grepl("^\\d{4}-\\d{2}-\\d{2}$", date_part)] <- NA
  last_updated <- as.Date(date_part)
  as_of <- as.Date(as_of)
  as.numeric(as_of - last_updated) / 365.25
}

#' Bucket record age into isochrones' own age bands
#'
#' The exact cut points `e1_last_update_staleness.R` used, ported as-is for
#' comparability with that analysis rather than chosen fresh here.
#'
#' Intervals are right-closed: exactly 1 year old is `"<1 yr"`, exactly 3 is
#' `"1-3 yr"`, exactly 5 is `"3-5 yr"`.
#'
#' @param last_updated,as_of See [record_age_years()].
#' @return A factor with levels `"<1 yr"`, `"1-3 yr"`, `"3-5 yr"`, `"5+ yr"`, or
#'   `NA` where age is `NA`.
#' @examples
#' record_age_band(c("2024-06-01", "2022-01-01", "2020-01-01", "2010-01-01"),
#'                 as_of = "2024-12-31")
#' @family no-gold-standard comparison
#' @export
record_age_band <- function(last_updated, as_of = Sys.Date()) {
  age <- record_age_years(last_updated, as_of)
  cut(age, breaks = c(-Inf, 1, 3, 5, Inf), labels = c("<1 yr", "1-3 yr", "3-5 yr", "5+ yr"))
}

#' Classify which of two disagreeing records is provably the older information
#'
#' There is no gold standard for which value is correct -- only an
#' independent, dated basis for saying one record *cannot* reflect
#' information the other already had. If record A's own last-updated date
#' predates record B's, A provably cannot encode whatever changed by B's
#' date; this does not make B correct, only older information strictly
#' excluded as current. Ported from `mysterymedicaid::mm_classify_record_staleness()`,
#' made symmetric (pairwise, no reference date) for cross-source attribute
#' comparison.
#'
#' @param updated_a,updated_b The two records' own last-edited dates (`Date`,
#'   or coercible via `as.Date()`).
#' @return One of `"a_predates_b"`, `"b_predates_a"` (the named side is
#'   provably the older information), `"same_date"` (no temporal basis to
#'   prefer either), or `"no_temporal_basis"` (either date is missing).
#'   Vectorized; `NA` dates never raise an error.
#' @examples
#' classify_record_staleness(
#'   updated_a = c("2015-03-01", "2024-05-01", "2024-05-01", NA),
#'   updated_b = c("2024-05-01", "2015-03-01", "2024-05-01", "2024-05-01")
#' )
#' @family no-gold-standard comparison
#' @export
classify_record_staleness <- function(updated_a, updated_b) {
  updated_a <- as.Date(updated_a)
  updated_b <- as.Date(updated_b)
  dplyr::case_when(
    is.na(updated_a) | is.na(updated_b) ~ "no_temporal_basis",
    updated_a < updated_b ~ "a_predates_b",
    updated_a > updated_b ~ "b_predates_a",
    TRUE ~ "same_date"
  )
}

#' Pairwise cross-source agreement on a shared attribute, freshness-tuned
#'
#' Takes [registry_overlap()]'s cross-linking a step further: instead of only
#' asking whether an identifier is *present* in each source, this asks
#' whether sources that share an identifier *agree on an attribute value*
#' (for example practice state). There is no gold standard -- every pair of
#' sources is compared only against each other, never against an assumed
#' truth -- so the output is pairwise, not a single per-identifier verdict.
#'
#' Each source's own freshness date (its record's last-updated date, or a
#' vintage/snapshot date -- never the date it happened to be downloaded) is
#' carried through so a disagreement can be read against
#' [classify_record_staleness()]: a disagreement between a record last
#' touched in 2015 and one from 2024 is not the same finding as one between
#' two records both touched in 2024. A source with no such field (common for
#' a scraped roster or a static extract) is still compared; its staleness
#' classification is simply `"no_temporal_basis"` for every row.
#'
#' @details
#' **Pairs.** With `k` sources there are `choose(k, 2)` pairs, taken in the
#' order the sources are listed: `source_a` is always the earlier name in
#' `sources`, so `staleness == "a_predates_b"` means the earlier-listed
#' source's record is the older one. A pair that shares no identifier-year
#' contributes no rows (it is skipped, not an error); the call errors only if
#' no pair overlaps at all.
#'
#' **Normalization.** Identifiers are trimmed; attribute values are trimmed
#' and upper-cased, so `" co"` and `"CO"` agree. Rows with a missing or empty
#' identifier or attribute value are dropped before pairing, so `n_compared`
#' in [attribute_agreement_summary()] counts only identifier-years both
#' sources actually report.
#'
#' **Years.** Rows pair on `(id, year)`. A source that lacks the `year`
#' column gets `year = NA` and therefore pairs only with other sources that
#' also lack it; pass `year = NULL` for a pure cross-section.
#'
#' **Freshness.** `staleness` compares the two records' own dates;
#' `age_years_*`/`age_band_*` measure each record against one `as_of` date.
#' `as_of` is a single date for the whole call, so for a multi-year panel
#' call once per year with that year's reference date and bind the results
#' (see `vignette("attribute-agreement")`).
#'
#' **Reading a disagreement.** `"a_predates_b"` says only that A's record
#' was last touched before B's, so A cannot reflect whatever changed in
#' between. It does not say B is correct, and an `agree == TRUE` row is not
#' evidence that either value is true -- two sources can agree on a stale
#' value, especially when one copies the other.
#'
#' @param sources Named list, same shape as [registry_overlap()]'s argument
#'   but attribute-bearing: each element is a data frame with an identifier
#'   column, the attribute column, and (optionally) a freshness-date column
#'   and a year column. Names are the source labels and must be unique.
#' @param attribute Name of the attribute column to compare (e.g. `"state"`).
#'   Values are trimmed and upper-cased before comparison.
#' @param id Name of the identifier column. Default `"npi"`.
#' @param year Name of the year column, or `NULL` if the comparison is a
#'   single cross-section (all rows are treated as one unlabeled year).
#' @param observed_at Name of the freshness-date column, or `NULL` if no
#'   source carries one (every row's staleness classification is then
#'   `"no_temporal_basis"`).
#' @param as_of Date each source's record age is measured against (passed to
#'   [record_age_years()]/[record_age_band()]). Default today. Pass the
#'   audit year's own reference date when backfilling a historical audit so
#'   "how stale was this record for that year" is answered correctly instead
#'   of against today.
#' @return A tibble, one row per `(id, year, source_a, source_b)` where both
#'   sources have a non-missing attribute value for that identifier-year:
#'   `value_a`, `value_b`, `agree` (logical), `observed_at_a`,
#'   `observed_at_b`, `staleness` (see [classify_record_staleness()]),
#'   `age_years_a`, `age_band_a`, `age_years_b`, `age_band_b`. Attribute
#'   `sources` holds the source names in order.
#' @examples
#' s <- list(
#'   nppes = data.frame(npi = c("1", "2"), year = 2024, state = c("CO", "CO"),
#'                       last_updated = c("2023-01-01", "2020-06-01")),
#'   board = data.frame(npi = c("1", "2"), year = 2024, state = c("CO", "NM"),
#'                       last_updated = c("2024-05-01", "2019-01-01"))
#' )
#' ag <- attribute_agreement(s, attribute = "state", observed_at = "last_updated",
#'                           as_of = "2024-12-31")
#' ag[, c("id", "value_a", "value_b", "agree", "staleness", "age_band_a", "age_band_b")]
#' attribute_agreement_summary(ag)
#' @seealso [registry_overlap()] for membership-only cross-linking, and
#'   `vignette("attribute-agreement")` for a worked example with figures.
#' @family no-gold-standard comparison
#' @export
attribute_agreement <- function(sources, attribute, id = "npi", year = "year",
                                 observed_at = NULL, as_of = Sys.Date()) {
  checkmate::assert_list(sources, min.len = 2, names = "unique")
  checkmate::assert_string(attribute, min.chars = 1)
  checkmate::assert_string(id, min.chars = 1)
  nm <- names(sources)

  long <- vector("list", length(nm))
  names(long) <- nm
  for (s in nm) {
    x <- sources[[s]]
    checkmate::assert_data_frame(x)
    for (col in c(id, attribute)) {
      if (!col %in% names(x)) {
        stop(sprintf("attribute_agreement: source '%s' has no column '%s'", s, col), call. = FALSE)
      }
    }
    row <- tibble::tibble(
      id = trimws(as.character(x[[id]])),
      year = if (!is.null(year) && year %in% names(x)) x[[year]] else NA,
      value = toupper(trimws(as.character(x[[attribute]]))),
      observed_at = if (!is.null(observed_at) && observed_at %in% names(x)) {
        as.character(x[[observed_at]])
      } else {
        NA_character_
      }
    )
    row <- row[!is.na(row$value) & nzchar(row$value) & !is.na(row$id) & nzchar(row$id), ]
    row$source <- s
    long[[s]] <- row
  }
  long <- dplyr::bind_rows(long)
  if (!nrow(long)) stop("attribute_agreement: no non-missing attribute values in any source", call. = FALSE)

  pairs <- utils::combn(nm, 2, simplify = FALSE)
  out <- vector("list", length(pairs))
  for (i in seq_along(pairs)) {
    sa <- pairs[[i]][1]; sb <- pairs[[i]][2]
    la <- long[long$source == sa, c("id", "year", "value", "observed_at")]
    lb <- long[long$source == sb, c("id", "year", "value", "observed_at")]
    names(la) <- c("id", "year", "value_a", "observed_at_a")
    names(lb) <- c("id", "year", "value_b", "observed_at_b")
    m <- merge(la, lb, by = c("id", "year"))
    if (!nrow(m)) next
    m$source_a <- sa
    m$source_b <- sb
    m$agree <- m$value_a == m$value_b
    m$staleness <- classify_record_staleness(m$observed_at_a, m$observed_at_b)
    m$age_years_a <- record_age_years(m$observed_at_a, as_of)
    m$age_band_a <- record_age_band(m$observed_at_a, as_of)
    m$age_years_b <- record_age_years(m$observed_at_b, as_of)
    m$age_band_b <- record_age_band(m$observed_at_b, as_of)
    out[[i]] <- m
  }
  out <- dplyr::bind_rows(out)
  if (!nrow(out)) {
    stop("attribute_agreement: no identifier-year is covered by more than one source", call. = FALSE)
  }
  out <- tibble::as_tibble(out[, c("id", "year", "source_a", "source_b", "value_a", "value_b", "agree",
                                   "observed_at_a", "observed_at_b", "staleness",
                                   "age_years_a", "age_band_a", "age_years_b", "age_band_b")])
  attr(out, "sources") <- nm
  out
}

#' Pairwise cross-source agreement when sources may report more than one
#' value per identifier-year
#'
#' [attribute_agreement()] assumes each source has at most one value per
#' `(id, year)`; merging two sources that violate that assumption produces an
#' uncontrolled cartesian product, not a comparison (two rows on one side and
#' one on the other becomes two compared rows, each pairing the single value
#' against one of the pair, with no row saying whether the two *sets*
#' actually matched). This is the explicit set-based alternative for
#' attributes that can legitimately take more than one value from the same
#' source in the same identifier-year -- for example a physician with two
#' practice locations reported by the same registry in the same year. Every
#' source contributes the *set* of distinct values it reports for each
#' `(id, year)`, and two sources are compared on that set, not value-by-value.
#'
#' @details
#' **Two agreement measures, not one.** OIG-style directory audits treat two
#' sources as agreeing if they share *any* value in common (`any_shared`) --
#' the looser standard OIG itself typically applies, since a directory with
#' 3 listed locations is not wrong just because the comparison source only
#' confirms 1 of them. `same_set` is a stricter extension: every value on
#' both sides matches, with none extra on either side. Reporting only one of
#' the two would silently pick a standard for the caller; both are returned
#' so the analyst chooses.
#'
#' **No freshness fields.** Unlike [attribute_agreement()], this has no
#' `observed_at`/`as_of` staleness machinery -- a set of values has no single
#' record date to measure age against. Run [record_age_years()] /
#' [classify_record_staleness()] separately on whichever single-valued
#' fields do carry a date.
#'
#' @param sources Named list, same shape as [attribute_agreement()]'s
#'   argument: each element is a data frame with an identifier column, the
#'   attribute column, and (optionally) a year column. One row per value --
#'   a source with two practice locations for the same identifier-year
#'   contributes two rows, not one row with a combined value. Names are the
#'   source labels and must be unique.
#' @param attribute Name of the attribute column to compare. Values are
#'   trimmed and upper-cased before comparison.
#' @param id Name of the identifier column. Default `"npi"`.
#' @param year Name of the year column, or `NULL` if the comparison is a
#'   single cross-section (all rows are treated as one unlabeled year).
#' @return A tibble, one row per `(id, year, source_a, source_b)` where both
#'   sources have at least one non-missing attribute value for that
#'   identifier-year: `values_a`, `values_b` (each source's distinct values,
#'   sorted and joined with `"|"`), `n_values_a`, `n_values_b`, `any_shared`
#'   (logical), `same_set` (logical). Attribute `sources` holds the source
#'   names in order.
#' @examples
#' s <- list(
#'   nppes = data.frame(npi = c("1", "1", "2"), year = 2024,
#'                       state = c("CO", "NM", "CO")), # npi 1 has two states
#'   pecos = data.frame(npi = c("1", "2"), year = 2024, state = c("CO", "NM"))
#' )
#' sa <- attribute_set_agreement(s, attribute = "state")
#' sa[, c("id", "values_a", "values_b", "any_shared", "same_set")]
#' @seealso [attribute_agreement()] for the single-value case this
#'   generalizes, and `vignette("attribute-agreement")`.
#' @family no-gold-standard comparison
#' @export
attribute_set_agreement <- function(sources, attribute, id = "npi", year = "year") {
  checkmate::assert_list(sources, min.len = 2, names = "unique")
  checkmate::assert_string(attribute, min.chars = 1)
  checkmate::assert_string(id, min.chars = 1)
  nm <- names(sources)

  long <- vector("list", length(nm))
  names(long) <- nm
  for (s in nm) {
    x <- sources[[s]]
    checkmate::assert_data_frame(x)
    for (col in c(id, attribute)) {
      if (!col %in% names(x)) {
        stop(sprintf("attribute_set_agreement: source '%s' has no column '%s'", s, col), call. = FALSE)
      }
    }
    row <- tibble::tibble(
      id = trimws(as.character(x[[id]])),
      year = if (!is.null(year) && year %in% names(x)) x[[year]] else NA,
      value = toupper(trimws(as.character(x[[attribute]])))
    )
    row <- row[!is.na(row$value) & nzchar(row$value) & !is.na(row$id) & nzchar(row$id), ]
    row$source <- s
    long[[s]] <- row
  }
  long <- dplyr::bind_rows(long)
  if (!nrow(long)) stop("attribute_set_agreement: no non-missing attribute values in any source", call. = FALSE)

  key <- paste(long$id, long$year, long$source, sep = "\r")
  idx <- split(seq_len(nrow(long)), key, drop = TRUE)
  by_source <- dplyr::bind_rows(lapply(idx, function(rows) {
    tibble::tibble(
      id = long$id[rows[1]], year = long$year[rows[1]], source = long$source[rows[1]],
      values = list(sort(unique(long$value[rows])))
    )
  }))

  pairs <- utils::combn(nm, 2, simplify = FALSE)
  out <- vector("list", length(pairs))
  for (i in seq_along(pairs)) {
    sa <- pairs[[i]][1]; sb <- pairs[[i]][2]
    la <- by_source[by_source$source == sa, c("id", "year", "values")]
    lb <- by_source[by_source$source == sb, c("id", "year", "values")]
    names(la) <- c("id", "year", "values_a")
    names(lb) <- c("id", "year", "values_b")
    m <- merge(la, lb, by = c("id", "year"))
    if (!nrow(m)) next
    m$source_a <- sa
    m$source_b <- sb
    m$any_shared <- mapply(function(a, b) length(intersect(a, b)) > 0, m$values_a, m$values_b)
    m$same_set <- mapply(identical, m$values_a, m$values_b)
    m$n_values_a <- lengths(m$values_a)
    m$n_values_b <- lengths(m$values_b)
    m$values_a <- vapply(m$values_a, paste, collapse = "|", FUN.VALUE = character(1))
    m$values_b <- vapply(m$values_b, paste, collapse = "|", FUN.VALUE = character(1))
    out[[i]] <- m
  }
  out <- dplyr::bind_rows(out)
  if (!nrow(out)) {
    stop("attribute_set_agreement: no identifier-year is covered by more than one source", call. = FALSE)
  }
  out <- tibble::as_tibble(out[, c("id", "year", "source_a", "source_b", "values_a", "values_b",
                                   "n_values_a", "n_values_b", "any_shared", "same_set")])
  attr(out, "sources") <- nm
  out
}

#' Coverage and agreement rate per source pair and year
#'
#' Aggregates [attribute_agreement()]'s row-level output the way a
#' state-by-state audit reports it: for each pair of sources and each year,
#' how many identifiers both sources covered, how many agreed, and how many
#' disagreements had no temporal basis to classify (because at least one
#' source carries no freshness date).
#'
#' @param agreement Output of [attribute_agreement()].
#' @return A tibble: `source_a`, `source_b`, `year`, `n_compared`, `n_agree`,
#'   `pct_agree`, `n_disagree`, `n_disagree_no_temporal_basis`, ordered by
#'   `year`, `source_a`, `source_b`. `pct_agree` is a proportion in `[0, 1]`,
#'   not a percentage. Pairs that share no identifier-year are absent rather
#'   than shown with `n_compared = 0`.
#' @examples
#' s <- list(
#'   nppes = data.frame(npi = c("1", "2", "3"), year = 2024, state = c("CO", "CO", "TX")),
#'   pecos = data.frame(npi = c("1", "2", "3"), year = 2024, state = c("CO", "NM", "TX")),
#'   op    = data.frame(npi = c("1", "3"), year = 2024, state = c("CO", "OK"))
#' )
#' attribute_agreement_summary(attribute_agreement(s, attribute = "state"))
#' @family no-gold-standard comparison
#' @export
attribute_agreement_summary <- function(agreement) {
  checkmate::assert_data_frame(agreement)
  need <- c("source_a", "source_b", "year", "agree", "staleness")
  missing_cols <- setdiff(need, names(agreement))
  if (length(missing_cols) > 0) {
    stop("attribute_agreement_summary: agreement is missing column(s): ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  key <- paste(agreement$source_a, agreement$source_b, agreement$year, sep = "")
  groups <- split(agreement, key, drop = TRUE)
  rows <- lapply(groups, function(g) {
    n_compared <- nrow(g)
    n_agree <- sum(g$agree)
    tibble::tibble(
      source_a = g$source_a[1], source_b = g$source_b[1], year = g$year[1],
      n_compared = n_compared, n_agree = n_agree, pct_agree = n_agree / n_compared,
      n_disagree = n_compared - n_agree,
      n_disagree_no_temporal_basis = sum(!g$agree & g$staleness == "no_temporal_basis")
    )
  })
  out <- dplyr::bind_rows(rows)
  out <- out[order(out$year, out$source_a, out$source_b), ]
  tibble::as_tibble(out)
}
