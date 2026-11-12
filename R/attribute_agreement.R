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

#' Comparable count and agreement rate per source pair and year, set-based
#'
#' The set-based sibling of [attribute_agreement_summary()]: aggregates
#' [attribute_set_agreement()]'s row-level output the way a state-by-state
#' audit reports it, for each pair of sources and each year -- how many
#' identifiers both sources had a comparable value set for, and what share
#' agreed under each of the two looser-to-stricter standards
#' [attribute_set_agreement()] returns.
#'
#' @param set_agreement Output of [attribute_set_agreement()].
#' @return A tibble: `source_a`, `source_b`, `year`, `n_compared`,
#'   `n_any_shared`, `pct_any_shared`, `n_same_set`, `pct_same_set`, ordered
#'   by `year`, `source_a`, `source_b`. Both `pct_*` columns are proportions
#'   in `[0, 1]`, not percentages. Pairs that share no identifier-year are
#'   absent rather than shown with `n_compared = 0`.
#' @examples
#' s <- list(
#'   nppes = data.frame(npi = c("1", "1", "2"), year = 2024, state = c("CO", "NM", "CO")),
#'   pecos = data.frame(npi = c("1", "2"), year = 2024, state = c("CO", "NM"))
#' )
#' attribute_set_agreement_summary(attribute_set_agreement(s, attribute = "state"))
#' @seealso [attribute_set_agreement()], [attribute_agreement_summary()] for
#'   the single-value case this generalizes.
#' @family no-gold-standard comparison
#' @export
attribute_set_agreement_summary <- function(set_agreement) {
  checkmate::assert_data_frame(set_agreement)
  need <- c("source_a", "source_b", "year", "any_shared", "same_set")
  missing_cols <- setdiff(need, names(set_agreement))
  if (length(missing_cols) > 0) {
    stop("attribute_set_agreement_summary: set_agreement is missing column(s): ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  key <- paste(set_agreement$source_a, set_agreement$source_b, set_agreement$year, sep = "")
  groups <- split(set_agreement, key, drop = TRUE)
  rows <- lapply(groups, function(g) {
    n_compared <- nrow(g)
    n_any_shared <- sum(g$any_shared)
    n_same_set <- sum(g$same_set)
    tibble::tibble(
      source_a = g$source_a[1], source_b = g$source_b[1], year = g$year[1],
      n_compared = n_compared, n_any_shared = n_any_shared, pct_any_shared = n_any_shared / n_compared,
      n_same_set = n_same_set, pct_same_set = n_same_set / n_compared
    )
  })
  out <- dplyr::bind_rows(rows)
  out <- out[order(out$year, out$source_a, out$source_b), ]
  tibble::as_tibble(out)
}

#' Mutually exclusive hierarchical geographic concordance between practice and licensing/mailing addresses
#'
#' When comparing a clinical practice directory (such as NPPES or PECOS/DAC)
#' against a state licensing or professional board roster (which often records
#' a clinician's mailing or residential licensing address), reporting separate
#' match counts for exact 5-digit practice ZIP (`zip5`), 3-digit Sectional
#' Center Facility (`scf3`), and mailing ZIP produces overlapping categories
#' whose percentages sum to more than 100%. This function enforces a strictly
#' mutually exclusive five-way (or six-way when state is supplied) hierarchical
#' partition whose counts are guaranteed to sum to `n_compared` (`100.0%`), and
#' reports overlapping marginal indicators in a separate table.
#'
#' @details
#' **Mutually exclusive hierarchy (evaluated in order):**
#' \enumerate{
#'   \item `"1. Exact Practice ZIP5 Match"` -- Reference ZIP5 equals directory
#'     practice ZIP5.
#'   \item `"2. Differs from Practice ZIP5, Matches Mailing ZIP5 (Same 3-Digit SCF)"` --
#'     Reference ZIP5 differs from practice ZIP5, equals the clinician's
#'     mailing ZIP5, and shares the same 3-digit SCF prefix as practice ZIP5.
#'   \item `"3. Differs from Practice ZIP5, Matches Mailing ZIP5 (Different 3-Digit SCF)"` --
#'     Reference ZIP5 differs from practice ZIP5 and SCF3, but equals the
#'     clinician's mailing ZIP5.
#'   \item `"4. Same 3-Digit SCF Area Only (No Practice or Mailing ZIP5 Match)"` --
#'     Reference ZIP5 shares the same 3-digit SCF prefix as practice ZIP5,
#'     without matching either practice ZIP5 or mailing ZIP5.
#'   \item `"5. Unattributed ZIP5 Discordance"` -- Reference ZIP5 lies in a
#'     different 3-digit SCF and matches neither practice ZIP5 nor mailing ZIP5.
#' }
#'
#' @param directory Data frame of directory records containing `id`, optional
#'   `year`, `practice_zip_col`, and optional `mailing_zip_col`.
#' @param reference Data frame of comparison/licensing/board records containing
#'   `id`, optional `year`, and `reference_zip_col`.
#' @param id,year Names of identifier and year columns. Defaults `"npi"`,
#'   `"year"` (`year = NULL` for a single cross-section).
#' @param practice_zip_col Name of the practice ZIP column in `directory`.
#'   Default `"practice_zip5"`.
#' @param mailing_zip_col Optional name of the mailing ZIP column in
#'   `directory`. Default `"mailing_zip5"`. If `NULL` or absent from
#'   `directory`, mailing-match categories have count `0`.
#' @param reference_zip_col Name of the ZIP column in `reference`. Default
#'   `"reference_zip5"`.
#' @param conf_level Confidence level for Wilson intervals via [wilson_ci()].
#'   Default `0.95`.
#' @return A list with three tibbles:
#'   \describe{
#'     \item{`records`}{One row per compared `(id, year)` with normalized 5-digit
#'       ZIPs, 3-digit SCF prefixes, boolean flags (`exact_practice_zip5`,
#'       `matches_mailing_zip5`, `same_scf3`), and the mutually exclusive factor
#'       `concordance_category`.}
#'     \item{`mutually_exclusive`}{Summary table of the 5 mutually exclusive
#'       categories (plus a `"Total"` row summing to `n_compared` and `100.0%`),
#'       with `n`, `n_compared`, `pct`, `ci_low`, `ci_high`.}
#'     \item{`marginal_indicators`}{Summary table of the 4 overlapping marginal
#'       indicators (`any_same_scf3`, `same_scf3_diff_practice_zip5`,
#'       `diff_practice_matches_mailing_zip5`,
#'       `matches_practice_or_mailing_zip5`).}
#'   }
#' @examples
#' dir_df <- data.frame(
#'   npi = c("1", "2", "3", "4", "5"),
#'   year = 2026,
#'   practice_zip5 = c("80204", "80204", "80204", "80204", "80204"),
#'   mailing_zip5  = c("80204", "80206", "80301", "80204", "80204")
#' )
#' ref_df <- data.frame(
#'   npi = c("1", "2", "3", "4", "5"),
#'   year = 2026,
#'   reference_zip5 = c("80204", "80206", "80301", "80210", "98101")
#' )
#' conc <- attribute_geographic_concordance(dir_df, ref_df)
#' conc$mutually_exclusive[, c("concordance_category", "n", "pct")]
#' conc$marginal_indicators[, c("indicator", "n", "pct")]
#' @seealso [attribute_agreement()], [attribute_set_agreement()].
#' @family no-gold-standard comparison
#' @export
attribute_geographic_concordance <- function(directory,
                                             reference,
                                             id = "npi",
                                             year = "year",
                                             practice_zip_col = "practice_zip5",
                                             mailing_zip_col = "mailing_zip5",
                                             reference_zip_col = "reference_zip5",
                                             conf_level = 0.95) {
  checkmate::assert_data_frame(directory, min.rows = 1)
  checkmate::assert_data_frame(reference, min.rows = 1)
  checkmate::assert_string(id, min.chars = 1)
  checkmate::assert_string(practice_zip_col, min.chars = 1)
  checkmate::assert_string(reference_zip_col, min.chars = 1)
  checkmate::assert_number(conf_level, lower = 0, upper = 1)
  if (conf_level <= 0 || conf_level >= 1) {
    stop("attribute_geographic_concordance: conf_level must be strictly between 0 and 1.", call. = FALSE)
  }

  for (col in c(id, practice_zip_col)) {
    if (!col %in% names(directory)) {
      stop(sprintf("attribute_geographic_concordance: directory is missing column '%s'.", col), call. = FALSE)
    }
  }
  for (col in c(id, reference_zip_col)) {
    if (!col %in% names(reference)) {
      stop(sprintf("attribute_geographic_concordance: reference is missing column '%s'.", col), call. = FALSE)
    }
  }

  clean_zip5 <- function(x) {
    d <- gsub("[^0-9]", "", trimws(as.character(x)))
    short <- !is.na(d) & nchar(d) >= 3L & nchar(d) <= 4L
    d[short] <- sprintf("%05d", suppressWarnings(as.integer(d[short])))
    z <- ifelse(!is.na(d) & nchar(d) >= 5L, substr(d, 1L, 5L), NA_character_)
    z[z == "00000"] <- NA_character_
    z
  }

  has_yr <- !is.null(year) && (year %in% names(directory)) && (year %in% names(reference))
  has_mail <- !is.null(mailing_zip_col) && (mailing_zip_col %in% names(directory))

  d_clean <- tibble::tibble(
    id = trimws(as.character(directory[[id]])),
    year = if (has_yr) directory[[year]] else NA,
    practice_zip5 = clean_zip5(directory[[practice_zip_col]]),
    mailing_zip5 = if (has_mail) clean_zip5(directory[[mailing_zip_col]]) else NA_character_
  )
  d_clean <- d_clean[!is.na(d_clean$id) & nzchar(d_clean$id) & !is.na(d_clean$practice_zip5), , drop = FALSE]

  r_clean <- tibble::tibble(
    id = trimws(as.character(reference[[id]])),
    year = if (has_yr) reference[[year]] else NA,
    reference_zip5 = clean_zip5(reference[[reference_zip_col]])
  )
  r_clean <- r_clean[!is.na(r_clean$id) & nzchar(r_clean$id) & !is.na(r_clean$reference_zip5), , drop = FALSE]

  by_cols <- if (has_yr) c("id", "year") else "id"
  if (anyDuplicated(d_clean[, by_cols, drop = FALSE])) {
    stop("attribute_geographic_concordance: directory has duplicate (id, year) rows.", call. = FALSE)
  }
  if (anyDuplicated(r_clean[, by_cols, drop = FALSE])) {
    stop("attribute_geographic_concordance: reference has duplicate (id, year) rows.", call. = FALSE)
  }

  m <- merge(d_clean, r_clean[, c(by_cols, "reference_zip5"), drop = FALSE], by = by_cols)
  if (nrow(m) == 0L) {
    stop("attribute_geographic_concordance: no overlapping records with valid 5-digit ZIP codes in both directory and reference.", call. = FALSE)
  }
  if (!has_yr) m$year <- NA

  m$practice_scf3 <- substr(m$practice_zip5, 1L, 3L)
  m$reference_scf3 <- substr(m$reference_zip5, 1L, 3L)
  m$exact_practice_zip5 <- m$practice_zip5 == m$reference_zip5
  m$same_scf3 <- m$practice_scf3 == m$reference_scf3
  m$matches_mailing_zip5 <- !is.na(m$mailing_zip5) & (m$mailing_zip5 == m$reference_zip5)

  cat_levels <- c(
    "1. Exact Practice ZIP5 Match",
    "2. Differs from Practice ZIP5, Matches Mailing ZIP5 (Same 3-Digit SCF)",
    "3. Differs from Practice ZIP5, Matches Mailing ZIP5 (Different 3-Digit SCF)",
    "4. Same 3-Digit SCF Area Only (No Practice or Mailing ZIP5 Match)",
    "5. Unattributed ZIP5 Discordance"
  )

  cat_vec <- dplyr::case_when(
    m$exact_practice_zip5 ~ cat_levels[1],
    !m$exact_practice_zip5 & m$matches_mailing_zip5 & m$same_scf3 ~ cat_levels[2],
    !m$exact_practice_zip5 & m$matches_mailing_zip5 & !m$same_scf3 ~ cat_levels[3],
    !m$exact_practice_zip5 & !m$matches_mailing_zip5 & m$same_scf3 ~ cat_levels[4],
    TRUE ~ cat_levels[5]
  )
  m$concordance_category <- factor(cat_vec, levels = cat_levels, ordered = TRUE)
  m <- m[order(m$id, m$year), c(
    "id", "year", "practice_zip5", "mailing_zip5", "reference_zip5",
    "practice_scf3", "reference_scf3", "exact_practice_zip5",
    "matches_mailing_zip5", "same_scf3", "concordance_category"
  ), drop = FALSE]

  n_tot <- nrow(m)
  make_row <- function(label, n_k) {
    ci <- wilson_ci(n_k, n_tot, conf_level = conf_level, as_percent = TRUE)
    tibble::tibble(
      concordance_category = label,
      n = as.integer(n_k),
      n_compared = as.integer(n_tot),
      pct = round(100 * n_k / n_tot, 2),
      ci_low = round(ci$ci_low, 2),
      ci_high = round(ci$ci_high, 2)
    )
  }

  me_rows <- lapply(cat_levels, function(cl) {
    make_row(cl, sum(m$concordance_category == cl))
  })
  me_tbl <- dplyr::bind_rows(
    me_rows,
    tibble::tibble(
      concordance_category = "Total",
      n = as.integer(n_tot),
      n_compared = as.integer(n_tot),
      pct = 100.0,
      ci_low = 100.0,
      ci_high = 100.0
    )
  )

  make_marg <- function(ind_code, ind_label, flag_vec) {
    n_k <- sum(flag_vec, na.rm = TRUE)
    ci <- wilson_ci(n_k, n_tot, conf_level = conf_level, as_percent = TRUE)
    tibble::tibble(
      indicator = ind_code,
      description = ind_label,
      n = as.integer(n_k),
      n_compared = as.integer(n_tot),
      pct = round(100 * n_k / n_tot, 2),
      ci_low = round(ci$ci_low, 2),
      ci_high = round(ci$ci_high, 2)
    )
  }

  marg_tbl <- dplyr::bind_rows(
    make_marg("any_same_scf3", "Any Same 3-Digit SCF (Categories 1 + 2 + 4)", m$same_scf3),
    make_marg("same_scf3_diff_practice_zip5", "Same 3-Digit SCF but Different 5-Digit Practice ZIP (Categories 2 + 4)", !m$exact_practice_zip5 & m$same_scf3),
    make_marg("diff_practice_matches_mailing_zip5", "Differs from Practice ZIP5 but Matches Mailing ZIP5 (Categories 2 + 3)", !m$exact_practice_zip5 & m$matches_mailing_zip5),
    make_marg("matches_practice_or_mailing_zip5", "Matches Either Practice ZIP5 or Mailing ZIP5 (Categories 1 + 2 + 3)", m$exact_practice_zip5 | m$matches_mailing_zip5)
  )

  list(
    records = tibble::as_tibble(m),
    mutually_exclusive = tibble::as_tibble(me_tbl),
    marginal_indicators = tibble::as_tibble(marg_tbl)
  )
}

.scf3_default_centroids <- function() {
  # Representative geographic centroids by 1st-digit / 2nd-digit US Postal SCF regions
  # Used as a fallback when neither explicit lat/lon nor a 5-digit ZCTA centroid table is supplied
  scf2 <- sprintf("%02d", 0:99)
  # Approximate regional lat/lon anchor grid across US postal zones 00..99
  zone_lat <- c(
    18.2, 42.3, 42.4, 43.1, 41.7, 44.3, 41.3, 40.7, 40.2, 18.4, # 00-09 (PR, MA, RI, NH, ME, VT, CT, NJ, AE)
    40.7, 40.8, 42.7, 43.0, 42.9, 40.4, 41.2, 40.3, 40.6, 39.9, # 10-19 (NY, PA, DE)
    38.9, 39.3, 38.8, 37.5, 37.3, 38.3, 39.3, 35.8, 35.2, 34.0, # 20-29 (DC, MD, VA, WV, NC, SC)
    33.7, 31.6, 30.3, 25.8, 28.0, 33.5, 32.4, 36.2, 35.1, 32.3, # 30-39 (GA, FL, AL, TN, MS)
    38.2, 37.5, 37.0, 40.0, 41.5, 39.1, 39.8, 39.2, 42.3, 43.0, # 40-49 (KY, OH, IN, MI)
    41.6, 42.0, 41.5, 43.1, 44.5, 45.0, 46.5, 43.5, 46.9, 46.9, # 50-59 (IA, WI, MN, SD, ND, MT)
    41.9, 40.7, 39.8, 38.6, 39.1, 37.8, 39.0, 37.7, 41.3, 41.1, # 60-69 (IL, MO, KS, NE)
    30.0, 32.5, 34.7, 35.5, 36.2, 32.8, 32.7, 29.8, 29.4, 33.6, # 70-79 (LA, AR, OK, TX)
    39.7, 38.8, 41.1, 43.6, 40.8, 33.4, 35.2, 35.1, 32.3, 36.2, # 80-89 (CO, WY, ID, UT, AZ, NM, NV)
    34.1, 34.2, 32.7, 36.7, 37.8, 38.6, 21.3, 45.5, 47.6, 61.2  # 90-99 (CA, HI, OR, WA, AK)
  )
  zone_lon <- c(
    -66.5, -71.8, -71.1, -71.5, -70.0, -72.6, -72.7, -74.2, -74.5, -66.1,
    -74.0, -73.2, -73.8, -76.1, -78.8, -80.0, -77.5, -76.9, -75.5, -75.2,
    -77.0, -76.6, -77.3, -77.4, -79.9, -81.6, -80.5, -78.6, -80.8, -81.0,
    -84.4, -83.5, -81.7, -80.2, -82.5, -86.8, -86.3, -86.8, -89.9, -90.2,
    -85.8, -83.5, -86.4, -83.0, -81.7, -84.5, -86.2, -86.5, -83.1, -85.7,
    -93.6, -91.6, -90.6, -89.4, -89.5, -93.3, -95.0, -96.7, -100.8, -110.4,
    -87.8, -89.6, -89.0, -90.2, -94.6, -92.6, -95.7, -97.3, -96.0, -100.8,
    -90.1, -93.7, -92.3, -97.5, -95.9, -96.8, -97.3, -95.4, -98.5, -101.8,
    -105.0, -106.0, -104.8, -116.2, -111.9, -112.1, -111.7, -106.6, -106.8, -115.1,
    -118.2, -118.4, -117.2, -119.8, -122.4, -121.5, -157.9, -122.7, -122.3, -149.9
  )
  # Expand to 3-digit SCF with slight deterministic sub-grid offset (~0.08 deg per last digit)
  rows <- lapply(0:999, function(k) {
    s3 <- sprintf("%03d", k)
    z2 <- (k %/% 10L) + 1L
    d1 <- k %% 10L
    tibble::tibble(
      scf3 = s3,
      lat = zone_lat[z2] + (d1 - 4.5) * 0.06,
      lon = zone_lon[z2] + ((d1 %% 3L) - 1.0) * 0.08
    )
  })
  dplyr::bind_rows(rows)
}

#' Continuous geodesic Haversine distance and commute-shed concordance between provider directories
#'
#' Postal boundaries (`ZIP5`, `SCF3`, and `state`) are discrete administrative
#' polygons: two clinics located `3 km` apart across a river or postal boundary
#' (such as Kansas City, MO `64108` vs. Kansas City, KS `66103`, or Manhattan
#' `10001` vs. Hoboken `07030`) appear completely discordant on state, 3-digit
#' SCF, and 5-digit ZIP, whereas two rural clinics in the same 3-digit SCF can
#' lie `120 km` apart.
#'
#' This function computes continuous geodesic Haversine distance (`distance_km`
#' and `distance_miles`) between a `directory` and a `reference` source and
#' classifies every pair into five mutually exclusive spatial commute-shed
#' concordance bands:
#' \enumerate{
#'   \item `"1. Exact Co-Location (<1 km or Exact ZIP5)"`
#'   \item `"2. Same Neighborhood / Hospital Campus (1 to <5 km)"`
#'   \item `"3. Same Metro Commute Shed (5 to <25 km)"`
#'   \item `"4. Regional Satellite / Extended Commute (25 to <100 km)"`
#'   \item `"5. Long-Distance / Cross-Market Discordance (>=100 km)"`
#' }
#'
#' @param directory,reference Data frames for the two sources to compare, each
#'   containing `id`, optional `year`, and either explicit latitude/longitude
#'   columns or a 5-digit ZIP code column (`dir_zip_col`, `ref_zip_col`).
#' @param id,year Names of identifier and year columns. Defaults `"npi"`,
#'   `"year"`.
#' @param dir_zip_col,ref_zip_col Names of the 5-digit ZIP columns in
#'   `directory` and `reference`. Defaults `"practice_zip5"`,
#'   `"reference_zip5"`.
#' @param dir_lat_col,dir_lon_col,ref_lat_col,ref_lon_col Optional names of
#'   explicit decimal-degree latitude and longitude columns in `directory` and
#'   `reference`. When supplied and non-missing, exact point-to-point Haversine
#'   distance is used.
#' @param zip_centroids Optional data frame with columns `zip5`, `lat`, and
#'   `lon` providing 5-digit ZCTA centroids. When `NULL` and explicit lat/lon
#'   columns are absent, exact 5-digit ZIP matches are assigned `0.0 km` and
#'   non-matching ZIPs are resolved via the package's built-in 3-digit SCF
#'   centroid grid (`resolution_method = "scf3_centroid_fallback"`).
#' @param conf_level Confidence level for Wilson intervals via [wilson_ci()].
#'   Default `0.95`.
#' @return A list with three tibbles:
#'   \describe{
#'     \item{`records`}{One row per compared `(id, year)` with `dir_zip5`,
#'       `ref_zip5`, `dir_lat`, `dir_lon`, `ref_lat`, `ref_lon`,
#'       `resolution_method`, `distance_km`, `distance_miles`, and the mutually
#'       exclusive `spatial_band`.}
#'     \item{`band_summary`}{Mutually exclusive 5-band summary table (plus a
#'       `"Total"` row summing to `100.0%`) with `n`, `n_compared`, `pct`,
#'       `ci_low`, `ci_high`.}
#'     \item{`distance_distribution`}{Summary of continuous distance (mean, SD,
#'       median, p25, p75, p90, max in km and miles) overall and restricted to
#'       ZIP5-discordant pairs.}
#'   }
#' @examples
#' dir_df <- data.frame(
#'   npi = c("1", "2", "3", "4"),
#'   year = 2026,
#'   practice_zip5 = c("80204", "80204", "80204", "80204"),
#'   lat = c(39.7392, 39.7392, 39.7392, 39.7392),
#'   lon = c(-105.0186, -105.0186, -105.0186, -105.0186)
#' )
#' ref_df <- data.frame(
#'   npi = c("1", "2", "3", "4"),
#'   year = 2026,
#'   reference_zip5 = c("80204", "80206", "80301", "98101"),
#'   lat = c(39.7392, 39.7400, 40.0150, 47.6062),
#'   lon = c(-105.0186, -104.9500, -105.2705, -122.3321)
#' )
#' sp <- attribute_spatial_distance(
#'   dir_df, ref_df,
#'   dir_lat_col = "lat", dir_lon_col = "lon",
#'   ref_lat_col = "lat", ref_lon_col = "lon"
#' )
#' sp$band_summary[, c("spatial_band", "n", "pct")]
#' sp$distance_distribution
#' @seealso [attribute_geographic_concordance()], [attribute_agreement()].
#' @family no-gold-standard comparison
#' @export
attribute_spatial_distance <- function(directory,
                                       reference,
                                       id = "npi",
                                       year = "year",
                                       dir_zip_col = "practice_zip5",
                                       ref_zip_col = "reference_zip5",
                                       dir_lat_col = NULL,
                                       dir_lon_col = NULL,
                                       ref_lat_col = NULL,
                                       ref_lon_col = NULL,
                                       zip_centroids = NULL,
                                       conf_level = 0.95) {
  checkmate::assert_data_frame(directory, min.rows = 1)
  checkmate::assert_data_frame(reference, min.rows = 1)
  checkmate::assert_string(id, min.chars = 1)
  checkmate::assert_number(conf_level, lower = 0, upper = 1)
  if (conf_level <= 0 || conf_level >= 1) {
    stop("attribute_spatial_distance: conf_level must be strictly between 0 and 1.", call. = FALSE)
  }

  if (!id %in% names(directory) || !id %in% names(reference)) {
    stop(sprintf("attribute_spatial_distance: both directory and reference must contain '%s'.", id), call. = FALSE)
  }

  has_yr <- !is.null(year) && (year %in% names(directory)) && (year %in% names(reference))
  by_cols <- if (has_yr) c("id", "year") else "id"

  clean_z5 <- function(x) {
    if (is.null(x)) return(NA_character_)
    d <- gsub("[^0-9]", "", trimws(as.character(x)))
    short <- !is.na(d) & nchar(d) >= 3L & nchar(d) <= 4L
    d[short] <- sprintf("%05d", suppressWarnings(as.integer(d[short])))
    z <- ifelse(!is.na(d) & nchar(d) >= 5L, substr(d, 1L, 5L), NA_character_)
    z[z == "00000"] <- NA_character_
    z
  }

  d_df <- tibble::tibble(
    id = trimws(as.character(directory[[id]])),
    year = if (has_yr) directory[[year]] else NA,
    dir_zip5 = if (!is.null(dir_zip_col) && dir_zip_col %in% names(directory)) clean_z5(directory[[dir_zip_col]]) else rep(NA_character_, nrow(directory)),
    dir_lat = if (!is.null(dir_lat_col) && dir_lat_col %in% names(directory)) as.numeric(directory[[dir_lat_col]]) else rep(NA_real_, nrow(directory)),
    dir_lon = if (!is.null(dir_lon_col) && dir_lon_col %in% names(directory)) as.numeric(directory[[dir_lon_col]]) else rep(NA_real_, nrow(directory))
  )
  r_df <- tibble::tibble(
    id = trimws(as.character(reference[[id]])),
    year = if (has_yr) reference[[year]] else NA,
    ref_zip5 = if (!is.null(ref_zip_col) && ref_zip_col %in% names(reference)) clean_z5(reference[[ref_zip_col]]) else rep(NA_character_, nrow(reference)),
    ref_lat = if (!is.null(ref_lat_col) && ref_lat_col %in% names(reference)) as.numeric(reference[[ref_lat_col]]) else rep(NA_real_, nrow(reference)),
    ref_lon = if (!is.null(ref_lon_col) && ref_lon_col %in% names(reference)) as.numeric(reference[[ref_lon_col]]) else rep(NA_real_, nrow(reference))
  )

  if (anyDuplicated(d_df[, by_cols, drop = FALSE])) {
    stop("attribute_spatial_distance: directory has duplicate (id, year) rows.", call. = FALSE)
  }
  if (anyDuplicated(r_df[, by_cols, drop = FALSE])) {
    stop("attribute_spatial_distance: reference has duplicate (id, year) rows.", call. = FALSE)
  }

  m <- merge(
    d_df,
    r_df[, c(by_cols, "ref_zip5", "ref_lat", "ref_lon"), drop = FALSE],
    by = by_cols
  )
  if (nrow(m) == 0L) {
    stop("attribute_spatial_distance: no overlapping (id, year) records between directory and reference.", call. = FALSE)
  }
  if (!has_yr) m$year <- NA

  # Fill missing coordinates from zip_centroids if supplied
  res_method <- ifelse(
    !is.na(m$dir_lat) & !is.na(m$dir_lon) & !is.na(m$ref_lat) & !is.na(m$ref_lon),
    "explicit_lat_lon",
    NA_character_
  )

  if (!is.null(zip_centroids)) {
    checkmate::assert_data_frame(zip_centroids, min.rows = 1)
    if (!all(c("zip5", "lat", "lon") %in% names(zip_centroids))) {
      stop("attribute_spatial_distance: zip_centroids must contain columns 'zip5', 'lat', and 'lon'.", call. = FALSE)
    }
    zc_z <- clean_z5(zip_centroids$zip5)
    lat_map <- stats::setNames(as.numeric(zip_centroids$lat), zc_z)
    lon_map <- stats::setNames(as.numeric(zip_centroids$lon), zc_z)

    need_d <- is.na(m$dir_lat) | is.na(m$dir_lon)
    m$dir_lat[need_d] <- unname(lat_map[m$dir_zip5[need_d]])
    m$dir_lon[need_d] <- unname(lon_map[m$dir_zip5[need_d]])

    need_r <- is.na(m$ref_lat) | is.na(m$ref_lon)
    m$ref_lat[need_r] <- unname(lat_map[m$ref_zip5[need_r]])
    m$ref_lon[need_r] <- unname(lon_map[m$ref_zip5[need_r]])

    now_ok <- is.na(res_method) & !is.na(m$dir_lat) & !is.na(m$dir_lon) & !is.na(m$ref_lat) & !is.na(m$ref_lon)
    res_method[now_ok] <- "zip5_centroid"
  }

  # Fallback to built-in 3-digit SCF centroid grid for any remaining rows with valid ZIP5
  scf_grid <- .scf3_default_centroids()
  scf_lat <- stats::setNames(scf_grid$lat, scf_grid$scf3)
  scf_lon <- stats::setNames(scf_grid$lon, scf_grid$scf3)

  need_d2 <- (is.na(m$dir_lat) | is.na(m$dir_lon)) & !is.na(m$dir_zip5)
  m$dir_lat[need_d2] <- unname(scf_lat[substr(m$dir_zip5[need_d2], 1L, 3L)])
  m$dir_lon[need_d2] <- unname(scf_lon[substr(m$dir_zip5[need_d2], 1L, 3L)])

  need_r2 <- (is.na(m$ref_lat) | is.na(m$ref_lon)) & !is.na(m$ref_zip5)
  m$ref_lat[need_r2] <- unname(scf_lat[substr(m$ref_zip5[need_r2], 1L, 3L)])
  m$ref_lon[need_r2] <- unname(scf_lon[substr(m$ref_zip5[need_r2], 1L, 3L)])

  now_scf <- is.na(res_method) & !is.na(m$dir_lat) & !is.na(m$dir_lon) & !is.na(m$ref_lat) & !is.na(m$ref_lon)
  res_method[now_scf] <- "scf3_centroid_fallback"

  same_zip <- !is.na(m$dir_zip5) & !is.na(m$ref_zip5) & (m$dir_zip5 == m$ref_zip5)
  res_method[same_zip & res_method == "scf3_centroid_fallback"] <- "exact_zip5_zero_distance"

  m$resolution_method <- res_method
  m <- m[!is.na(m$resolution_method), , drop = FALSE]
  if (nrow(m) == 0L) {
    stop("attribute_spatial_distance: no comparable rows with resolvable coordinates or ZIP codes.", call. = FALSE)
  }

  # Haversine great-circle distance in km (Earth mean radius = 6371.0088 km)
  rad <- pi / 180
  lat1 <- m$dir_lat * rad
  lon1 <- m$dir_lon * rad
  lat2 <- m$ref_lat * rad
  lon2 <- m$ref_lon * rad
  dlat <- lat2 - lat1
  dlon <- lon2 - lon1
  a_hav <- sin(dlat / 2)^2 + cos(lat1) * cos(lat2) * sin(dlon / 2)^2
  c_hav <- 2 * asin(pmin(1, sqrt(a_hav)))
  dist_km <- 6371.0088 * c_hav

  # If resolved only by ZIP/SCF centroid and the 5-digit ZIPs match exactly, distance is 0
  same_z_now <- !is.na(m$dir_zip5) & !is.na(m$ref_zip5) & (m$dir_zip5 == m$ref_zip5) & (m$resolution_method != "explicit_lat_lon")
  dist_km[same_z_now] <- 0.0

  m$distance_km <- round(dist_km, 3)
  m$distance_miles <- round(dist_km * 0.62137119, 3)
  m$exact_zip5_match <- !is.na(m$dir_zip5) & !is.na(m$ref_zip5) & (m$dir_zip5 == m$ref_zip5)

  band_levels <- c(
    "1. Exact Co-Location (<1 km or Exact ZIP5)",
    "2. Same Neighborhood / Hospital Campus (1 to <5 km)",
    "3. Same Metro Commute Shed (5 to <25 km)",
    "4. Regional Satellite / Extended Commute (25 to <100 km)",
    "5. Long-Distance / Cross-Market Discordance (>=100 km)"
  )
  b_vec <- dplyr::case_when(
    m$exact_zip5_match | m$distance_km < 1.0 ~ band_levels[1],
    m$distance_km < 5.0 ~ band_levels[2],
    m$distance_km < 25.0 ~ band_levels[3],
    m$distance_km < 100.0 ~ band_levels[4],
    TRUE ~ band_levels[5]
  )
  m$spatial_band <- factor(b_vec, levels = band_levels, ordered = TRUE)
  m <- m[order(m$id, m$year), , drop = FALSE]

  n_tot <- nrow(m)
  b_rows <- lapply(band_levels, function(bl) {
    n_k <- sum(m$spatial_band == bl)
    ci <- wilson_ci(n_k, n_tot, conf_level = conf_level, as_percent = TRUE)
    tibble::tibble(
      spatial_band = bl,
      n = as.integer(n_k),
      n_compared = as.integer(n_tot),
      pct = round(100 * n_k / n_tot, 2),
      ci_low = round(ci$ci_low, 2),
      ci_high = round(ci$ci_high, 2)
    )
  })
  band_summary <- dplyr::bind_rows(
    b_rows,
    tibble::tibble(
      spatial_band = "Total",
      n = as.integer(n_tot),
      n_compared = as.integer(n_tot),
      pct = 100.0,
      ci_low = 100.0,
      ci_high = 100.0
    )
  )

  summarize_dist <- function(sub_df, subset_label) {
    if (nrow(sub_df) == 0L) {
      return(tibble::tibble(
        subset = subset_label, n = 0L,
        mean_km = NA_real_, sd_km = NA_real_, median_km = NA_real_,
        p25_km = NA_real_, p75_km = NA_real_, p90_km = NA_real_, max_km = NA_real_,
        median_miles = NA_real_, mean_miles = NA_real_
      ))
    }
    dk <- sub_df$distance_km
    dm <- sub_df$distance_miles
    qs <- stats::quantile(dk, probs = c(0.25, 0.50, 0.75, 0.90), type = 7, names = FALSE)
    tibble::tibble(
      subset = subset_label,
      n = as.integer(nrow(sub_df)),
      mean_km = round(mean(dk), 2),
      sd_km = if (length(dk) >= 2L) round(stats::sd(dk), 2) else NA_real_,
      median_km = round(qs[2], 2),
      p25_km = round(qs[1], 2),
      p75_km = round(qs[3], 2),
      p90_km = round(qs[4], 2),
      max_km = round(max(dk), 2),
      median_miles = round(stats::median(dm), 2),
      mean_miles = round(mean(dm), 2)
    )
  }

  dist_tbl <- dplyr::bind_rows(
    summarize_dist(m, "all_compared_pairs"),
    summarize_dist(m[!m$exact_zip5_match, , drop = FALSE], "zip5_discordant_pairs")
  )

  list(
    records = tibble::as_tibble(m),
    band_summary = tibble::as_tibble(band_summary),
    distance_distribution = tibble::as_tibble(dist_tbl)
  )
}

