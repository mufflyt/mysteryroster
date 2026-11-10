# Internal validation helpers
.assert_unique_keys <- function(df, keys, label) {
  dups <- duplicated(df[, keys, drop = FALSE])
  if (any(dups)) {
    stop(sprintf(
      "%s has %d duplicate row(s) on key (%s); deduplicate before calling",
      label, sum(dups), paste(keys, collapse = ", ")
    ), call. = FALSE)
  }
}

.parse_whole_integer <- function(x, label, allow_na = FALSE) {
  num <- suppressWarnings(as.numeric(as.character(x)))
  bad_na <- if (allow_na) (!is.na(x) & is.na(num)) else is.na(num)
  bad_frac <- !is.na(num) & (num != floor(num))
  if (any(bad_na | bad_frac)) {
    stop(sprintf("%s must contain whole integer values (no NAs or fractional values)", label), call. = FALSE)
  }
  as.integer(num)
}

# checkmate::assert_string(x, min.chars = 1) accepts "   " (3 characters, all
# whitespace) -- real enough to pass that check, but a sentence built around
# it reads as broken ("In   , 10 of 20..."). Catches what min.chars alone
# cannot.
.assert_nonblank_string <- function(x, label) {
  checkmate::assert_string(x, min.chars = 1)
  if (!nzchar(trimws(x))) {
    stop(sprintf("%s must not be empty or whitespace-only", label), call. = FALSE)
  }
}

#' Empirical spell age of an attribute across a longitudinal panel
#'
#' When a registry does not carry a reliable per-field `last_updated` timestamp
#' (or when historical snapshots only record the attribute value each year),
#' a record's staleness can still be measured empirically from a multi-year
#' panel: how many consecutive years has the provider's current attribute value
#' sat unchanged?
#'
#' A calendar-year gap in the panel (e.g. observed in 2020 and 2024, but absent
#' in 2021-2023) breaks the uninterrupted spell so that an unobserved multi-year
#' gap is never misclassified as verified continuity.
#'
#' @param panel Data frame containing at least `id`, `year`, and one or more
#'   attribute columns.
#' @param attribute Name of the attribute column whose consecutive-year spell
#'   age is measured (e.g. `"street"` or `"state"`).
#' @param id Name of the identifier column. Default `"npi"`.
#' @param year Name of the integer year column. Default `"year"`.
#' Values are trimmed and upper-cased before comparison, so `"10 Main St"`
#' and `"10 MAIN ST "` continue the same spell. A missing value breaks the
#' spell on both sides: the missing row gets `NA` spell columns, and the next
#' observed value starts a new spell. Spell age is therefore a *lower bound*
#' on how long a value has sat unchanged -- the clock starts at the first
#' panel year, so a value already present in that year is never older than
#' `year - first_panel_year`.
#'
#' @return A tibble of `panel` ordered by `(id, year)`, with `id` coerced to
#'   trimmed character, `year` to integer, and three added columns:
#'   `first_panel_year` (earliest year observed for `id`),
#'   `spell_start_year` (first year of the current uninterrupted consecutive-year
#'   spell of `attribute`), and `spell_age_years` (`year - spell_start_year`;
#'   `0` in the year a value first appears). Errors on duplicate `(id, year)`
#'   rows, missing ids, or non-integer years.
#' @examples
#' p <- data.frame(
#'   npi = c("1", "1", "1", "2", "2"),
#'   year = c(2022, 2023, 2024, 2023, 2024),
#'   street = c("10 MAIN ST", "10 MAIN ST", "20 OAK AVE", "5 ELM ST", "5 ELM ST")
#' )
#' attribute_spell_age(p, attribute = "street")
#' @seealso [attribute_agreement_by_spell_age()], which stratifies agreement by
#'   this column; `vignette("lead-lag-staleness")`.
#' @family no-gold-standard comparison
#' @export
attribute_spell_age <- function(panel, attribute, id = "npi", year = "year") {
  checkmate::assert_data_frame(panel, min.rows = 1)
  checkmate::assert_string(attribute, min.chars = 1)
  checkmate::assert_string(id, min.chars = 1)
  checkmate::assert_string(year, min.chars = 1)
  for (col in c(id, year, attribute)) {
    if (!col %in% names(panel)) {
      stop(sprintf("attribute_spell_age: panel has no column '%s'", col), call. = FALSE)
    }
  }

  out <- panel
  out[[id]] <- trimws(as.character(out[[id]]))
  if (any(is.na(out[[id]]) | !nzchar(out[[id]]))) {
    stop("attribute_spell_age: panel contains missing or empty id values", call. = FALSE)
  }
  out[[year]] <- .parse_whole_integer(out[[year]], "attribute_spell_age: year", allow_na = FALSE)
  .assert_unique_keys(out, c(id, year), "attribute_spell_age: panel")

  val_clean <- toupper(trimws(as.character(out[[attribute]])))
  out$.val_clean <- ifelse(is.na(val_clean) | !nzchar(val_clean), NA_character_, val_clean)

  out <- out[order(out[[id]], out[[year]]), , drop = FALSE]
  groups <- split(out, out[[id]], drop = TRUE)
  res <- lapply(groups, function(g) {
    v <- g$.val_clean
    yrs <- g[[year]]
    n <- nrow(g)
    changed <- rep(TRUE, n)
    if (n > 1L) {
      for (i in 2:n) {
        prev <- v[i - 1L]
        curr <- v[i]
        consec_yr <- (yrs[i] == yrs[i - 1L] + 1L)
        changed[i] <- !consec_yr || is.na(prev) || is.na(curr) || (curr != prev)
      }
    }
    spell_id <- cumsum(changed)
    spell_start <- as.integer(stats::ave(yrs, spell_id, FUN = min))
    is_missing_val <- is.na(v)
    spell_start[is_missing_val] <- NA_integer_
    g$first_panel_year <- min(yrs, na.rm = TRUE)
    g$spell_start_year <- spell_start
    g$spell_age_years <- ifelse(is_missing_val, NA_integer_, as.integer(yrs - spell_start))
    g$.val_clean <- NULL
    g
  })
  tibble::as_tibble(dplyr::bind_rows(res))
}

#' How often does one source's own value change, independent of whether it's right?
#'
#' A source that flips its reported value every year is less trustworthy than
#' one that holds steady, even setting aside which one is *correct* -- a
#' question [attribute_spell_age()] already answers for a single current
#' value ("how old is this specific record") but does not aggregate into a
#' rate that one source's overall volatility can be compared to another's.
#' This does that: among a single source's own consecutive-year transitions,
#' what fraction show a changed value at all (no second source, no
#' "is this the right change" judgment -- pure churn).
#'
#' @details
#' A transition counts only when the identifier is present in *both*
#' consecutive years with a non-missing value -- a calendar-year gap (e.g.
#' observed in 2020 and 2022 but absent in 2021) is not counted as a
#' transition at all, the same convention [attribute_spell_age()] uses for
#' breaking a spell. Values are trimmed and upper-cased before comparison.
#'
#' **Compare two sources' churn with [format_proportion_comparison_prose()],
#' not a separate function here** -- `overall$n_changed`/`overall$n_transitions`
#' from two calls are exactly the `n_a`/`n_total_a`/`n_b`/`n_total_b` that
#' function already expects (see Examples), so no redundant wrapper is
#' needed just to get a two-proportion test and a sentence out of it.
#'
#' **Churn is not staleness, and neither implies accuracy.** A source that
#' never changes might be current and correct, or might simply never be
#' re-observed. A source that changes constantly might be tracking a
#' genuinely mobile population, or might be unreliable data entry. This
#' function reports the rate; what it means for a specific source is a
#' judgment call outside its scope.
#'
#' @param panel Data frame with at least `id`, `year`, and `attribute`.
#' @param attribute Name of the attribute column to track.
#' @param id,year Column names. Defaults `"npi"`, `"year"`.
#' @param conf_level Confidence level for the overall and by-year Wilson
#'   intervals. Default `0.95`.
#' @return A list:
#'   \describe{
#'     \item{`overall`}{List: `n_transitions`, `n_changed`, `pct`, `ci_low`,
#'       `ci_high` (Wilson, via [wilson_ci()]).}
#'     \item{`by_year`}{Tibble, one row per transition `year_from`/`year_to`:
#'       `n_transitions`, `n_changed`, `pct`, `ci_low`, `ci_high`.}
#'     \item{`by_identifier`}{Tibble, one row per identifier that has at
#'       least one transition: `n_transitions`, `n_changed`.}
#'     \item{`change_count_distribution`}{Tibble: how many identifiers had
#'       `0`, `1`, `2`, or `3+` changes, with `n` and `pct` (of identifiers
#'       with at least one transition) -- separates a source whose churn is
#'       spread evenly from one where a handful of identifiers account for
#'       most of it.}
#'   }
#' @examples
#' panel <- data.frame(
#'   npi = c("1", "1", "1", "2", "2", "2"),
#'   year = c(2022, 2023, 2024, 2022, 2023, 2024),
#'   state = c("CO", "CO", "CO", "NM", "TX", "NM")
#' )
#' churn <- attribute_churn_rate(panel, attribute = "state")
#' churn$overall
#'
#' # Comparing two sources' churn reuses format_proportion_comparison_prose():
#' panel_b <- data.frame(
#'   npi = c("1", "1", "1", "2", "2", "2"),
#'   year = c(2022, 2023, 2024, 2022, 2023, 2024),
#'   state = c("CO", "CO", "CO", "NM", "NM", "NM")
#' )
#' churn_a <- attribute_churn_rate(panel, attribute = "state")
#' churn_b <- attribute_churn_rate(panel_b, attribute = "state")
#' format_proportion_comparison_prose(
#'   n_a = churn_a$overall$n_changed, n_total_a = churn_a$overall$n_transitions,
#'   n_b = churn_b$overall$n_changed, n_total_b = churn_b$overall$n_transitions,
#'   label_a = "Source A", label_b = "Source B",
#'   unit_label = "consecutive-year transitions", condition_label = "showed a changed value"
#' )$comparison_sentence
#' @seealso [attribute_spell_age()]; [format_proportion_comparison_prose()];
#'   [attribute_coverage()] for the completeness side of comparing sources.
#' @family no-gold-standard comparison
#' @export
attribute_churn_rate <- function(panel, attribute, id = "npi", year = "year", conf_level = 0.95) {
  checkmate::assert_data_frame(panel, min.rows = 1)
  checkmate::assert_string(attribute, min.chars = 1)
  checkmate::assert_string(id, min.chars = 1)
  checkmate::assert_string(year, min.chars = 1)
  checkmate::assert_number(conf_level, lower = 0, upper = 1)
  for (col in c(id, year, attribute)) {
    if (!col %in% names(panel)) {
      stop(sprintf("attribute_churn_rate: panel has no column '%s'", col), call. = FALSE)
    }
  }

  out <- tibble::tibble(id = trimws(as.character(panel[[id]])))
  if (any(is.na(out$id) | !nzchar(out$id))) {
    stop("attribute_churn_rate: panel contains missing or empty id values", call. = FALSE)
  }
  out$year <- .parse_whole_integer(panel[[year]], "attribute_churn_rate: year", allow_na = FALSE)
  .assert_unique_keys(out, c("id", "year"), "attribute_churn_rate: panel")
  v <- toupper(trimws(as.character(panel[[attribute]])))
  out$value <- ifelse(is.na(v) | !nzchar(v), NA_character_, v)
  out <- out[order(out$id, out$year), , drop = FALSE]

  groups <- split(out, out$id, drop = TRUE)
  trans_list <- lapply(groups, function(g) {
    n <- nrow(g)
    if (n < 2L) return(NULL)
    year_from <- g$year[1:(n - 1L)]
    year_to <- g$year[2:n]
    val_from <- g$value[1:(n - 1L)]
    val_to <- g$value[2:n]
    consec <- (year_to == year_from + 1L) & !is.na(val_from) & !is.na(val_to)
    if (!any(consec)) return(NULL)
    tibble::tibble(
      id = g$id[1], year_from = year_from[consec], year_to = year_to[consec],
      changed = val_from[consec] != val_to[consec]
    )
  })
  transitions <- dplyr::bind_rows(trans_list)
  if (nrow(transitions) == 0L) {
    stop("attribute_churn_rate: no consecutive-year transitions with a non-missing value on both sides", call. = FALSE)
  }

  wci <- function(n_changed, n_total) {
    ci <- wilson_ci(n_changed, n_total, conf_level = conf_level, as_percent = TRUE)
    list(n_transitions = n_total, n_changed = n_changed,
         pct = round(100 * n_changed / n_total, 1), ci_low = ci$ci_low, ci_high = ci$ci_high)
  }

  overall <- wci(sum(transitions$changed), nrow(transitions))

  year_key <- paste(transitions$year_from, transitions$year_to, sep = "->")
  by_year <- tibble::tibble(
    year_from = as.integer(tapply(transitions$year_from, year_key, `[`, 1)),
    year_to = as.integer(tapply(transitions$year_to, year_key, `[`, 1)),
    n_transitions = as.integer(tapply(transitions$changed, year_key, length)),
    n_changed = as.integer(tapply(transitions$changed, year_key, sum))
  )
  by_year <- by_year[order(by_year$year_from), , drop = FALSE]
  by_year_ci <- wilson_ci(by_year$n_changed, by_year$n_transitions, conf_level = conf_level, as_percent = TRUE)
  by_year$pct <- round(100 * by_year$n_changed / by_year$n_transitions, 1)
  by_year$ci_low <- by_year_ci$ci_low
  by_year$ci_high <- by_year_ci$ci_high

  by_identifier <- tibble::tibble(
    id = names(tapply(transitions$changed, transitions$id, length)),
    n_transitions = as.integer(tapply(transitions$changed, transitions$id, length)),
    n_changed = as.integer(tapply(transitions$changed, transitions$id, sum))
  )

  bucket <- cut(by_identifier$n_changed, breaks = c(-Inf, 0, 1, 2, Inf),
                labels = c("0", "1", "2", "3+"), right = TRUE)
  tab <- table(bucket)
  change_count_distribution <- tibble::tibble(
    n_changes = names(tab), n = as.integer(tab),
    pct = round(100 * as.integer(tab) / nrow(by_identifier), 1)
  )

  list(
    overall = overall,
    by_year = tibble::as_tibble(by_year),
    by_identifier = tibble::as_tibble(by_identifier),
    change_count_distribution = change_count_distribution
  )
}

#' Which source actually has data for more entity-years?
#'
#' A currency/completeness question, distinct from timing (does a source
#' move first) or agreement (do sources say the same thing when both are
#' present): among a defined set of entity-years that *should* be covered,
#' how many does each source actually carry a non-missing value for? A
#' source that leads every race but is only present for a fifth of the
#' entity-years in question is a different kind of limitation than one that
#' lags but is nearly always there.
#'
#' @details
#' **The universe is supplied, not inferred, by default -- except when it
#' isn't.** Coverage is only meaningful relative to a stated target (a
#' cohort's NPI-years, a study's intended panel). If `universe` is `NULL`,
#' this falls back to the union of every `(id, year)` pair present in *any*
#' of `sources` -- convenient, but it silently inflates every source's
#' apparent coverage toward the sources that already define the union (a
#' source present nowhere outside its own rows trivially "covers" 100% of a
#' universe built from itself). Passing an explicit `universe` -- the actual
#' cohort being studied -- is recommended whenever one exists.
#'
#' **Presence means a non-missing `attribute` value**, not merely a row --
#' consistent with every other function in this package ("presence is never
#' inferred," see `README.md`'s Design rules). A row with an empty or `NA`
#' value does not count as coverage.
#'
#' @param sources Named list of data frames, each with `id`, `year`, and
#'   `attribute` columns -- same convention as
#'   [attribute_consensus_tier()]/[attribute_source_corroboration()].
#' @param attribute Name of the attribute column to check for a non-missing
#'   value.
#' @param universe Optional data frame of `(id, year)` pairs defining the
#'   entity-years coverage is measured against. If `NULL` (default), uses
#'   the union of `(id, year)` pairs across all of `sources` (see Details).
#' @param id,year Column names. Defaults `"npi"`, `"year"`.
#' @param conf_level Confidence level for the Wilson intervals. Default
#'   `0.95`.
#' @return A list:
#'   \describe{
#'     \item{`overall`}{Tibble, one row per source, ordered by descending
#'       `pct`: `source`, `n_covered`, `n_total` (size of `universe`),
#'       `pct`, `ci_low`, `ci_high` (Wilson, via [wilson_ci()]). `n_covered`/
#'       `n_total` from any two rows are exactly what
#'       [format_proportion_comparison_prose()] expects for a direct,
#'       statistically compared pairwise claim.}
#'     \item{`by_year`}{Tibble, one row per `(source, year)` present in
#'       `universe`: `source`, `year`, `n_covered`, `n_total`, `pct`,
#'       `ci_low`, `ci_high`.}
#'   }
#' @examples
#' nppes <- data.frame(npi = c("1", "1", "2"), year = c(2022, 2023, 2022), state = c("CO", "CO", "NM"))
#' dac <- data.frame(npi = c("1"), year = c(2022), state = c("CO"))
#' universe <- data.frame(npi = c("1", "1", "2"), year = c(2022, 2023, 2022))
#' cov <- attribute_coverage(list(nppes = nppes, dac = dac), attribute = "state", universe = universe)
#' cov$overall
#' @seealso [attribute_churn_rate()] for the volatility side of comparing
#'   sources; [format_proportion_comparison_prose()]; [wilson_ci()].
#' @family no-gold-standard comparison
#' @export
attribute_coverage <- function(sources, attribute, universe = NULL,
                                id = "npi", year = "year", conf_level = 0.95) {
  checkmate::assert_list(sources, min.len = 1, names = "unique")
  checkmate::assert_string(attribute, min.chars = 1)
  checkmate::assert_string(id, min.chars = 1)
  checkmate::assert_string(year, min.chars = 1)
  checkmate::assert_number(conf_level, lower = 0, upper = 1)

  clean_source <- function(x, label) {
    checkmate::assert_data_frame(x)
    for (col in c(id, year, attribute)) {
      if (!col %in% names(x)) {
        stop(sprintf("attribute_coverage: source '%s' has no column '%s'", label, col), call. = FALSE)
      }
    }
    res <- tibble::tibble(
      id = trimws(as.character(x[[id]])),
      year = .parse_whole_integer(x[[year]], sprintf("attribute_coverage: source '%s' year", label), allow_na = FALSE)
    )
    v <- toupper(trimws(as.character(x[[attribute]])))
    res$value <- ifelse(is.na(v) | !nzchar(v), NA_character_, v)
    res[!is.na(res$id) & nzchar(res$id), , drop = FALSE]
  }

  nm <- names(sources)
  cleaned <- lapply(nm, function(s) clean_source(sources[[s]], s))
  names(cleaned) <- nm

  if (is.null(universe)) {
    universe <- unique(dplyr::bind_rows(lapply(cleaned, function(x) x[, c("id", "year")])))
  } else {
    checkmate::assert_data_frame(universe, min.rows = 1)
    for (col in c(id, year)) {
      if (!col %in% names(universe)) {
        stop(sprintf("attribute_coverage: universe has no column '%s'", col), call. = FALSE)
      }
    }
    universe <- tibble::tibble(
      id = trimws(as.character(universe[[id]])),
      year = .parse_whole_integer(universe[[year]], "attribute_coverage: universe year", allow_na = FALSE)
    )
    universe <- unique(universe[!is.na(universe$id) & nzchar(universe$id), , drop = FALSE])
  }
  n_total <- nrow(universe)
  if (n_total == 0L) {
    stop("attribute_coverage: universe has zero distinct (id, year) pairs", call. = FALSE)
  }

  overall_rows <- lapply(nm, function(s) {
    cov <- cleaned[[s]]
    cov <- cov[!is.na(cov$value), c("id", "year"), drop = FALSE]
    m <- merge(universe, cov, by = c("id", "year"))
    n_covered <- nrow(m)
    ci <- wilson_ci(n_covered, n_total, conf_level = conf_level, as_percent = TRUE)
    tibble::tibble(source = s, n_covered = n_covered, n_total = n_total,
                   pct = round(100 * n_covered / n_total, 1), ci_low = ci$ci_low, ci_high = ci$ci_high)
  })
  overall <- dplyr::bind_rows(overall_rows)
  overall <- overall[order(-overall$pct), , drop = FALSE]

  by_year_rows <- lapply(nm, function(s) {
    cov <- cleaned[[s]]
    cov <- cov[!is.na(cov$value), c("id", "year"), drop = FALSE]
    years <- sort(unique(universe$year))
    sub_rows <- lapply(years, function(yr) {
      u_yr <- universe[universe$year == yr, , drop = FALSE]
      n_tot_yr <- nrow(u_yr)
      n_cov_yr <- nrow(merge(u_yr, cov[cov$year == yr, , drop = FALSE], by = c("id", "year")))
      tibble::tibble(source = s, year = yr, n_covered = n_cov_yr, n_total = n_tot_yr)
    })
    dplyr::bind_rows(sub_rows)
  })
  by_year <- dplyr::bind_rows(by_year_rows)
  by_year_ci <- wilson_ci(by_year$n_covered, by_year$n_total, conf_level = conf_level, as_percent = TRUE)
  by_year$pct <- round(100 * by_year$n_covered / by_year$n_total, 1)
  by_year$ci_low <- by_year_ci$ci_low
  by_year$ci_high <- by_year_ci$ci_high

  list(overall = tibble::as_tibble(overall), by_year = tibble::as_tibble(by_year))
}

#' Approach 1: Longitudinal lead-lag panel matrix between two registries
#'
#' Without a gold-standard registry, a single-year disagreement between Source A
#' and Source B (`A_t != B_t`) is symmetric. Observing both sources across
#' consecutive years (`t` and `t + 1`) and multi-year follow-up resolves which
#' source leads and which source lags:
#'
#' * **Concordance-breaking at `t -> t + 1`:** Among records where `A_t == B_t`,
#'   which source changes first at `t + 1`?
#' * **1-year directional convergence (`t -> t + 1`):** Among records where
#'   `A_t != B_t`, how often does Source B update at `t + 1` to adopt `A_t`
#'   without a cross-over swap (`B_{t+1} == A_t` and `A_{t+1} != B_t`) versus
#'   Source A reverting at `t + 1` to match `B_t` (`A_{t+1} == B_t` and
#'   `B_{t+1} != A_t`)?
#' * **Multi-year forward convergence (`base_year -> follow-up`):** Among
#'   disagreements in `base_year`, what share does Source B eventually adopt
#'   in any subsequent year (`year > base_year`) versus Source A reverting to
#'   `B_{base_year}`?
#' * **Historical back-check (`cross_section` vs. prior `source_b`):**
#'   Optionally, when a third cross-sectional source (e.g. DAC at
#'   `cross_section_year`) disagrees with Source B at `cross_section_year`, how
#'   often did the cross-sectional source's value already appear in Source B in
#'   an earlier year (`year < cross_section_year`)?
#'
#' @details
#' Values are trimmed and upper-cased, ids trimmed. A transition `t -> t + 1`
#' counts only when *both* sources report the identifier in both years with
#' non-missing values for that attribute, so `n_paired_transitions` can
#' differ by attribute. Each source must have one row per `(id, year)`.
#'
#' The concordance-breaking counts are not exhaustive: among concordant
#' pairs, `a_breaks_concordance_first` (A changes, B holds),
#' `b_breaks_concordance_first` (B changes, A holds), and
#' `both_move_together_at_t1` (both change to the same new value) leave out
#' pairs that stay concordant and pairs where both change to different
#' values. Likewise the 1-year discordant outcomes (`b_adopts_a`,
#' `a_reverts_to_b`, `persistent_discordant`) need not sum to 100%: mutual
#' swaps (`B_{t+1} == A_t` and `A_{t+1} == B_t`) and moves to a third value
#' are in none of them.
#'
#' Multi-year convergence counts an identifier once: B "adopts" if *any*
#' later B year equals `A_{base_year}`, A "reverts" if *any* later A year
#' equals `B_{base_year}`. The two are not mutually exclusive.
#'
#' A source that leads is informative; a source that never leads is not
#' thereby correct -- it may simply never be re-observed independently.
#'
#' @param source_a Data frame for longitudinal Source A (e.g. Open Payments),
#'   with `id`, `year`, and one or more `attributes` columns.
#' @param source_b Data frame for longitudinal Source B (e.g. NPPES panel),
#'   with `id`, `year`, and the same `attributes` columns.
#' @param attributes Character vector of unique attribute column names to evaluate
#'   (e.g. `c("state", "city", "zip5", "street")`).
#' @param id Name of the identifier column. Default `"npi"`.
#' @param year Name of the integer year column. Default `"year"`.
#' @param base_year Baseline year for multi-year forward convergence tracking.
#'   If `NULL` (default), uses the earliest year shared by `source_a` and `source_b`
#'   that has at least one follow-up year.
#' @param cross_section Optional data frame for a cross-sectional source (e.g.
#'   CMS DAC) containing `id` and `attributes`, to back-check whether its
#'   discordant values in `cross_section_year` ever appeared in `source_b`
#'   prior to `cross_section_year`.
#' @param cross_section_year Integer year corresponding to `cross_section`.
#'   Default is `max(source_b[[year]])`.
#' @return A tibble with one row per attribute in `attributes`. Counts are
#'   integers and `_pct` columns are percentages rounded to one decimal:
#'   \describe{
#'     \item{`attribute`}{Attribute name.}
#'     \item{`n_paired_transitions`}{Consecutive-year transitions with all
#'       four values (`A_t`, `B_t`, `A_{t+1}`, `B_{t+1}`) present.}
#'     \item{`n_concordant_at_t`, `n_discordant_at_t`}{Of those, transitions
#'       with `A_t == B_t` and `A_t != B_t`.}
#'     \item{`a_breaks_concordance_first`, `b_breaks_concordance_first`}{
#'       Concordant at `t`; only A (or only B) changed at `t + 1`.}
#'     \item{`both_move_together_at_t1`}{Concordant at `t`; both moved to the
#'       same new value at `t + 1`.}
#'     \item{`b_adopts_a_at_t1_n`, `b_adopts_a_at_t1_pct`}{Discordant at `t`;
#'       `B_{t+1} == A_t`, swaps excluded. Percent of `n_discordant_at_t`.}
#'     \item{`a_reverts_to_b_at_t1_n`, `a_reverts_to_b_at_t1_pct`}{
#'       Discordant at `t`; `A_{t+1} == B_t`, swaps excluded.}
#'     \item{`persistent_discordant_at_t1_pct`}{Discordant at `t`; neither
#'       source changed.}
#'     \item{`base_year`}{Year used for multi-year tracking.}
#'     \item{`disc_base_n`}{Identifiers discordant in `base_year`.}
#'     \item{`b_adopts_a_multi_n`, `b_adopts_a_multi_pct`}{Of those, B equals
#'       `A_{base_year}` in any later year. `NA` if `disc_base_n == 0`.}
#'     \item{`a_reverts_b_multi_n`, `a_reverts_b_multi_pct`}{Of those, A
#'       equals `B_{base_year}` in any later year. The percent's denominator
#'       is the identifiers A observes after `base_year`, not `disc_base_n`.}
#'     \item{`cross_section_disc_n`}{Identifiers where `cross_section`
#'       disagrees with B in `cross_section_year`. `NA` without
#'       `cross_section`.}
#'     \item{`cross_section_in_prior_b_n`, `cross_section_in_prior_b_pct`}{Of
#'       those, the cross-section's value appears in B in an earlier year.}
#'   }
#' @examples
#' a <- data.frame(npi = rep(c("1", "2"), each = 3), year = rep(2022:2024, 2),
#'                 state = c("CO", "NM", "NM", "NY", "NY", "NY"))
#' b <- data.frame(npi = rep(c("1", "2"), each = 3), year = rep(2022:2024, 2),
#'                 state = c("CO", "CO", "NM", "NJ", "NY", "NY"))
#' attribute_lead_lag_matrix(a, b, attributes = "state")
#' @seealso `vignette("lead-lag-staleness")` for worked examples and figures.
#' @family no-gold-standard comparison
#' @export
attribute_lead_lag_matrix <- function(source_a, source_b, attributes,
                                      id = "npi", year = "year",
                                      base_year = NULL,
                                      cross_section = NULL,
                                      cross_section_year = NULL) {
  checkmate::assert_data_frame(source_a, min.rows = 1)
  checkmate::assert_data_frame(source_b, min.rows = 1)
  checkmate::assert_character(attributes, min.len = 1, any.missing = FALSE, unique = TRUE)
  checkmate::assert_string(id, min.chars = 1)
  checkmate::assert_string(year, min.chars = 1)

  if (any(attributes %in% c(id, year, "id", "year"))) {
    stop("attribute_lead_lag_matrix: attributes cannot be the same as id or year", call. = FALSE)
  }

  for (col in c(id, year, attributes)) {
    if (!col %in% names(source_a)) {
      stop(sprintf("attribute_lead_lag_matrix: source_a has no column '%s'", col), call. = FALSE)
    }
    if (!col %in% names(source_b)) {
      stop(sprintf("attribute_lead_lag_matrix: source_b has no column '%s'", col), call. = FALSE)
    }
  }
  if (!is.null(cross_section)) {
    checkmate::assert_data_frame(cross_section, min.rows = 1)
    for (col in c(id, attributes)) {
      if (!col %in% names(cross_section)) {
        stop(sprintf("attribute_lead_lag_matrix: cross_section has no column '%s'", col), call. = FALSE)
      }
    }
  }

  clean_df <- function(df, cols, has_year = TRUE, label = "data") {
    res <- tibble::tibble(id = trimws(as.character(df[[id]])))
    if (any(is.na(res$id) | !nzchar(res$id))) {
      stop(sprintf("attribute_lead_lag_matrix: %s has missing or empty id values", label), call. = FALSE)
    }
    if (has_year) {
      res$year <- .parse_whole_integer(df[[year]], sprintf("attribute_lead_lag_matrix: %s year", label), allow_na = FALSE)
      .assert_unique_keys(res, c("id", "year"), sprintf("attribute_lead_lag_matrix: %s", label))
    } else {
      .assert_unique_keys(res, "id", sprintf("attribute_lead_lag_matrix: %s", label))
    }
    for (at in cols) {
      v <- toupper(trimws(as.character(df[[at]])))
      res[[at]] <- ifelse(is.na(v) | !nzchar(v), NA_character_, v)
    }
    res
  }

  da <- clean_df(source_a, attributes, has_year = TRUE, label = "source_a")
  db <- clean_df(source_b, attributes, has_year = TRUE, label = "source_b")

  shared_years <- sort(intersect(unique(da$year), unique(db$year)))
  if (is.null(base_year)) {
    cand <- shared_years[shared_years < max(shared_years)]
    base_year <- if (length(cand)) min(cand) else min(shared_years)
  }
  base_year <- as.integer(base_year)

  if (is.null(cross_section_year)) {
    cross_section_year <- max(db$year, na.rm = TRUE)
  }
  cross_section_year <- as.integer(cross_section_year)
  if (!is.null(cross_section) && !cross_section_year %in% db$year) {
    stop(sprintf("attribute_lead_lag_matrix: cross_section_year %d is not present in source_b", cross_section_year), call. = FALSE)
  }

  da_next <- da
  da_next$year <- da_next$year - 1L
  names(da_next)[names(da_next) %in% attributes] <- paste0(attributes, "_a_next")

  db_next <- db
  db_next$year <- db_next$year - 1L
  names(db_next)[names(db_next) %in% attributes] <- paste0(attributes, "_b_next")

  da_curr <- da
  names(da_curr)[names(da_curr) %in% attributes] <- paste0(attributes, "_a")
  db_curr <- db
  names(db_curr)[names(db_curr) %in% attributes] <- paste0(attributes, "_b")

  paired <- merge(da_curr, db_curr, by = c("id", "year"))
  paired <- merge(paired, da_next, by = c("id", "year"))
  paired <- merge(paired, db_next, by = c("id", "year"))
  if (nrow(paired) == 0L) {
    stop("attribute_lead_lag_matrix: zero paired consecutive-year transitions between source_a and source_b", call. = FALSE)
  }

  base_joined <- merge(
    da_curr[da_curr$year == base_year, , drop = FALSE],
    db_curr[db_curr$year == base_year, , drop = FALSE],
    by = c("id", "year")
  )

  cs_joined <- NULL
  if (!is.null(cross_section)) {
    dcs <- clean_df(cross_section, attributes, has_year = FALSE, label = "cross_section")
    names(dcs)[names(dcs) %in% attributes] <- paste0(attributes, "_cs")
    cs_joined <- merge(dcs, db_curr[db_curr$year == cross_section_year, , drop = FALSE], by = "id")
  }

  rows <- lapply(attributes, function(lv) {
    a_t    <- paired[[paste0(lv, "_a")]]
    b_t    <- paired[[paste0(lv, "_b")]]
    a_next <- paired[[paste0(lv, "_a_next")]]
    b_next <- paired[[paste0(lv, "_b_next")]]

    valid <- !is.na(a_t) & !is.na(b_t) & !is.na(a_next) & !is.na(b_next)
    conc  <- valid & (a_t == b_t)
    disc  <- valid & (a_t != b_t)

    a_breaks <- sum(conc & (a_next != a_t) & (b_next == b_t))
    b_breaks <- sum(conc & (b_next != b_t) & (a_next == a_t))
    both_mv  <- sum(conc & (a_next != a_t) & (b_next == a_next))

    n_disc <- sum(disc)
    # Exclude mutual cross-over swaps (where b_next == a_t AND a_next == b_t) from unilateral adoption/reversion
    is_swap <- disc & (b_next == a_t) & (a_next == b_t)
    b_adopts_1yr  <- sum(disc & (b_next == a_t) & !is_swap)
    a_reverts_1yr <- sum(disc & (a_next == b_t) & !is_swap)
    persist_1yr   <- sum(disc & (a_next == a_t) & (b_next == b_t))

    a_col <- paste0(lv, "_a")
    b_col <- paste0(lv, "_b")
    d_base <- base_joined[!is.na(base_joined[[a_col]]) & !is.na(base_joined[[b_col]]) &
                            base_joined[[a_col]] != base_joined[[b_col]], , drop = FALSE]

    disc_base_n <- nrow(d_base)
    b_multi_n <- NA_integer_; b_multi_pct <- NA_real_
    a_multi_n <- NA_integer_; a_multi_pct <- NA_real_

    if (disc_base_n > 0L) {
      b_fut <- db[db$year > base_year & db$id %in% d_base$id, c("id", lv), drop = FALSE]
      names(b_fut)[2] <- "fut_b"
      m_b <- merge(d_base[, c("id", a_col)], b_fut, by = "id", all.x = TRUE)
      by_id_b <- tapply(m_b$fut_b == m_b[[a_col]], m_b$id, function(x) any(x, na.rm = TRUE))
      b_multi_n <- sum(by_id_b)
      b_multi_pct <- round(100 * mean(by_id_b), 1)

      a_fut <- da[da$year > base_year & da$id %in% d_base$id, c("id", lv), drop = FALSE]
      names(a_fut)[2] <- "fut_a"
      m_a <- merge(d_base[, c("id", b_col)], a_fut, by = "id")
      if (nrow(m_a) > 0L) {
        by_id_a <- tapply(m_a$fut_a == m_a[[b_col]], m_a$id, function(x) any(x, na.rm = TRUE))
        a_multi_n <- sum(by_id_a)
        a_multi_pct <- round(100 * mean(by_id_a), 1)
      } else {
        a_multi_n <- 0L
        a_multi_pct <- 0.0
      }
    }

    cs_disc_n <- NA_integer_; cs_prior_n <- NA_integer_; cs_prior_pct <- NA_real_
    if (!is.null(cs_joined)) {
      cs_col <- paste0(lv, "_cs")
      d_cs <- cs_joined[!is.na(cs_joined[[cs_col]]) & !is.na(cs_joined[[b_col]]) &
                          cs_joined[[cs_col]] != cs_joined[[b_col]], , drop = FALSE]
      cs_disc_n <- nrow(d_cs)
      if (cs_disc_n > 0L) {
        b_hist <- db[db$year < cross_section_year & db$id %in% d_cs$id, c("id", lv), drop = FALSE]
        names(b_hist)[2] <- "hist_b"
        m_cs <- merge(d_cs[, c("id", cs_col)], b_hist, by = "id", all.x = TRUE)
        by_id_cs <- tapply(m_cs$hist_b == m_cs[[cs_col]], m_cs$id, function(x) any(x, na.rm = TRUE))
        cs_prior_n <- sum(by_id_cs)
        cs_prior_pct <- round(100 * mean(by_id_cs), 1)
      }
    }

    tibble::tibble(
      attribute                       = lv,
      n_paired_transitions            = sum(valid),
      n_concordant_at_t               = sum(conc),
      a_breaks_concordance_first      = a_breaks,
      b_breaks_concordance_first      = b_breaks,
      both_move_together_at_t1        = both_mv,
      n_discordant_at_t               = n_disc,
      b_adopts_a_at_t1_n              = b_adopts_1yr,
      b_adopts_a_at_t1_pct            = if (n_disc > 0L) round(100 * b_adopts_1yr / n_disc, 1) else NA_real_,
      a_reverts_to_b_at_t1_n          = a_reverts_1yr,
      a_reverts_to_b_at_t1_pct        = if (n_disc > 0L) round(100 * a_reverts_1yr / n_disc, 1) else NA_real_,
      persistent_discordant_at_t1_pct = if (n_disc > 0L) round(100 * persist_1yr / n_disc, 1) else NA_real_,
      base_year                       = base_year,
      disc_base_n                     = disc_base_n,
      b_adopts_a_multi_n              = b_multi_n,
      b_adopts_a_multi_pct            = b_multi_pct,
      a_reverts_b_multi_n             = a_multi_n,
      a_reverts_b_multi_pct           = a_multi_pct,
      cross_section_disc_n            = cs_disc_n,
      cross_section_in_prior_b_n      = cs_prior_n,
      cross_section_in_prior_b_pct    = cs_prior_pct
    )
  })

  tibble::as_tibble(dplyr::bind_rows(rows))
}

#' How many years until one source adopts the other's disputed value?
#'
#' [attribute_lead_lag_matrix()]'s multi-year convergence columns
#' (`b_adopts_a_multi_n`/`pct`, `a_reverts_b_multi_n`/`pct`) answer whether a
#' source eventually adopts the other's `base_year` value -- never how long
#' that takes. This answers the duration question: for every identifier
#' discordant at `base_year`, the number of years until each direction's
#' *first* adoption (not just whether one ever occurs), summarized as
#' n/median/IQR/mean/SD/CI for each direction, plus a manuscript sentence for
#' each -- the basis for saying one source is, say, "typically about a year
#' and a half ahead of" another, instead of only "adopts more often."
#'
#' @details
#' **Years, not months.** This package's longitudinal sources are annual
#' snapshots -- a value is only known "as of year `Y`," never to a specific
#' date -- so years is the finest unit these panels can honestly support.
#' Reporting a month-level lag from year-only data would overclaim the
#' panels' actual precision; a source with real calendar dates (not just an
#' annual snapshot) is a different, separate kind of comparison.
#'
#' **First adoption, not any adoption.** `attribute_lead_lag_matrix()`'s
#' `..._multi_pct` counts an identifier if the adopted value appears in *any*
#' later year. This function instead finds the *earliest* later year it
#' appears, because a duration has to measure from a specific point -- "ever,
#' eventually" has no length.
#'
#' **The mean's CI assumes the duration distribution is reasonably
#' well-behaved for a t-interval** (not literally normal -- year-counts are
#' discrete and often right-skewed with a handful of identifiers). The
#' median and IQR are reported alongside specifically so a heavily skewed
#' distribution is visible rather than hidden behind a mean and CI alone.
#' `ci_low`/`ci_high` are `NA` when fewer than 2 identifiers adopt (no spread
#' to estimate from).
#'
#' @param source_a,source_b Longitudinal data frames with `id`, `year`, and
#'   `attribute`, same shape as [attribute_lead_lag_matrix()]'s `source_a`/
#'   `source_b` (one row per `(id, year)` each).
#' @param attribute Single attribute column name to evaluate (not a vector --
#'   call once per attribute for more than one).
#' @param id,year Column names. Defaults `"npi"`, `"year"`.
#' @param base_year Baseline year for discordance. If `NULL` (default), the
#'   earliest year shared by both sources that has a follow-up year -- same
#'   resolution rule as [attribute_lead_lag_matrix()].
#' @param source_a_label,source_b_label Short names substituted into the
#'   sentences, e.g. `"CMS Open Payments"` / `"NPPES"`.
#' @param conf_level Confidence level for the mean's t-interval. Default
#'   `0.95`.
#' @return A list:
#'   \describe{
#'     \item{`base_year`}{The base year used.}
#'     \item{`disc_base_n`}{Identifiers with both sources present and
#'       disagreeing at `base_year`.}
#'     \item{`b_adopts`}{List: `years` (integer vector, one entry per
#'       identifier that ever adopts `source_a`'s base-year value, the
#'       number of years until its *first* occurrence), and `n`, `pct` (of
#'       `disc_base_n`), `median`, `p25`, `p75`, `mean`, `sd`, `ci_low`,
#'       `ci_high`.}
#'     \item{`a_reverts`}{Same shape as `b_adopts`, for `source_a` reverting
#'       to `source_b`'s base-year value.}
#'     \item{`b_adopts_sentence`,`a_reverts_sentence`}{One manuscript
#'       sentence each.}
#'     \item{`combined_text`}{Both sentences joined with a space.}
#'   }
#' @examples
#' a <- data.frame(
#'   npi = c("1", "1", "1", "1", "2", "2", "2"),
#'   year = c(2020, 2021, 2022, 2023, 2020, 2021, 2022),
#'   state = c("CO", "CO", "CO", "CO", "NM", "NM", "TX")
#' )
#' b <- data.frame(
#'   npi = c("1", "1", "1", "1", "2", "2", "2"),
#'   year = c(2020, 2021, 2022, 2023, 2020, 2021, 2022),
#'   state = c("NM", "NM", "CO", "CO", "NM", "TX", "TX")
#' )
#' dur <- attribute_lead_lag_duration(a, b, attribute = "state", base_year = 2020,
#'                                     source_a_label = "Source A", source_b_label = "Source B")
#' dur$combined_text
#' @seealso [attribute_lead_lag_matrix()] for the whether/how-often version
#'   this extends; `vignette("lead-lag-staleness")`.
#' @family no-gold-standard comparison
#' @export
attribute_lead_lag_duration <- function(source_a, source_b, attribute,
                                         id = "npi", year = "year", base_year = NULL,
                                         source_a_label = "Source A", source_b_label = "Source B",
                                         conf_level = 0.95) {
  checkmate::assert_data_frame(source_a, min.rows = 1)
  checkmate::assert_data_frame(source_b, min.rows = 1)
  checkmate::assert_string(attribute, min.chars = 1)
  checkmate::assert_string(id, min.chars = 1)
  checkmate::assert_string(year, min.chars = 1)
  .assert_nonblank_string(source_a_label, "attribute_lead_lag_duration: source_a_label")
  .assert_nonblank_string(source_b_label, "attribute_lead_lag_duration: source_b_label")
  checkmate::assert_number(conf_level, lower = 0, upper = 1)
  if (conf_level <= 0 || conf_level >= 1) {
    stop("attribute_lead_lag_duration: conf_level must be strictly between 0 and 1", call. = FALSE)
  }

  for (col in c(id, year, attribute)) {
    if (!col %in% names(source_a)) {
      stop(sprintf("attribute_lead_lag_duration: source_a has no column '%s'", col), call. = FALSE)
    }
    if (!col %in% names(source_b)) {
      stop(sprintf("attribute_lead_lag_duration: source_b has no column '%s'", col), call. = FALSE)
    }
  }

  clean_df <- function(df, label) {
    res <- tibble::tibble(id = trimws(as.character(df[[id]])))
    if (any(is.na(res$id) | !nzchar(res$id))) {
      stop(sprintf("attribute_lead_lag_duration: %s has missing or empty id values", label), call. = FALSE)
    }
    res$year <- .parse_whole_integer(df[[year]], sprintf("attribute_lead_lag_duration: %s year", label), allow_na = FALSE)
    .assert_unique_keys(res, c("id", "year"), sprintf("attribute_lead_lag_duration: %s", label))
    v <- toupper(trimws(as.character(df[[attribute]])))
    res$value <- ifelse(is.na(v) | !nzchar(v), NA_character_, v)
    res
  }

  da <- clean_df(source_a, "source_a")
  db <- clean_df(source_b, "source_b")

  shared_years <- sort(intersect(unique(da$year), unique(db$year)))
  if (is.null(base_year)) {
    cand <- shared_years[shared_years < max(shared_years)]
    base_year <- if (length(cand)) min(cand) else min(shared_years)
  }
  base_year <- as.integer(base_year)

  a_base <- da[da$year == base_year & !is.na(da$value), c("id", "value"), drop = FALSE]
  names(a_base)[2] <- "a_value"
  b_base <- db[db$year == base_year & !is.na(db$value), c("id", "value"), drop = FALSE]
  names(b_base)[2] <- "b_value"
  base_joined <- merge(a_base, b_base, by = "id")
  d_base <- base_joined[base_joined$a_value != base_joined$b_value, , drop = FALSE]
  disc_base_n <- nrow(d_base)

  first_adoption_years <- function(future_source, target_ids_values, target_col) {
    # target_ids_values: data frame with id + the value each id must eventually match
    fut <- future_source[future_source$year > base_year & future_source$id %in% target_ids_values$id,
                          c("id", "year", "value"), drop = FALSE]
    if (nrow(fut) == 0L) return(integer(0))
    m <- merge(fut, target_ids_values, by = "id")
    hit <- m[!is.na(m$value) & m$value == m[[target_col]], c("id", "year"), drop = FALSE]
    if (nrow(hit) == 0L) return(integer(0))
    first_year <- tapply(hit$year, hit$id, min)
    as.integer(first_year) - base_year
  }

  summarize_duration <- function(years_vec, denom) {
    n <- length(years_vec)
    pct <- if (denom > 0L) round(100 * n / denom, 1) else NA_real_
    if (n == 0L) {
      return(list(years = years_vec, n = 0L, pct = pct, median = NA_real_, p25 = NA_real_, p75 = NA_real_,
                  mean = NA_real_, sd = NA_real_, ci_low = NA_real_, ci_high = NA_real_))
    }
    med <- stats::median(years_vec)
    qs <- stats::quantile(years_vec, c(0.25, 0.75), type = 7, names = FALSE)
    m <- mean(years_vec)
    s <- if (n >= 2L) stats::sd(years_vec) else NA_real_
    if (n >= 2L && !is.na(s) && s > 0) {
      tcrit <- stats::qt(1 - (1 - conf_level) / 2, df = n - 1L)
      se <- s / sqrt(n)
      ci_low <- m - tcrit * se
      ci_high <- m + tcrit * se
    } else {
      ci_low <- NA_real_
      ci_high <- NA_real_
    }
    list(years = years_vec, n = n, pct = pct, median = med, p25 = unname(qs[1]), p75 = unname(qs[2]),
         mean = round(m, 2), sd = if (is.na(s)) NA_real_ else round(s, 2),
         ci_low = if (is.na(ci_low)) NA_real_ else round(ci_low, 2),
         ci_high = if (is.na(ci_high)) NA_real_ else round(ci_high, 2))
  }

  fmt1 <- function(x) if (is.na(x)) "NA" else sprintf("%.1f", x)
  conf_pct <- round(100 * conf_level)

  noun_id <- function(n) if (n == 1L) "identifier" else "identifiers"

  sentence_for <- function(d, leader_label, follower_label, verb) {
    if (disc_base_n == 0L) {
      return(sprintf(
        "No identifiers had both %s and %s present and disagreeing in %d, so no adoption timing could be measured.",
        source_a_label, source_b_label, base_year
      ))
    }
    if (d$n == 0L) {
      return(sprintf(
        "Of %d %s where %s and %s disagreed in %d, %s never %s %s's %d value in any later year.",
        disc_base_n, noun_id(disc_base_n), source_a_label, source_b_label, base_year, follower_label, verb, leader_label, base_year
      ))
    }
    ci_part <- if (is.na(d$ci_low)) "" else sprintf(", %d%% CI %s-%s years", conf_pct, fmt1(d$ci_low), fmt1(d$ci_high))
    sprintf(
      paste0(
        "Of %d %s where %s and %s disagreed in %d, %d (%.1f%%) %s %s %s's %d value, ",
        "after a median of %s years (IQR %s-%s; mean %s years%s)."
      ),
      disc_base_n, noun_id(disc_base_n), source_a_label, source_b_label, base_year, d$n, d$pct, follower_label, verb, leader_label, base_year,
      fmt1(d$median), fmt1(d$p25), fmt1(d$p75), fmt1(d$mean), ci_part
    )
  }

  if (disc_base_n == 0L) {
    b_adopts <- summarize_duration(integer(0), 0L)
    a_reverts <- summarize_duration(integer(0), 0L)
  } else {
    b_adopts_years <- first_adoption_years(db, d_base[, c("id", "a_value")], "a_value")
    a_reverts_years <- first_adoption_years(da, d_base[, c("id", "b_value")], "b_value")
    b_adopts <- summarize_duration(b_adopts_years, disc_base_n)
    a_reverts <- summarize_duration(a_reverts_years, disc_base_n)
  }

  b_adopts_sentence <- sentence_for(b_adopts, source_a_label, source_b_label, "adopted")
  a_reverts_sentence <- sentence_for(a_reverts, source_b_label, source_a_label, "reverted to")

  structure(
    list(
      base_year = base_year,
      disc_base_n = disc_base_n,
      b_adopts = b_adopts,
      a_reverts = a_reverts,
      b_adopts_sentence = b_adopts_sentence,
      a_reverts_sentence = a_reverts_sentence,
      combined_text = paste(b_adopts_sentence, a_reverts_sentence)
    ),
    class = "mysteryroster_lead_lag_duration"
  )
}

#' @export
print.mysteryroster_lead_lag_duration <- function(x, ...) {
  cat(sprintf(
    "<mysteryroster_lead_lag_duration>\n  base year: %d | %d identifier%s discordant\n\n",
    x$base_year, x$disc_base_n, if (x$disc_base_n == 1L) "" else "s"
  ))
  cat(strwrap(x$b_adopts_sentence, width = 76, indent = 2, exdent = 2), sep = "\n")
  cat("\n")
  cat(strwrap(x$a_reverts_sentence, width = 76, indent = 2, exdent = 2), sep = "\n")
  cat("\n")
  invisible(x)
}

#' Kaplan-Meier-style catch-up curve: how long until a source shows a value another already has?
#'
#' [attribute_lead_lag_duration()] answers a related question with a real limitation:
#' its median/IQR is computed only over identifiers that *eventually* resolve within the
#' observed panel -- one discordant at `base_year` who never catches up simply disappears
#' from that summary, understating how long a slow source actually takes. This answers the
#' same family of question the way time-to-event analysis is built to: as a proper
#' right-censored Kaplan-Meier survival curve, where a value never (yet) shown by a source
#' stays correctly "at risk" of eventually being shown for as long as that identifier's own
#' follow-up window lasts, then drops out once follow-up runs out -- never silently excluded,
#' never miscounted as "never".
#'
#' Ported and generalized from a `tyler.muffly`/Devin-AI co-authored exploratory analysis
#' (`vignette("lead-lag-staleness")`'s "How many years ahead or behind?" section, commit
#' `5892254` on branch `devin/1793847201-lead-lag-years-ahead`) -- the censoring-aware
#' survival-curve mechanics are that work's own real methodological contribution, ported here
#' as a tested, documented, reusable function instead of inline, panel-specific vignette code.
#'
#' @details
#' **What counts as "a value arriving".** For each source independently, an identifier's
#' *arrival* at a value is a consecutive-year transition (year `t` to `t + 1`, no calendar-year
#' gap -- the same convention [attribute_churn_rate()]/[attribute_spell_age()] use) where the
#' value changes. An identifier's first observed year in a source is never itself an arrival
#' (there is no prior value to differ from).
#'
#' **What counts as a source "showing" the value, for catch-up purposes.** This is
#' deliberately looser than the arrival rule above: once an `(id, value)` event's `start`
#' year is known (pooled across all sources, see below), a source is credited with showing
#' that value as soon as *any* of its own records -- at or after `start` -- carries that
#' exact value, whether or not its own immediately preceding record was the year before.
#' Requiring consecutive years here too would silently undercount catch-up for any source
#' sampled less frequently than annually (e.g. enrollment- or claims-derived data with
#' multi-year gaps between records): such a source can display the correct value while its
#' own prior record for that identifier is several years earlier, which the strict
#' consecutive-year rule would never recognize as a match. A record of that exact value from
#' *before* `start` -- a coincidental earlier occurrence unrelated to the tracked arrival --
#' does not count.
#'
#' **What defines one "event" to track catch-up for.** Every source's arrivals are pooled, and
#' each distinct `(id, value)` pair's event `start` is the *earliest* year *any* source shows
#' that identifier arriving at that value. This means a value an identifier returns to a second
#' time, years after an earlier departure and return, is **not** treated as a second, distinct
#' event -- it collapses into the same `(id, value)` event as the first occurrence anywhere
#' that value was ever seen for that identifier. For this package's ordinary use (tracking
#' arrival at a value, not every individual spell), this is the right simplification; it is not
#' silent -- a value visited more than once by the same identifier is a known, accepted
#' limitation of this function, not an oversight.
#'
#' **Censoring uses each source's OWN observation window, not the panel's.** At each
#' `horizon` (years since an event's `start`), a source's risk set excludes any event the
#' source could not possibly have observed yet -- entry into the risk set is delayed
#' (left-truncated) until that source's own earliest observed year, and the source is
#' censored (right-censored, not counted as "never shown") once its own latest observed
#' year has passed, regardless of whether some *other* source's data extends further. Using
#' the panel-wide latest year for every source conflates two different things: a source that
#' was watched and simply had not shown the value yet (real loss to follow-up) versus a
#' source whose own data collection had already ended, or had not yet started, when the
#' comparison window extends past it (structural non-coverage). The latter is not evidence
#' the source is slow -- it is evidence the source cannot be assessed over years it was
#' never observed in at all. A source whose own data entirely predates or postdates an
#' event's observable window (no overlap at all) is never at risk for that event and simply
#' does not contribute to its curve.
#'
#' **Person-level non-coverage is a third category, distinct from both of the above.** A
#' source's own observation window tells you when the *source* was operating; it says
#' nothing about whether a *given identifier* ever appears in it at all. An identifier this
#' source has zero records for, in any year, cannot show any value for it, let alone the
#' tracked one -- this is not "watched and still pending", it is structural absence, and on
#' real data it is often the dominant effect: in a real application, 46% of tracked
#' identifiers never appeared in a sparse source at all, for any year, which was a far
#' larger driver of that source's slow-looking curve than its temporal window ever was.
#' Such identifiers are excluded from a source's risk set entirely (not merely left
#' unresolved) and counted in the returned `n_no_coverage`, the same way `n_unanchored`
#' counts events dropped for lacking independent confirmation -- both are reasons an event
#' cannot inform a source's curve at all, as opposed to informing it with a slow or fast
#' result.
#'
#' **Circularity in which source "defines" an event, and the `reference` parameter.** By
#' default (`reference = "pooled"`), an event's `start` is pooled across every source's own
#' detected arrivals -- which means the source that happens to detect an event earliest gets
#' credited with "catching up" to it immediately (`horizon = 0`), simply because it supplied
#' the reference time, not because it is actually fast. In practice this is severe: in a
#' real five-source comparison, 66-85% of the events a source resolved at `horizon = 0` were
#' ones that source itself had (partly) defined. `reference = "exclude_self"` removes this:
#' for each source, the event set it is scored against is built from *every other* source's
#' detected arrivals, excluding its own. An event detected only by the source being scored
#' (no other source independently confirms it) cannot be scored at all under this mode and
#' is dropped, counted in the returned `n_unanchored`. This does not create ground truth --
#' an event two sources both happen to miss is still invisible to both -- but it removes the
#' most direct form of circularity, and comparing `reference = "pooled"` against
#' `reference = "exclude_self"` on the same data is itself a sensitivity check worth running
#' before treating any single source's apparent speed as real.
#'
#' **A reported `median_horizon` can still be a thin, easily-shifted estimate even though
#' it was computed correctly.** The Kaplan-Meier risk set shrinks at later horizons (fewer
#' events have that much follow-up left at all), so a source whose curve approaches 50% only
#' gradually can sit within a point or two of the threshold for several consecutive horizons
#' on a small and shrinking sample, where a single additional event tips the point estimate
#' by multiple years. This is not a bug -- re-running the same data gives the same number --
#' but treating that number as precise would be a mistake a reader has no way to catch
#' without more information. `median_ci_low`/`median_ci_high` and `median_n_at_risk` exist
#' specifically to surface this: a wide interval or a small `median_n_at_risk` at the
#' crossing horizon both say the same thing -- the point estimate is real but imprecise --
#' and should be reported alongside it, not silently dropped in favor of the single number.
#'
#' @param sources Named list of longitudinal data frames (2+), each with `id`, `year`, and
#'   `attribute` columns -- same convention as [attribute_coverage()]/
#'   [attribute_source_corroboration()].
#' @param attribute Name of the attribute column to track (e.g. `"state"`).
#' @param id,year Column names. Defaults `"npi"`, `"year"`.
#' @param max_horizon Maximum years since an event's `start` to evaluate. If `NULL` (default),
#'   the full span of years present across all sources (`max(year) - min(year)`).
#' @param reference `"pooled"` (default) pools every source's own detected arrivals to find
#'   each event's `start`, so a source can get credit for "catching up" to an event it itself
#'   defined. `"exclude_self"` scores each source only against events *other* sources
#'   detected, removing that circularity at the cost of dropping events no other source
#'   independently confirms (see `n_unanchored`).
#' @param lineage_groups Only used when `reference = "exclude_self"`. Optional named list,
#'   same convention as [attribute_claims_collapse_lineage()]'s own `lineage_groups`: each
#'   element a character vector of `sources` names that share underlying provenance (e.g.
#'   a facility-affiliation extract derived from the same upstream file as an enrollment
#'   source). A source not named by any group is its own singleton group. Excluding "self"
#'   then means excluding every member of the scored source's *whole group*, not just that
#'   one source -- two sources that are really the same evidence re-exported under
#'   different names cannot independently confirm each other's events, and treating them
#'   as independent just because their files have different names reintroduces the
#'   circularity `reference = "exclude_self"` exists to remove. Each source name may belong
#'   to at most one group.
#' @param ci_level Confidence level for the pointwise survival-curve interval (`curve$ci_low`/
#'   `ci_high`) and the Brookmeyer-Crowley interval around `median_horizon`. Default `0.95`.
#'   Computed via the log-log (Kalbfleisch-Prentice) transform of Greenwood's formula, the
#'   same method `survival::survfit()` uses by default.
#' @return A list:
#'   \describe{
#'     \item{`curve`}{Tibble, one row per `(source, horizon)`: `n_at_risk`, `n_events` (first
#'       shown at exactly this horizon), `pct_not_yet_shown` (the Kaplan-Meier survival
#'       estimate, non-increasing within a source; may start below `100` if that source's own
#'       observation window begins after some events' `start`, since those events have not
#'       yet entered its risk set), `ci_low`/`ci_high` (the pointwise confidence interval on
#'       `pct_not_yet_shown` at `ci_level`, `NA` only when there is no data at all to estimate
#'       from).}
#'     \item{`median_horizon`}{Tibble, one row per source: `median_horizon`, the smallest
#'       `horizon` at which `pct_not_yet_shown <= 50`, or `NA` if the curve never crosses 50%
#'       within `max_horizon` -- a source that never reaches its median catch-up time within
#'       the observed panel, reported as `NA`, not a misleadingly small number. `median_ci_low`/
#'       `median_ci_high`, the Brookmeyer-Crowley confidence interval around that horizon
#'       (`median_ci_high` is `NA` when the upper confidence bound on the curve never crosses
#'       50% within the observed horizons -- an unbounded slow end, not a fabricated one).
#'       `median_n_at_risk`, the number of events still at risk at the horizon where the
#'       point estimate crosses 50% -- a small value (relative to `n_tracked`) is a direct
#'       warning sign that the point estimate itself is sitting on a thin, easily-shifted
#'       sample, which the width of `median_ci_low`/`median_ci_high` will also reflect.}
#'     \item{`n_tracked`}{Total distinct `(id, value)` arrival events pooled across all
#'       sources -- a single constant, independent of any source's own observation window.}
#'     \item{`reference`}{The `reference` argument used, echoed back.}
#'     \item{`n_no_coverage`}{Tibble, one row per source: the count of pooled events dropped
#'       for that source because the identifier has no record in that source at all, in any
#'       year -- structural non-coverage, excluded from the risk set entirely rather than
#'       left unresolved.}
#'     \item{`n_unanchored`}{Only present when `reference = "exclude_self"`: tibble, one row
#'       per source, the count of pooled events dropped for that source because no source
#'       outside its `lineage_groups` group (itself alone, if ungrouped) independently
#'       detected them.}
#'   }
#' @examples
#' a <- data.frame(
#'   npi = c("1", "1", "1", "2", "2", "2"), year = rep(2020:2022, 2),
#'   state = c("CO", "CO", "NM", "TX", "TX", "TX")
#' )
#' b <- data.frame(
#'   npi = c("1", "1", "1", "2", "2", "2"), year = rep(2020:2022, 2),
#'   state = c("CO", "NM", "NM", "TX", "TX", "NM")
#' )
#' cu <- attribute_catch_up_curve(list(a = a, b = b), attribute = "state")
#' cu$curve
#' cu$median_horizon
#' # Rank sources by how fast they typically catch up (lower is faster):
#' format_ranking_prose(
#'   group = cu$median_horizon$source, value = cu$median_horizon$median_horizon,
#'   value_label = "median years to catch up", fmt = "%.0f", decreasing = FALSE
#' )$sentence
#' @seealso [attribute_lead_lag_duration()] for the simpler, non-censoring-aware version this
#'   complements; [format_ranking_prose()] to rank `median_horizon` across sources.
#' @family no-gold-standard comparison
#' @export
attribute_catch_up_curve <- function(sources, attribute, id = "npi", year = "year", max_horizon = NULL,
                                      reference = c("pooled", "exclude_self"), lineage_groups = NULL,
                                      ci_level = 0.95) {
  reference <- match.arg(reference)
  checkmate::assert_list(sources, min.len = 2, names = "unique")
  checkmate::assert_string(attribute, min.chars = 1)
  checkmate::assert_string(id, min.chars = 1)
  checkmate::assert_string(year, min.chars = 1)
  if (!is.null(max_horizon)) checkmate::assert_count(max_horizon, positive = TRUE)
  checkmate::assert_number(ci_level, lower = 0, upper = 1)

  nm <- names(sources)

  if (!is.null(lineage_groups)) {
    checkmate::assert_list(lineage_groups, names = "unique")
    group_members <- unlist(lineage_groups, use.names = FALSE)
    if (anyDuplicated(group_members) > 0L) {
      stop(sprintf("attribute_catch_up_curve: source(s) appear in more than one lineage group: %s",
                   paste(unique(group_members[duplicated(group_members)]), collapse = ", ")), call. = FALSE)
    }
    unknown <- setdiff(group_members, nm)
    if (length(unknown) > 0L) {
      stop(sprintf("attribute_catch_up_curve: lineage_groups references unknown source(s): %s",
                   paste(unknown, collapse = ", ")), call. = FALSE)
    }
  }
  group_of <- function(s) {
    if (is.null(lineage_groups)) return(s)
    hit <- Filter(function(g) s %in% g, lineage_groups)
    if (length(hit) == 0L) s else hit[[1]]
  }
  clean_source <- function(x, label) {
    checkmate::assert_data_frame(x)
    for (col in c(id, year, attribute)) {
      if (!col %in% names(x)) {
        stop(sprintf("attribute_catch_up_curve: source '%s' has no column '%s'", label, col), call. = FALSE)
      }
    }
    res <- tibble::tibble(
      id = trimws(as.character(x[[id]])),
      year = .parse_whole_integer(x[[year]], sprintf("attribute_catch_up_curve: source '%s' year", label), allow_na = FALSE)
    )
    v <- toupper(trimws(as.character(x[[attribute]])))
    res$value <- ifelse(is.na(v) | !nzchar(v), NA_character_, v)
    res <- res[!is.na(res$id) & nzchar(res$id) & !is.na(res$value), , drop = FALSE]
    .assert_unique_keys(res, c("id", "year"), sprintf("attribute_catch_up_curve: source '%s'", label))
    res[order(res$id, res$year), , drop = FALSE]
  }
  cleaned <- lapply(nm, function(s) clean_source(sources[[s]], s))
  names(cleaned) <- nm

  # Each source's own arrivals: consecutive-year transitions where the value changed.
  extract_events <- function(df) {
    groups <- split(df, df$id, drop = TRUE)
    rows <- lapply(groups, function(g) {
      n <- nrow(g)
      if (n < 2L) return(NULL)
      prev_year <- g$year[1:(n - 1L)]; prev_val <- g$value[1:(n - 1L)]
      this_year <- g$year[2:n]; this_val <- g$value[2:n]
      consec <- this_year == prev_year + 1L
      changed <- consec & (this_val != prev_val)
      if (!any(changed)) return(NULL)
      tibble::tibble(id = g$id[1], year = this_year[changed], value = this_val[changed])
    })
    dplyr::bind_rows(rows)
  }
  events <- lapply(cleaned, extract_events) # events[[s]] is NULL if source s detects nothing
  events_present <- events[!vapply(events, is.null, logical(1))]
  all_events <- dplyr::bind_rows(events_present)
  if (nrow(all_events) == 0L) {
    stop("attribute_catch_up_curve: no consecutive-year arrivals detected in any source", call. = FALSE)
  }

  first_seen_pooled <- stats::aggregate(year ~ id + value, all_events, min)
  names(first_seen_pooled)[3] <- "start"
  n_tracked <- nrow(first_seen_pooled)

  source_min_year <- vapply(cleaned, function(x) min(x$year), integer(1))
  source_max_year <- vapply(cleaned, function(x) max(x$year), integer(1))
  panel_min_year <- min(source_min_year)
  panel_max_year <- max(source_max_year)
  if (is.null(max_horizon)) max_horizon <- panel_max_year - panel_min_year
  horizons <- 0:max_horizon

  # A source is credited with "showing" a tracked value as soon as ANY of its own
  # records carries that exact value in a year at or after the value's pooled first
  # appearance -- not only if that source's own immediately preceding record was
  # exactly one year earlier. The latter (stricter) rule is used above, in
  # extract_events(), to discover genuine arrivals robustly; it is deliberately NOT
  # reused here, because it silently undercounts catch-up for any source that is
  # sampled intermittently (e.g. claims- or enrollment-derived data with multi-year
  # gaps between records) relative to a densely/annually sampled source -- such a
  # source can show the correct value while its own prior record is several years
  # earlier, which extract_events() would never flag as an "arrival".
  km_for_source <- function(s) {
    src <- cleaned[[s]]
    n_unanchored <- 0L
    if (identical(reference, "exclude_self")) {
      other_events <- events[setdiff(nm, group_of(s))]
      other_events <- other_events[!vapply(other_events, is.null, logical(1))]
      all_other <- dplyr::bind_rows(other_events)
      if (nrow(all_other) == 0L) {
        first_seen_s <- first_seen_pooled[0L, ]
      } else {
        first_seen_s <- stats::aggregate(year ~ id + value, all_other, min)
        names(first_seen_s)[3] <- "start"
      }
      n_unanchored <- n_tracked - nrow(first_seen_s)
    } else {
      first_seen_s <- first_seen_pooled
    }

    # Person-level non-coverage: an identifier this source has NO records for at all, in
    # any year, cannot show ANY value for it, let alone the tracked one -- structurally
    # absent from this source, not "watched but not yet updated". Keeping such an
    # identifier in the risk set anyway (as the source-level observation window alone
    # would) drags the curve down for a reason that has nothing to do with update speed:
    # on real data, this was the dominant driver of an implausibly slow-looking source,
    # far more than the source-level window ever was. Excluded here and reported
    # separately, the same way n_unanchored is, rather than silently counted as still
    # pending.
    ids_observed <- unique(src$id)
    n_no_coverage <- sum(!(first_seen_s$id %in% ids_observed))
    first_seen_s <- first_seen_s[first_seen_s$id %in% ids_observed, , drop = FALSE]

    if (nrow(first_seen_s) == 0L) {
      return(list(
        curve = tibble::tibble(horizon = horizons, n_at_risk = 0L, n_events = 0L, pct_not_yet_shown = NA_real_,
                               ci_low = NA_real_, ci_high = NA_real_),
        n_unanchored = n_unanchored, n_no_coverage = n_no_coverage
      ))
    }

    m <- merge(first_seen_s, src, by = c("id", "value"), all.x = TRUE)
    m$d <- m$year - m$start
    # A match that occurred only *before* this value's first appearance anywhere
    # (a coincidental earlier occurrence of the same value, unrelated to the
    # tracked arrival) does not count as catching up; treat it as no match so the
    # (id, value, start) triple is retained in the risk set rather than vanishing.
    m$d[!is.na(m$d) & m$d < 0] <- NA
    m <- m[order(is.na(m$d), m$d), ]
    m <- m[!duplicated(m[, c("id", "value", "start")]), , drop = FALSE]

    # This source's OWN observation window, not the panel's: it cannot be at risk of
    # showing a value before its own data begins (entry is delayed/left-truncated, not
    # retroactive), and it is censored at its own last observed year, regardless of how
    # much further some other source's data happens to extend.
    entry_h <- pmax(0, source_min_year[[s]] - m$start)
    exit_h <- source_max_year[[s]] - m$start

    n_at_risk_vec <- integer(length(horizons))
    n_events_vec <- integer(length(horizons))
    surv_vec <- numeric(length(horizons))
    greenwood_vec <- numeric(length(horizons))
    surv <- 1
    cum_greenwood <- 0
    for (i in seq_along(horizons)) {
      h <- horizons[i]
      at_risk <- (is.na(m$d) | m$d >= h) & entry_h <= h & exit_h >= h
      n_at_risk <- sum(at_risk)
      n_events <- sum(!is.na(m$d) & m$d == h & at_risk)
      n_at_risk_vec[i] <- n_at_risk
      n_events_vec[i] <- n_events
      if (n_at_risk > 0L) surv <- surv * (1 - n_events / n_at_risk)
      surv_vec[i] <- 100 * surv
      # Greenwood's formula, accumulated: Var(S(h)) = S(h)^2 * cum_greenwood. Kept on this
      # variance-ratio scale (rather than multiplying through by S(h)^2 immediately) because
      # the log-log-transformed confidence interval below needs exactly this ratio, not the
      # raw variance.
      if (n_at_risk > 0L && n_at_risk > n_events) {
        cum_greenwood <- cum_greenwood + n_events / (n_at_risk * (n_at_risk - n_events))
      }
      greenwood_vec[i] <- cum_greenwood
    }

    # Pointwise confidence interval for the survival curve itself, via the log-log
    # (Kalbfleisch-Prentice) transform of Greenwood's variance -- the same method
    # survival::survfit() uses by default, chosen because it stays within [0, 100] rather
    # than needing to be clipped, unlike a plain S(h) +/- z*SE(S(h)) interval. A horizon
    # with no uncertainty yet (0% or 100%, no events have had the chance to occur or all of
    # them already have) gets a degenerate, exact interval rather than an undefined one.
    z <- stats::qnorm(1 - (1 - ci_level) / 2)
    s_frac <- surv_vec / 100
    valid <- s_frac > 0 & s_frac < 1 & greenwood_vec > 0
    theta <- ifelse(valid, log(-log(s_frac)), NA_real_)
    se_theta <- ifelse(valid, sqrt(greenwood_vec) / abs(log(s_frac)), NA_real_)
    ci_low_vec <- rep(NA_real_, length(horizons))
    ci_high_vec <- rep(NA_real_, length(horizons))
    ci_low_vec[valid] <- 100 * exp(-exp(theta[valid] + z * se_theta[valid]))
    ci_high_vec[valid] <- 100 * exp(-exp(theta[valid] - z * se_theta[valid]))
    ci_low_vec[s_frac == 1] <- 100; ci_high_vec[s_frac == 1] <- 100
    ci_low_vec[s_frac == 0] <- 0; ci_high_vec[s_frac == 0] <- 0

    list(
      curve = tibble::tibble(horizon = horizons, n_at_risk = n_at_risk_vec, n_events = n_events_vec,
                             pct_not_yet_shown = round(surv_vec, 1),
                             ci_low = round(ci_low_vec, 1), ci_high = round(ci_high_vec, 1)),
      n_unanchored = n_unanchored, n_no_coverage = n_no_coverage
    )
  }

  per_source <- lapply(nm, km_for_source)
  names(per_source) <- nm

  curve_rows <- lapply(nm, function(s) {
    out <- per_source[[s]]$curve
    out$source <- s
    out[, c("source", "horizon", "n_at_risk", "n_events", "pct_not_yet_shown", "ci_low", "ci_high")]
  })
  curve <- tibble::as_tibble(dplyr::bind_rows(curve_rows))

  # Brookmeyer-Crowley confidence interval for the median: the horizons at which the
  # pointwise lower and upper confidence bounds on the survival curve itself (computed
  # above) first cross 50%. The lower survival bound represents the most-already-resolved
  # plausible curve, so it crosses first (the fast end of the interval); the upper bound is
  # the least-resolved plausible curve, crossing last or never (the slow end) -- `NA` when
  # it never crosses within the observed horizons, not a fabricated upper limit.
  median_rows <- lapply(nm, function(s) {
    cv <- curve[curve$source == s, ]
    below <- cv$horizon[!is.na(cv$pct_not_yet_shown) & cv$pct_not_yet_shown <= 50]
    median_h <- if (length(below) > 0L) min(below) else NA_integer_
    lo_below <- cv$horizon[!is.na(cv$ci_low) & cv$ci_low <= 50]
    hi_below <- cv$horizon[!is.na(cv$ci_high) & cv$ci_high <= 50]
    tibble::tibble(
      source = s, median_horizon = median_h,
      median_ci_low = if (!is.na(median_h) && length(lo_below) > 0L) min(lo_below) else NA_integer_,
      median_ci_high = if (!is.na(median_h) && length(hi_below) > 0L) min(hi_below) else NA_integer_,
      median_n_at_risk = if (!is.na(median_h)) cv$n_at_risk[cv$horizon == median_h] else NA_integer_
    )
  })
  median_horizon <- tibble::as_tibble(dplyr::bind_rows(median_rows))

  out <- list(curve = curve, median_horizon = median_horizon, n_tracked = n_tracked, reference = reference)
  out$n_no_coverage <- tibble::tibble(
    source = nm,
    n_no_coverage = unname(vapply(nm, function(s) per_source[[s]]$n_no_coverage, integer(1)))
  )
  if (identical(reference, "exclude_self")) {
    out$n_unanchored <- tibble::tibble(
      source = nm,
      n_unanchored = unname(vapply(nm, function(s) per_source[[s]]$n_unanchored, integer(1)))
    )
  }
  structure(out, class = "mysteryroster_catch_up_curve")
}

#' @export
print.mysteryroster_catch_up_curve <- function(x, ...) {
  cat(sprintf(
    "<mysteryroster_catch_up_curve>\n  reference: %s | %s tracked event%s across %d source%s\n",
    x$reference, format(x$n_tracked, big.mark = ","), if (x$n_tracked == 1L) "" else "s",
    nrow(x$median_horizon), if (nrow(x$median_horizon) == 1L) "" else "s"
  ))
  mh <- x$median_horizon
  resolved <- mh[!is.na(mh$median_horizon), , drop = FALSE]
  resolved <- resolved[order(resolved$median_horizon), , drop = FALSE]
  if (nrow(resolved) > 0L) {
    cat("  median years to catch up (95% CI), fastest first:\n")
    for (i in seq_len(nrow(resolved))) {
      r <- resolved[i, ]
      hi <- if (is.na(r$median_ci_high)) "not estimable" else r$median_ci_high
      cat(sprintf("    %-20s %s (%s-%s)  [n at risk at crossing = %s]\n",
                  r$source, r$median_horizon, r$median_ci_low, hi, r$median_n_at_risk))
    }
  }
  unresolved <- setdiff(mh$source, resolved$source)
  if (length(unresolved) > 0L) {
    cat(sprintf("  never reached 50%% catch-up: %s\n", paste(unresolved, collapse = ", ")))
  }
  invisible(x)
}

#' Manuscript-ready prose for an attribute_catch_up_curve() result
#'
#' A thin wrapper around [format_ranking_prose()] specific to
#' [attribute_catch_up_curve()]'s own output shape: ranks sources whose catch-up curve
#' crosses 50% within the observed panel, and -- because [format_ranking_prose()]
#' deliberately refuses `NA` values -- writes its own sentence for any source whose curve
#' never does, naming how much of the pooled event set still has not caught up, both
#' immediately and at a chosen horizon.
#'
#' @details
#' A source's curve crossing 50% ("resolved") and never doing so ("unresolved") are
#' reported in two different registers for a reason: a median catch-up time is only a
#' meaningful summary for a source that actually reaches one within the panel. Forcing
#' every source through the same ranking sentence would either drop the unresolved ones
#' silently (the same failure mode [attribute_lead_lag_duration()]'s median/IQR has, which
#' [attribute_catch_up_curve()] exists to avoid) or report a fabricated "greater than X
#' years" figure with no real upper bound. Resolved sources are ranked by
#' [format_ranking_prose()]; unresolved ones each get a sentence giving
#' `pct_not_yet_shown` at horizon 0 and at `unresolved_horizon` (clamped down to a source's
#' own maximum observed horizon if that is shorter), so a reader sees exactly how much
#' ground an unresolved source has or hasn't covered, not just that it failed to reach the
#' 50% mark.
#'
#' Fewer than two resolved sources cannot be ranked ([format_ranking_prose()] requires
#' 2+); with exactly one, its own median is reported as a plain sentence instead, and with
#' zero, the ranking sentence is omitted entirely -- `sentence` is always well-formed,
#' never an error, regardless of how many sources resolve.
#'
#' @param cu Output of [attribute_catch_up_curve()]: a list with `curve` and
#'   `median_horizon`.
#' @param display_label Optional named character vector mapping a source name (as it
#'   appears in `cu`) to the label used in prose (e.g. `c(nppes = "NPPES", dac_pecos =
#'   "the DAC/PECOS-combined enrollment record")`). A source with no entry is shown under
#'   its own name.
#' @param unresolved_horizon Horizon (years) at which to report the catch-up percentage
#'   for a source whose curve never crosses 50%. Default `5`. Clamped down to a source's
#'   own maximum observed horizon if that is shorter.
#' @param decreasing Passed to [format_ranking_prose()] for the resolved-source ranking.
#'   Default `FALSE` (fastest catch-up first), the natural reading for a "years to catch
#'   up" value -- the opposite of that function's own default.
#' @param register `"technical"` (default) produces this function's original terse
#'   sentences ("reached 50% catch-up", "tracked arrivals"). `"clinical"` instead produces
#'   plain-language prose with no abbreviations and no em dashes, suitable for a clinical-
#'   journal Methods/Results section: "confirmed a change already reported by another
#'   source" rather than "caught up", "changes" rather than "arrivals". As with
#'   [format_ranking_prose()], the caller is still responsible for passing fully spelled-
#'   out `display_label` values in clinical register; this parameter controls sentence
#'   structure, not domain vocabulary.
#' @param ci_level Display label only (e.g. "95% CI"); must match the `ci_level` actually
#'   passed to the [attribute_catch_up_curve()] call that produced `cu`, which this
#'   function has no way to verify and trusts the caller on. Default `0.95`, matching that
#'   function's own default.
#' @return A list:
#'   \describe{
#'     \item{`n_events`}{The total number of tracked events pooled across all sources
#'       (`cu$n_tracked` when present).}
#'     \item{`ranking`}{[format_ranking_prose()]'s full return for the resolved sources,
#'       or `NULL` if fewer than 2 sources resolved.}
#'     \item{`unresolved`}{Tibble, one row per unresolved source: `source`, `horizon`,
#'       `pct_at_horizon_0`, `pct_at_horizon`.}
#'     \item{`sentence`}{The complete, ready-to-paste Results text -- including, for each
#'       resolved source, a confidence interval and the number of events still at risk
#'       when its median was reached, whenever `cu` carries those columns (every real
#'       [attribute_catch_up_curve()] result does).}
#'   }
#' @examples
#' a <- data.frame(
#'   npi = c("1", "1", "1", "2", "2", "2"), year = rep(2020:2022, 2),
#'   state = c("CO", "CO", "NM", "TX", "TX", "TX")
#' )
#' b <- data.frame(
#'   npi = c("1", "1", "1", "2", "2", "2"), year = rep(2020:2022, 2),
#'   state = c("CO", "NM", "NM", "TX", "GA", "GA")
#' )
#' cu <- attribute_catch_up_curve(list(a = a, b = b), attribute = "state")
#' attribute_catch_up_curve_prose(cu, display_label = c(a = "Source A", b = "Source B"))$sentence
#'
#' attribute_catch_up_curve_prose(
#'   cu, display_label = c(a = "the first registry", b = "the second registry"),
#'   register = "clinical"
#' )$sentence
#' @seealso [attribute_catch_up_curve()]; [format_ranking_prose()], used internally for the
#'   resolved-source ranking.
#' @family no-gold-standard comparison
#' @export
attribute_catch_up_curve_prose <- function(cu, display_label = NULL, unresolved_horizon = 5L,
                                            decreasing = FALSE, register = c("technical", "clinical"),
                                            ci_level = 0.95) {
  register <- match.arg(register)
  checkmate::assert_number(ci_level, lower = 0, upper = 1)
  checkmate::assert_list(cu, names = "unique")
  if (!all(c("curve", "median_horizon") %in% names(cu))) {
    stop("attribute_catch_up_curve_prose: cu must have 'curve' and 'median_horizon' elements, as returned by attribute_catch_up_curve()", call. = FALSE)
  }
  curve <- cu$curve
  median_horizon <- cu$median_horizon
  checkmate::assert_data_frame(curve, min.rows = 1)
  checkmate::assert_data_frame(median_horizon, min.rows = 1)
  for (col in c("source", "horizon", "n_at_risk", "pct_not_yet_shown")) {
    if (!col %in% names(curve)) {
      stop(sprintf("attribute_catch_up_curve_prose: cu$curve has no column '%s'", col), call. = FALSE)
    }
  }
  for (col in c("source", "median_horizon")) {
    if (!col %in% names(median_horizon)) {
      stop(sprintf("attribute_catch_up_curve_prose: cu$median_horizon has no column '%s'", col), call. = FALSE)
    }
  }
  if (!is.null(display_label)) checkmate::assert_character(display_label, names = "unique")
  checkmate::assert_count(unresolved_horizon, positive = TRUE)
  checkmate::assert_flag(decreasing)

  label_for <- function(s) if (!is.null(display_label) && s %in% names(display_label)) display_label[[s]] else s

  nm <- unique(as.character(median_horizon$source))
  # cu$n_tracked (present on every real attribute_catch_up_curve() result) is the true,
  # reference-mode-independent pooled total. Falling back to the largest single source's
  # own horizon-0 risk-set size only happens to equal that total under reference = "pooled",
  # where every source starts with an identical, unfiltered risk set; under "exclude_self",
  # each source's own risk set is already reduced by its unanchored/uncovered exclusions, so
  # the two are not interchangeable, and using the fallback there would understate the real
  # number of tracked events.
  n_events <- if (!is.null(cu$n_tracked)) {
    cu$n_tracked
  } else {
    max(curve$n_at_risk[curve$horizon == 0], na.rm = TRUE)
  }

  resolved <- median_horizon[!is.na(median_horizon$median_horizon), , drop = FALSE]
  unresolved_nm <- setdiff(nm, resolved$source)

  clinical <- identical(register, "clinical")
  value_label <- if (clinical) "median years to confirmation" else "median years to catch up"

  ranking <- NULL
  ranking_sentence <- NULL
  if (nrow(resolved) >= 2L) {
    ranking <- format_ranking_prose(
      group = vapply(resolved$source, label_for, character(1)), value = resolved$median_horizon,
      value_label = value_label, fmt = "%.0f", decreasing = decreasing, register = register
    )
    ranking_sentence <- ranking$sentence
  } else if (nrow(resolved) == 1L) {
    ranking_sentence <- if (clinical) {
      sprintf(
        "The estimated time for %s to confirm a change already reported by another source was %.0f years.",
        label_for(resolved$source[1]), resolved$median_horizon[1]
      )
    } else {
      sprintf(
        "%s reached 50%% catch-up after a median of %.0f years.",
        label_for(resolved$source[1]), resolved$median_horizon[1]
      )
    }
  }

  # A median_horizon's point estimate can be real but imprecise, sitting on a thin,
  # already-censoring-reduced risk set for several consecutive horizons before actually
  # crossing 50% (see attribute_catch_up_curve()'s own Details). Reporting it bare, with
  # no interval, hides that from a reader who has no way to tell a well-powered median
  # from a fragile one. This is only possible when cu was produced by a version of
  # attribute_catch_up_curve() that returns the confidence-interval columns; older or
  # hand-built cu objects without them simply skip this (no error).
  has_ci <- all(c("median_ci_low", "median_ci_high", "median_n_at_risk") %in% names(median_horizon))
  ci_sentences <- character(0)
  if (has_ci && nrow(resolved) > 0L) {
    ci_pct <- round(100 * ci_level)
    # Builds the "X to Y years" clause with correct grammar in all four combinations of
    # which bound, if either, is NA (not estimable within the observed horizons) -- a
    # naive sprintf("%s to %s years", lo, hi) reads badly ("...not estimable ... years")
    # whenever either bound is text rather than a number.
    yr <- function(n) if (n == 1L) "year" else "years"
    format_ci_years <- function(lo, hi) {
      if (!is.na(lo) && !is.na(hi)) {
        sprintf("%d to %d %s", lo, hi, yr(hi))
      } else if (!is.na(lo)) {
        if (clinical) sprintf("%d %s, with no upper bound estimable within the period observed", lo, yr(lo))
        else sprintf("%d %s to not estimable", lo, yr(lo))
      } else if (!is.na(hi)) {
        if (clinical) sprintf("not estimable at the lower end, up to %d %s", hi, yr(hi))
        else sprintf("not estimable to %d %s", hi, yr(hi))
      } else {
        "not estimable in either direction within the period observed"
      }
    }
    ci_sentences <- vapply(seq_len(nrow(resolved)), function(i) {
      r <- resolved[i, ]
      ci_text <- format_ci_years(r$median_ci_low, r$median_ci_high)
      if (clinical) {
        sprintf(
          "This estimate for %s was based on %d event%s still being tracked at the time the median was reached, with a %d%% confidence interval of %s.",
          label_for(r$source), r$median_n_at_risk, if (r$median_n_at_risk == 1L) "" else "s", ci_pct, ci_text
        )
      } else {
        sprintf(
          "%s: %d%% CI %s (n at risk at crossing = %d).",
          label_for(r$source), ci_pct, ci_text, r$median_n_at_risk
        )
      }
    }, character(1))
  }

  unresolved_rows <- lapply(unresolved_nm, function(s) {
    cv <- curve[curve$source == s, , drop = FALSE]
    h <- min(unresolved_horizon, max(cv$horizon))
    tibble::tibble(
      source = s, horizon = h,
      pct_at_horizon_0 = cv$pct_not_yet_shown[cv$horizon == 0],
      pct_at_horizon = cv$pct_not_yet_shown[cv$horizon == h]
    )
  })
  unresolved <- if (length(unresolved_rows) > 0L) {
    tibble::as_tibble(dplyr::bind_rows(unresolved_rows))
  } else {
    tibble::tibble(source = character(0), horizon = integer(0),
                   pct_at_horizon_0 = numeric(0), pct_at_horizon = numeric(0))
  }

  unresolved_sentences <- vapply(seq_len(nrow(unresolved)), function(i) {
    r <- unresolved[i, ]
    if (clinical) {
      sprintf(
        "%s did not reach confirmation for even half of the changes tracked within the period observed. At the time a change first appeared, %.1f%% had not yet been confirmed by this source; %.1f%% remained unconfirmed %d year%s later.",
        label_for(r$source), r$pct_at_horizon_0, r$pct_at_horizon, r$horizon, if (r$horizon == 1L) "" else "s"
      )
    } else {
      sprintf(
        "%s never reached 50%% catch-up within the observed panel (%.1f%% of tracked arrivals still not reflected immediately, %.1f%% still not reflected %d year%s later).",
        label_for(r$source), r$pct_at_horizon_0, r$pct_at_horizon, r$horizon, if (r$horizon == 1L) "" else "s"
      )
    }
  }, character(1))

  intro_sentence <- if (clinical) {
    sprintf("A total of %s change%s in the tracked value %s identified across %d source%s.",
            format(n_events, big.mark = ","), if (n_events == 1L) "" else "s",
            if (n_events == 1L) "was" else "were",
            length(nm), if (length(nm) == 1L) "" else "s")
  } else {
    sprintf("We identified %s state-arrival event%s tracked across %d source%s.",
            format(n_events, big.mark = ","), if (n_events == 1L) "" else "s",
            length(nm), if (length(nm) == 1L) "" else "s")
  }

  sentence <- paste(
    c(intro_sentence, ranking_sentence, ci_sentences, unresolved_sentences),
    collapse = " "
  )

  list(n_events = n_events, ranking = ranking, unresolved = unresolved, sentence = sentence)
}

#' Approach 2: Attribute agreement stratified by empirical spell age or career tenure
#'
#' Tests whether cross-registry disagreement is driven by accumulated staleness
#' in a longitudinal registry: if agreement peaks when `source_b`'s attribute
#' was updated 1-2 years ago and decays steadily as uninterrupted spell age
#' reaches 11+ years, disagreement is driven by un-updated records in `source_b`.
#' Conversely, if a comparison source shows no decay with `source_b`'s spell age,
#' that source is likely echoing `source_b` rather than observing the attribute
#' independently.
#'
#' @details
#' Rows join on `(id, year)` when `source_a` has a `year` column, otherwise on
#' `id` against `panel_b`'s `target_year` rows. Matched rows with a missing
#' `age_col` are dropped. Ages are binned as `0`, `1-2`, `3-5`, `6-10`, and
#' `11+` years; a bin with no matched rows is omitted from the output rather
#' than reported with `n = 0`. Each `<attr>_agree_pct` is computed over the
#' rows in that stratum where both sources have a non-missing value, so its
#' denominator can be smaller than `n`. Values are trimmed and upper-cased.
#'
#' `age_col` can be any non-negative integer clock, such as years since
#' board certification; the bins are the same.
#'
#' @param source_a Data frame for comparison Source A (either cross-sectional or
#'   multi-year), with `id`, `attributes`, and (if multi-year) `year`.
#' @param panel_b Data frame for longitudinal Source B annotated with
#'   `spell_age_years` (see [attribute_spell_age()]), with `id`, `year`,
#'   `spell_age_years`, and `attributes`.
#' @param attributes Character vector of unique attribute column names to compare.
#' @param id Name of the identifier column. Default `"npi"`.
#' @param year Name of the year column in `panel_b` (and `source_a` if present).
#'   Default `"year"`.
#' @param target_year If `source_a` is a single cross-section without a `year`
#'   column, which year of `panel_b` to match against. Default `max(panel_b[[year]])`.
#' @param comparison_label Label stored in the `comparison` output column.
#' @param age_col Column in `panel_b` containing integer years of spell age or
#'   tenure. Default `"spell_age_years"`.
#' @param stratification_label Label stored in `stratification`. Default
#'   `"spell_age_years"`.
#' @return A tibble with one row per non-empty age stratum, in age order:
#'   `comparison`, `stratification`, `stratum` (one of
#'   `"0 yrs (updated in snapshot yr)"`, `"1-2 yrs"`, `"3-5 yrs"`,
#'   `"6-10 yrs"`, `"11+ yrs (un-updated >=11 yrs)"`), `n` (matched rows),
#'   and `<attr>_agree_pct` (percent, one decimal) for each attribute.
#' @examples
#' panel <- data.frame(npi = c("1", "1", "2", "2", "2"),
#'                     year = c(2023, 2024, 2022, 2023, 2024),
#'                     street = c("1 A", "2 B", "2 B", "2 B", "2 B"))
#' panel <- attribute_spell_age(panel, attribute = "street")
#' current <- data.frame(npi = c("1", "2"), street = c("2 B", "9 Z"))
#' attribute_agreement_by_spell_age(current, panel, attributes = "street")
#' @seealso [attribute_spell_age()]; `vignette("lead-lag-staleness")`.
#' @family no-gold-standard comparison
#' @export
attribute_agreement_by_spell_age <- function(source_a, panel_b, attributes,
                                             id = "npi", year = "year",
                                             target_year = NULL,
                                             comparison_label = "Source A vs Source B",
                                             age_col = "spell_age_years",
                                             stratification_label = "spell_age_years") {
  checkmate::assert_data_frame(source_a, min.rows = 1)
  checkmate::assert_data_frame(panel_b, min.rows = 1)
  checkmate::assert_character(attributes, min.len = 1, any.missing = FALSE, unique = TRUE)
  if (!age_col %in% names(panel_b)) {
    stop(sprintf("attribute_agreement_by_spell_age: panel_b has no column '%s'", age_col), call. = FALSE)
  }
  for (col in c(id, attributes)) {
    if (!col %in% names(source_a)) {
      stop(sprintf("attribute_agreement_by_spell_age: source_a has no column '%s'", col), call. = FALSE)
    }
    if (!col %in% names(panel_b)) {
      stop(sprintf("attribute_agreement_by_spell_age: panel_b has no column '%s'", col), call. = FALSE)
    }
  }
  if (!year %in% names(panel_b)) {
    stop(sprintf("attribute_agreement_by_spell_age: panel_b has no column '%s'", year), call. = FALSE)
  }

  raw_ages <- .parse_whole_integer(panel_b[[age_col]], sprintf("attribute_agreement_by_spell_age: %s", age_col), allow_na = TRUE)
  if (any(!is.na(raw_ages) & raw_ages < 0L)) {
    stop(sprintf("attribute_agreement_by_spell_age: '%s' contains negative spell_age values", age_col), call. = FALSE)
  }
  panel_b[[age_col]] <- raw_ages

  age_levels <- c(
    "0 yrs (updated in snapshot yr)",
    "1-2 yrs",
    "3-5 yrs",
    "6-10 yrs",
    "11+ yrs (un-updated >=11 yrs)"
  )
  classify_age <- function(yrs) {
    dplyr::case_when(
      is.na(yrs)    ~ NA_character_,
      yrs == 0L     ~ age_levels[1],
      yrs %in% 1:2  ~ age_levels[2],
      yrs %in% 3:5  ~ age_levels[3],
      yrs %in% 6:10 ~ age_levels[4],
      yrs > 10L     ~ age_levels[5]
    )
  }

  da <- source_a
  da[[id]] <- trimws(as.character(da[[id]]))
  db <- panel_b
  db[[id]] <- trimws(as.character(db[[id]]))
  db[[year]] <- .parse_whole_integer(db[[year]], "attribute_agreement_by_spell_age: panel_b year", allow_na = FALSE)
  .assert_unique_keys(db, c(id, year), "attribute_agreement_by_spell_age: panel_b")

  for (at in attributes) {
    va <- toupper(trimws(as.character(da[[at]])))
    da[[paste0(at, "_a")]] <- ifelse(is.na(va) | !nzchar(va), NA_character_, va)
    vb <- toupper(trimws(as.character(db[[at]])))
    db[[paste0(at, "_b")]] <- ifelse(is.na(vb) | !nzchar(vb), NA_character_, vb)
  }

  if (year %in% names(da)) {
    da[[year]] <- .parse_whole_integer(da[[year]], "attribute_agreement_by_spell_age: source_a year", allow_na = FALSE)
    .assert_unique_keys(da, c(id, year), "attribute_agreement_by_spell_age: source_a")
    joined <- merge(da, db, by = c(id, year))
  } else {
    .assert_unique_keys(da, id, "attribute_agreement_by_spell_age: source_a")
    if (is.null(target_year)) target_year <- max(db[[year]], na.rm = TRUE)
    target_year <- as.integer(target_year)
    if (!target_year %in% db[[year]]) {
      stop(sprintf("attribute_agreement_by_spell_age: target_year %d is not present in panel_b", target_year), call. = FALSE)
    }
    joined <- merge(da, db[db[[year]] == target_year, , drop = FALSE], by = id)
  }

  joined$stratum <- classify_age(as.integer(joined[[age_col]]))
  joined <- joined[!is.na(joined$stratum), , drop = FALSE]
  if (nrow(joined) == 0L) {
    stop("attribute_agreement_by_spell_age: no non-missing matched rows after joining and classifying spell age", call. = FALSE)
  }

  groups <- split(joined, factor(joined$stratum, levels = age_levels), drop = TRUE)
  rows <- lapply(groups, function(g) {
    row <- tibble::tibble(
      comparison     = comparison_label,
      stratification = stratification_label,
      stratum        = as.character(g$stratum[1]),
      n              = nrow(g)
    )
    for (at in attributes) {
      xa <- g[[paste0(at, "_a")]]
      xb <- g[[paste0(at, "_b")]]
      valid <- !is.na(xa) & !is.na(xb)
      row[[paste0(at, "_agree_pct")]] <- if (any(valid)) {
        round(100 * mean(xa[valid] == xb[valid]), 1)
      } else {
        NA_real_
      }
    }
    row
  })
  tibble::as_tibble(dplyr::bind_rows(rows))
}

#' Approach 3: Administrative enrollment interval lead/lag timing around panel moves
#'
#' Compares consecutive-year state (or attribute) transitions in a longitudinal
#' panel (`panel`) against date-stamped administrative enrollment intervals
#' (`intervals`, e.g. state Medicaid provider enrollment `interval_start` and
#' `interval_end`). Evaluates both **arrival timing** (does destination-state
#' enrollment start before or after the panel move year, restricted to intervals
#' active at or after the start of the origin year `t - 1`?) and **departure
#' timing** (does origin-state enrollment terminate before, during, or after
#' the panel move year?).
#'
#' @details
#' A *move* is a pair of consecutive panel years (`t - 1`, `t`) for one id
#' whose state differs, with `t >= min_move_year`. Panel rows with missing
#' state are dropped first, so a gap year does not create a move.
#'
#' **Arrival.** Each move is matched to the id's intervals in the
#' destination state that are still active on or after 1 January of
#' `t - 1`. The lag is `t` minus the interval's start year; when several
#' intervals match, the one with the smallest absolute lag (then the latest
#' start) is kept. Positive lags mean enrollment started first (the panel
#' lagged); negative lags mean the panel moved first.
#'
#' **Departure.** Each move is matched to the id's intervals in the origin
#' state that started by 31 December of `t`. An open-ended interval is
#' preferred, then the latest end. Termination before `t`, in `t`, or after
#' `t` (or still open) gives the three departure categories.
#'
#' Moves with no matching interval are left out of `total_matched_moves`
#' and the percentages, but are counted in `total_panel_moves`.
#'
#' @param panel Longitudinal panel data frame with `id`, `year`, and `state`.
#' @param intervals Enrollment interval data frame with `id`, `state`,
#'   `interval_start` (`Date`), and `interval_end` (`Date`; `NA` or `>= open_end_cutoff`
#'   is treated as open-ended/still active).
#' @param id Name of the identifier column. Default `"npi"`.
#' @param year Name of the integer year column in `panel`. Default `"year"`.
#' @param state Name of the state/attribute column in `panel` and `intervals`.
#'   Default `"state"`.
#' @param min_move_year Minimum transition year `t` (in `t - 1 -> t`) to include.
#'   Default `2016L`.
#' @param open_end_cutoff Date at or above which `interval_end` is treated as
#'   open-ended (active indefinitely). Default `as.Date("2099-01-01")`.
#' @return A tibble with arrival and departure lead/lag distributions:
#'   `direction_type` (`"destination_arrival_timing"` or
#'   `"origin_departure_termination_timing"`), `category`, `n`,
#'   `total_panel_moves`, `total_matched_moves`, and `pct_of_matched`
#'   (percent of `total_matched_moves` for that direction, one decimal).
#'   Every category is listed, including those with `n = 0`, in this order.
#'   Arrival: `"Enrollment led by 3+ yrs (pre-existing / long panel lag)"`,
#'   `"Enrollment led by 2 yrs (panel lagged 2 yrs)"`,
#'   `"Enrollment led by 1 yr (panel lagged 1 yr)"`,
#'   `"Same calendar year (0 yr lag)"`,
#'   `"Panel led by 1 yr (enrollment lagged 1 yr)"`,
#'   `"Panel led by 2+ yrs (enrollment lagged 2+ yrs)"`. Departure:
#'   `"Terminated BEFORE panel move year (panel lagged departure)"`,
#'   `"Terminated in SAME calendar year as panel move"`,
#'   `"Terminated AFTER panel move year or still open-ended (enrollment lagged departure)"`.
#'   A direction with no matched move contributes no rows.
#' @examples
#' panel <- data.frame(npi = c("1", "1"), year = c(2023, 2024), state = c("CO", "NM"))
#' intervals <- data.frame(
#'   npi = c("1", "1"), state = c("NM", "CO"),
#'   interval_start = as.Date(c("2023-05-01", "2020-01-01")),
#'   interval_end = as.Date(c("2099-12-31", "2025-03-31"))
#' )
#' enrollment_move_lead_lag(panel, intervals)
#' @seealso `vignette("lead-lag-staleness")`.
#' @family no-gold-standard comparison
#' @export
enrollment_move_lead_lag <- function(panel, intervals,
                                     id = "npi", year = "year", state = "state",
                                     min_move_year = 2016L,
                                     open_end_cutoff = as.Date("2099-01-01")) {
  checkmate::assert_data_frame(panel, min.rows = 1)
  checkmate::assert_data_frame(intervals, min.rows = 1)
  for (col in c(id, year, state)) {
    if (!col %in% names(panel)) {
      stop(sprintf("enrollment_move_lead_lag: panel has no column '%s'", col), call. = FALSE)
    }
  }
  for (col in c(id, state, "interval_start", "interval_end")) {
    if (!col %in% names(intervals)) {
      stop(sprintf("enrollment_move_lead_lag: intervals has no column '%s'", col), call. = FALSE)
    }
  }

  p <- tibble::tibble(
    id    = trimws(as.character(panel[[id]])),
    year  = .parse_whole_integer(panel[[year]], "enrollment_move_lead_lag: panel year", allow_na = FALSE),
    state = toupper(trimws(as.character(panel[[state]])))
  )
  .assert_unique_keys(p, c("id", "year"), "enrollment_move_lead_lag: panel")
  p <- p[!is.na(p$id) & nzchar(p$id) & !is.na(p$state) & nzchar(p$state), , drop = FALSE]
  p <- p[order(p$id, p$year), , drop = FALSE]

  groups <- split(p, p$id, drop = TRUE)
  move_list <- lapply(groups, function(g) {
    if (nrow(g) < 2L) return(NULL)
    n <- nrow(g)
    prev_yr <- c(NA_integer_, g$year[1:(n - 1L)])
    orig_st <- c(NA_character_, g$state[1:(n - 1L)])
    keep <- !is.na(prev_yr) & (g$year == prev_yr + 1L) & (g$state != orig_st) & (g$year >= min_move_year)
    if (!any(keep)) return(NULL)
    tibble::tibble(
      id         = g$id[keep],
      year       = g$year[keep],
      orig_state = orig_st[keep],
      dest_state = g$state[keep]
    )
  })
  moves <- dplyr::bind_rows(move_list)
  n_total_moves <- nrow(moves)
  if (n_total_moves == 0L) {
    stop("enrollment_move_lead_lag: no consecutive-year moves found in panel", call. = FALSE)
  }

  parse_date_safe <- function(x) {
    if (inherits(x, "Date")) return(x)
    s <- substr(trimws(as.character(x)), 1, 10)
    s[!grepl("^\\d{4}-\\d{2}-\\d{2}$", s)] <- NA_character_
    suppressWarnings(as.Date(s))
  }

  iv <- tibble::tibble(
    id             = trimws(as.character(intervals[[id]])),
    state          = toupper(trimws(as.character(intervals[[state]]))),
    interval_start = parse_date_safe(intervals$interval_start),
    interval_end   = parse_date_safe(intervals$interval_end)
  )
  if (any(is.na(iv$id) | !nzchar(iv$id) | is.na(iv$state) | !nzchar(iv$state))) {
    stop("enrollment_move_lead_lag: intervals contains missing or empty id or state values", call. = FALSE)
  }
  if (any(is.na(iv$interval_start))) {
    stop("enrollment_move_lead_lag: intervals contains missing or unparseable interval_start dates", call. = FALSE)
  }
  if (any(!is.na(iv$interval_end) & iv$interval_end < iv$interval_start)) {
    stop("enrollment_move_lead_lag: intervals contains inverted dates (interval_end < interval_start)", call. = FALSE)
  }

  # 1. Destination arrival timing (only intervals still active on or after Jan 1 of origin year t-1)
  m_dest <- merge(moves, iv, by.x = c("id", "dest_state"), by.y = c("id", "state"))
  if (nrow(m_dest) > 0L) {
    m_dest <- m_dest[is.na(m_dest$interval_end) |
                       m_dest$interval_end >= as.Date(sprintf("%d-01-01", m_dest$year - 1L)), , drop = FALSE]
  }
  dest_levels <- c(
    "Enrollment led by 3+ yrs (pre-existing / long panel lag)",
    "Enrollment led by 2 yrs (panel lagged 2 yrs)",
    "Enrollment led by 1 yr (panel lagged 1 yr)",
    "Same calendar year (0 yr lag)",
    "Panel led by 1 yr (enrollment lagged 1 yr)",
    "Panel led by 2+ yrs (enrollment lagged 2+ yrs)"
  )

  dest_summary <- tibble::tibble()
  if (nrow(m_dest) > 0L) {
    m_dest$start_yr <- as.integer(format(m_dest$interval_start, "%Y"))
    m_dest$lag_yrs  <- m_dest$year - m_dest$start_yr
    m_dest$abs_diff <- abs(m_dest$lag_yrs)
    m_dest <- m_dest[order(m_dest$id, m_dest$year, m_dest$abs_diff, -as.numeric(m_dest$interval_start)), , drop = FALSE]
    m_dest <- m_dest[!duplicated(m_dest[, c("id", "year")]), , drop = FALSE]

    cat_dest <- dplyr::case_when(
      m_dest$lag_yrs >= 3L  ~ dest_levels[1],
      m_dest$lag_yrs == 2L  ~ dest_levels[2],
      m_dest$lag_yrs == 1L  ~ dest_levels[3],
      m_dest$lag_yrs == 0L  ~ dest_levels[4],
      m_dest$lag_yrs == -1L ~ dest_levels[5],
      m_dest$lag_yrs <= -2L ~ dest_levels[6]
    )
    tab_d <- table(factor(cat_dest, levels = dest_levels))
    dest_summary <- tibble::tibble(
      direction_type      = "destination_arrival_timing",
      category            = names(tab_d),
      n                   = as.integer(tab_d),
      total_panel_moves   = n_total_moves,
      total_matched_moves = nrow(m_dest),
      pct_of_matched      = round(100 * as.integer(tab_d) / nrow(m_dest), 1)
    )
  }

  # 2. Origin departure termination timing
  m_orig <- merge(moves, iv, by.x = c("id", "orig_state"), by.y = c("id", "state"))
  m_orig <- m_orig[m_orig$interval_start <= as.Date(sprintf("%d-12-31", m_orig$year)), , drop = FALSE]
  orig_levels <- c(
    "Terminated BEFORE panel move year (panel lagged departure)",
    "Terminated in SAME calendar year as panel move",
    "Terminated AFTER panel move year or still open-ended (enrollment lagged departure)"
  )

  orig_summary <- tibble::tibble()
  if (nrow(m_orig) > 0L) {
    m_orig$still_open <- is.na(m_orig$interval_end) | (m_orig$interval_end >= open_end_cutoff)
    m_orig$end_yr <- ifelse(m_orig$still_open, NA_integer_, as.integer(format(m_orig$interval_end, "%Y")))
    m_orig$term_lag <- m_orig$end_yr - m_orig$year
    m_orig <- m_orig[order(m_orig$id, m_orig$year, -as.integer(m_orig$still_open), -as.numeric(m_orig$interval_end)), , drop = FALSE]
    m_orig <- m_orig[!duplicated(m_orig[, c("id", "year")]), , drop = FALSE]

    cat_orig <- dplyr::case_when(
      m_orig$still_open | m_orig$term_lag >= 1L ~ orig_levels[3],
      m_orig$term_lag == 0L                     ~ orig_levels[2],
      m_orig$term_lag < 0L                      ~ orig_levels[1]
    )
    tab_o <- table(factor(cat_orig, levels = orig_levels))
    orig_summary <- tibble::tibble(
      direction_type      = "origin_departure_termination_timing",
      category            = names(tab_o),
      n                   = as.integer(tab_o),
      total_panel_moves   = n_total_moves,
      total_matched_moves = nrow(m_orig),
      pct_of_matched      = round(100 * as.integer(tab_o) / nrow(m_orig), 1)
    )
  }

  tibble::as_tibble(dplyr::bind_rows(dest_summary, orig_summary))
}

#' Generate programmatic manuscript paragraphs for all three no-gold-standard approaches
#'
#' Produces three publication-ready paragraphs (one per approach) and a combined
#' block from the output tables of [attribute_lead_lag_matrix()],
#' [attribute_agreement_by_spell_age()], and [enrollment_move_lead_lag()].
#'
#' @details
#' **Treat the output as a first draft.** The five `*_label` arguments are
#' the only wording you can change. The rest of each sentence was written
#' for the OB/GYN midwifery workforce study, and it is fixed whatever the
#' inputs:
#'
#' * Study years: `2021--2025` transitions, `2022` discordances tracked
#'   through `2026`, a `2007--2025` back-check, a `20-year panel
#'   (2007--2026)`, and a `2016--2026` move cohort. These do not follow
#'   `base_year`, `cross_section_year`, or `min_move_year`.
#' * Source-specific detail: the Open Payments field
#'   `Recipient_Primary_Business_Street_Address_Line1`, manufacturer CRM
#'   systems, "Medicare PECOS reassignment", the Medicaid fields
#'   `ENRL_START_DT`/`ENRL_END_DT`, and "her" for the clinician.
#' * Interpretations: paragraph 1 says the back-check is "confirming" that
#'   the panel is not retaining outdated addresses, and paragraph 2 says
#'   agreement "dipped" at 0 years and "decayed steadily". The function
#'   inserts the numbers whatever they show, so check that each sentence
#'   still matches them.
#'
#' The text uses `$...$` math and Markdown emphasis. Render it with R
#' Markdown or Quarto, for example with `cat()` in a `results = "asis"` chunk.
#'
#' **Required inputs.** It stops instead of writing `"NA"` into a sentence.
#'
#' * `lead_lag_df` has one row each for `state`, `city`, `zip5`, and
#'   `street`, with the multi-year and cross-section columns non-missing.
#'   `attribute_lead_lag_matrix()` therefore needs a `cross_section`.
#' * In `freshness_df`, the first `comparison` block must have
#'   `stratification` equal to `"nppes_street_spell_age"` or
#'   `"spell_age_years"`, and the `"0 yrs ..."`, `"1-2 yrs"`, and
#'   `"11+ yrs ..."` strata. Two optional sentences are added when present.
#'   One needs a `comparison` containing `"Open Payments"` with `0 yrs` and
#'   `11+ yrs` strata. The other needs a `stratification` of
#'   `"amcb_career_tenure"` with strata starting `"3-5 yrs"`, `"11-20 yrs"`,
#'   and `"21+ yrs"`. [attribute_agreement_by_spell_age()] does not produce
#'   the last two career-tenure labels, so you must relabel those rows
#'   yourself.
#' * `enrollment_df` has both `direction_type` blocks.
#'
#' **What it computes.** Percentages from the input tables go in as they
#' are. Only these values are worked out here:
#'
#' * the two "-fold" break ratios, A breaks / max(1, B breaks);
#' * the arrival percentages, re-based on the "acute window" of matched moves
#'   (every arrival category except enrollment leading by 3+ years);
#' * the enrollment lead ratio.
#'
#' @param lead_lag_df Output of [attribute_lead_lag_matrix()].
#' @param freshness_df Output of [attribute_agreement_by_spell_age()] (containing
#'   one or more `comparison` / `stratification` blocks).
#' @param enrollment_df Output of [enrollment_move_lead_lag()].
#' @param roster_label Name of the certifying roster lacking addresses (default
#'   `"the American Midwifery Certification Board roster"`).
#' @param panel_label Name of the longitudinal self-reported panel (default
#'   `"NPPES"`).
#' @param source_a_label Name of longitudinal transaction Source A (default
#'   `"CMS Open Payments"`).
#' @param cross_section_label Name of cross-sectional Source C (default
#'   `"CMS Doctors and Clinicians (DAC/PECOS)"`).
#' @param enrollment_label Name of administrative enrollment source (default
#'   `"Medicaid"`).
#' @return Named list with `paragraph_1_lead_lag`, `paragraph_2_freshness`,
#'   `paragraph_3_enrollment`, and `combined_text` (the three joined by a
#'   blank line), each a single character string.
#' @seealso `vignette("lead-lag-staleness")` for the full output on example
#'   inputs.
#' @examples
#' # lead_lag_df needs exactly these four attribute rows and columns --
#' # round numbers here stand in for a real attribute_lead_lag_matrix()/
#' # attribute_lead_lag_duration() result; see Details for what each means.
#' lead_lag_df <- tibble::tibble(
#'   attribute = c("state", "city", "zip5", "street"),
#'   n_paired_transitions = 100L, n_concordant_at_t = 80L,
#'   a_breaks_concordance_first = 12L, b_breaks_concordance_first = 8L,
#'   both_move_together_at_t1 = 5L, n_discordant_at_t = 20L,
#'   b_adopts_a_at_t1_n = 10L, b_adopts_a_at_t1_pct = 50,
#'   a_reverts_to_b_at_t1_n = 6L, a_reverts_to_b_at_t1_pct = 30,
#'   persistent_discordant_at_t1_pct = 20, base_year = 2022L,
#'   disc_base_n = 20L, b_adopts_a_multi_n = 12L, b_adopts_a_multi_pct = 60,
#'   a_reverts_b_multi_n = 8L, a_reverts_b_multi_pct = 40,
#'   cross_section_disc_n = 20L, cross_section_in_prior_b_n = 4L,
#'   cross_section_in_prior_b_pct = 20
#' )
#' # freshness_df needs exactly these three spell-age strata.
#' freshness_df <- tibble::tibble(
#'   comparison = "DAC (2026) vs NPPES (2026)", stratification = "nppes_street_spell_age",
#'   stratum = c("0 yrs (updated in snapshot yr)", "1-2 yrs", "11+ yrs (un-updated >=11 yrs)"),
#'   n = c(70L, 150L, 100L), state_agree_pct = c(87.6, 91.8, 89.1),
#'   city_agree_pct = c(51.9, 62.7, 47.7), zip5_agree_pct = c(43.4, 57.8, 36.3),
#'   street_agree_pct = c(34.0, 43.7, 20.4)
#' )
#' # enrollment_df is [enrollment_move_lead_lag()]'s own output, unchanged.
#' panel <- data.frame(npi = c("1", "1"), year = c(2023, 2024), state = c("CO", "NM"))
#' intervals <- data.frame(
#'   npi = c("1", "1"), state = c("NM", "CO"),
#'   interval_start = as.Date(c("2023-05-01", "2020-01-01")),
#'   interval_end = as.Date(c("2099-12-31", "2025-03-31"))
#' )
#' enrollment_df <- enrollment_move_lead_lag(panel, intervals)
#'
#' prose <- format_no_gold_standard_prose(lead_lag_df, freshness_df, enrollment_df)
#' names(prose)
#' @family no-gold-standard comparison
#' @export
format_no_gold_standard_prose <- function(lead_lag_df, freshness_df, enrollment_df,
                                          roster_label = "the American Midwifery Certification Board roster",
                                          panel_label = "NPPES",
                                          source_a_label = "CMS Open Payments",
                                          cross_section_label = "CMS Doctors and Clinicians (DAC/PECOS)",
                                          enrollment_label = "Medicaid") {
  checkmate::assert_data_frame(lead_lag_df, min.rows = 1)
  checkmate::assert_data_frame(freshness_df, min.rows = 1)
  checkmate::assert_data_frame(enrollment_df, min.rows = 1)
  checkmate::assert_string(roster_label, min.chars = 1)
  checkmate::assert_string(panel_label, min.chars = 1)
  checkmate::assert_string(source_a_label, min.chars = 1)
  checkmate::assert_string(cross_section_label, min.chars = 1)
  checkmate::assert_string(enrollment_label, min.chars = 1)

  fmt_n <- function(x) formatC(as.numeric(x), format = "d", big.mark = ",")
  attr_col <- if ("attribute" %in% names(lead_lag_df)) "attribute" else "level"
  .assert_unique_keys(lead_lag_df, attr_col, "format_no_gold_standard_prose: lead_lag_df")

  need_attrs <- c("state", "city", "zip5", "street")
  missing_attrs <- setdiff(need_attrs, lead_lag_df[[attr_col]])
  if (length(missing_attrs) > 0L) {
    stop(sprintf(
      "format_no_gold_standard_prose: lead_lag_df is missing required attribute row(s): %s",
      paste(missing_attrs, collapse = ", ")
    ), call. = FALSE)
  }
  ll_row <- function(lv) lead_lag_df[lead_lag_df[[attr_col]] == lv, , drop = FALSE]

  st <- ll_row("state")
  ct <- ll_row("city")
  zp <- ll_row("zip5")
  sr <- ll_row("street")

  get_val <- function(df, generic_nm, legacy_nm) {
    v <- if (generic_nm %in% names(df)) df[[generic_nm]][1] else df[[legacy_nm]][1]
    if (is.null(v) || is.na(v)) {
      stop(sprintf("format_no_gold_standard_prose: required metric '%s' is NA or missing", generic_nm), call. = FALSE)
    }
    v
  }

  st_a_brk <- get_val(st, "a_breaks_concordance_first", "op_breaks_concordance_first")
  st_b_brk <- get_val(st, "b_breaks_concordance_first", "nppes_breaks_concordance_first")
  sr_a_brk <- get_val(sr, "a_breaks_concordance_first", "op_breaks_concordance_first")
  sr_b_brk <- get_val(sr, "b_breaks_concordance_first", "nppes_breaks_concordance_first")

  ratio_state_break  <- st_a_brk / pmax(1, st_b_brk)
  ratio_street_break <- sr_a_brk / pmax(1, sr_b_brk)

  st_b_multi_pct <- get_val(st, "b_adopts_a_multi_pct", "nppes_adopts_op2022_by_2026_pct")
  st_b_multi_n   <- get_val(st, "b_adopts_a_multi_n", "nppes_adopts_op2022_by_2026_n")
  st_disc_base   <- get_val(st, "disc_base_n", "disc_2022_n")

  sr_b_multi_pct <- get_val(sr, "b_adopts_a_multi_pct", "nppes_adopts_op2022_by_2026_pct")
  sr_b_multi_n   <- get_val(sr, "b_adopts_a_multi_n", "nppes_adopts_op2022_by_2026_n")
  sr_disc_base   <- get_val(sr, "disc_base_n", "disc_2022_n")

  ct_a_rev_pct <- get_val(ct, "a_reverts_b_multi_pct", "op_reverts_np2022_by_2025_pct")
  zp_a_rev_pct <- get_val(zp, "a_reverts_b_multi_pct", "op_reverts_np2022_by_2025_pct")
  sr_a_rev_pct <- get_val(sr, "a_reverts_b_multi_pct", "op_reverts_np2022_by_2025_pct")

  sr_cs_pct  <- get_val(sr, "cross_section_in_prior_b_pct", "dac2026_in_prior_nppes_pct")
  sr_cs_n    <- get_val(sr, "cross_section_in_prior_b_n", "dac2026_in_prior_nppes_n")
  sr_cs_disc <- get_val(sr, "cross_section_disc_n", "dac2026_disc_n")

  zp_cs_pct  <- get_val(zp, "cross_section_in_prior_b_pct", "dac2026_in_prior_nppes_pct")
  zp_cs_n    <- get_val(zp, "cross_section_in_prior_b_n", "dac2026_in_prior_nppes_n")
  zp_cs_disc <- get_val(zp, "cross_section_disc_n", "dac2026_disc_n")

  p1 <- sprintf(
    paste0(
      "Because %s carries no practice or residential address, ",
      "cross-sectional disagreement between two external registries in a single year ($A_t \\neq B_t$) cannot by itself establish which source is stale. ",
      "To resolve directionality without a ground-truth registry, we tracked %s consecutive provider-year transitions (2021--2025) ",
      "linking %s annual snapshots to %s (`Recipient_Primary_Business_Street_Address_Line1`, the recipient's reported primary practice location rather than a travel or event site). ",
      "When the two sources agreed in year $t$ and diverged in year $t+1$, %s broke concordance first %.1f-fold more often than %s at the state level ",
      "(%s vs. %s transitions) and %.1f-fold more often at the street level (%s vs. %s transitions). ",
      "Tracking 2022 discordances forward through 2026 separated these divergences into two distinct mechanisms: ",
      "(1) *genuine relocation lead* (%.1f%% [%s/%s] of state disagreements and %.1f%% [%s/%s] of street disagreements), ",
      "in which manufacturer field representatives logged the clinician's new primary business address in their customer-relationship management (CRM) system ",
      "1 to 4 years before the clinician updated her self-reported `practice_address` in %s; and ",
      "(2) *multi-site or vendor-CRM modal switching* (%.1f%% of city, %.1f%% of ZIP, and %.1f%% of street disagreements), ",
      "in which different manufacturers submitting payments in year $t$ versus $t+1$ recorded different clinic sites ",
      "(or the CMS Validated List/%s address versus a local satellite clinic) for the same clinician. ",
      "Conversely, back-checking 2026 %s discordances against all historical %s snapshots (2007--2025) ",
      "showed that only %.1f%% (%s/%s) of discordant street addresses and %.1f%% (%s/%s) of discordant ZIP codes ever appeared in an earlier %s file, ",
      "confirming that cross-registry disagreement is not driven by %s retaining outdated %s addresses."
    ),
    roster_label,
    fmt_n(st$n_paired_transitions[1]),
    panel_label, source_a_label,
    source_a_label, ratio_state_break, panel_label,
    fmt_n(st_a_brk), fmt_n(st_b_brk),
    ratio_street_break, fmt_n(sr_a_brk), fmt_n(sr_b_brk),
    st_b_multi_pct, fmt_n(st_b_multi_n), fmt_n(st_disc_base),
    sr_b_multi_pct, fmt_n(sr_b_multi_n), fmt_n(sr_disc_base),
    panel_label,
    ct_a_rev_pct, zp_a_rev_pct, sr_a_rev_pct,
    panel_label,
    cross_section_label, panel_label,
    sr_cs_pct, fmt_n(sr_cs_n), fmt_n(sr_cs_disc),
    zp_cs_pct, fmt_n(zp_cs_n), fmt_n(zp_cs_disc),
    panel_label, cross_section_label, panel_label
  )

  # Paragraph 2: Freshness decay
  comps <- unique(freshness_df$comparison)
  f_dac <- freshness_df[freshness_df$comparison == comps[1] &
                        freshness_df$stratification %in% c("nppes_street_spell_age", "spell_age_years"), , drop = FALSE]
  f_op  <- freshness_df[grepl("Open Payments", freshness_df$comparison), , drop = FALSE]
  f_car <- freshness_df[freshness_df$stratification == "amcb_career_tenure", , drop = FALSE]

  d0  <- f_dac[grepl("^0 yrs", f_dac$stratum), , drop = FALSE]
  d12 <- f_dac[f_dac$stratum == "1-2 yrs", , drop = FALSE]
  d11 <- f_dac[grepl("^11\\+ yrs", f_dac$stratum), , drop = FALSE]

  if (nrow(d0) == 0L || nrow(d12) == 0L || nrow(d11) == 0L) {
    stop("format_no_gold_standard_prose: missing required freshness stratum ('0 yrs', '1-2 yrs', or '11+ yrs') in freshness_df", call. = FALSE)
  }

  op_clause <- ""
  if (nrow(f_op) > 0L) {
    op0  <- f_op[grepl("^0 yrs", f_op$stratum), , drop = FALSE]
    op11 <- f_op[grepl("^11\\+ yrs", f_op$stratum), , drop = FALSE]
    if (nrow(op0) > 0L && nrow(op11) > 0L) {
      op_clause <- sprintf(
        paste0(
          " By contrast, when the same freshness stratification was applied to %s versus %s (2022--2025; $n=%s$ provider-years), ",
          "street agreement showed no decay with %s record age (%.1f%% at 0 years vs. %.1f%% at $\\ge 11$ years), ",
          "corroborating CMS documentation that manufacturer reporting pipelines draw recipient profile data directly from %s and therefore echo longstanding un-updated %s addresses."
        ),
        source_a_label, panel_label, fmt_n(sum(f_op$n)),
        panel_label, op0$street_agree_pct[1], op11$street_agree_pct[1],
        panel_label, panel_label
      )
    }
  }

  car_clause <- ""
  if (nrow(f_car) > 0L) {
    c35   <- f_car[grepl("^3-5 yrs", f_car$stratum), , drop = FALSE]
    c1120 <- f_car[grepl("^11-20 yrs", f_car$stratum), , drop = FALSE]
    c21   <- f_car[grepl("^21\\+ yrs", f_car$stratum), , drop = FALSE]
    if (nrow(c35) > 0L && nrow(c1120) > 0L && nrow(c21) > 0L) {
      car_clause <- sprintf(
        " A parallel gradient appeared across career stage, with street-level concordance declining from %.1f%% among clinicians 3 to 5 years post-certification to %.1f%%--%.1f%% among clinicians more than 10 years post-certification.",
        c35$street_agree_pct[1], c1120$street_agree_pct[1], c21$street_agree_pct[1]
      )
    }
  }

  p2 <- sprintf(
    paste0(
      "To test whether cross-database disagreement reflects accumulated staleness in self-reported %s records, ",
      "we reconstructed each provider's empirical %s address-spell age across the 20-year panel (2007--2026)---",
      "defined as the number of consecutive years the provider's 2026 %s street address had remained unchanged---",
      "and stratified year-matched 2026 %s-versus-%s concordance across %s linked clinicians. ",
      "Concordance followed a pronounced staleness decay curve: agreement peaked among clinicians whose %s address had been updated 1 to 2 years earlier ",
      "(%.1f%% state, %.1f%% city, %.1f%% ZIP, and %.1f%% street; $n=%s$), ",
      "dipped in the immediate year of an %s update (%.1f%% state and %.1f%% street at 0 years; $n=%s$) while Medicare PECOS reassignment caught up, ",
      "and decayed steadily as uninterrupted %s spell age increased, falling to %.1f%% for city, %.1f%% for ZIP, and %.1f%% for street ",
      "among records untouched for 11 or more years ($n=%s$).%s%s"
    ),
    panel_label, panel_label, panel_label,
    cross_section_label, panel_label, fmt_n(sum(f_dac$n)),
    panel_label,
    d12$state_agree_pct[1], d12$city_agree_pct[1], d12$zip5_agree_pct[1], d12$street_agree_pct[1], fmt_n(d12$n[1]),
    panel_label,
    d0$state_agree_pct[1], d0$street_agree_pct[1], fmt_n(d0$n[1]),
    panel_label,
    d11$city_agree_pct[1], d11$zip5_agree_pct[1], d11$street_agree_pct[1], fmt_n(d11$n[1]),
    car_clause, op_clause
  )

  # Paragraph 3: Enrollment move lead/lag
  m_dest <- enrollment_df[enrollment_df$direction_type == "destination_arrival_timing", , drop = FALSE]
  m_orig <- enrollment_df[enrollment_df$direction_type == "origin_departure_termination_timing", , drop = FALSE]
  if (nrow(m_dest) == 0L || nrow(m_orig) == 0L) {
    stop("format_no_gold_standard_prose: enrollment_df is missing destination_arrival_timing or origin_departure_termination_timing rows", call. = FALSE)
  }

  n_med_lead_12 <- sum(m_dest$n[grepl("led by [12] yr", m_dest$category) & !grepl("Panel|NPPES", m_dest$category)])
  n_same_yr     <- sum(m_dest$n[grepl("Same calendar year", m_dest$category)])
  n_nppes_lead  <- sum(m_dest$n[grepl("(Panel|NPPES) led by", m_dest$category)])
  n_acute       <- n_med_lead_12 + n_same_yr + n_nppes_lead
  if (n_acute == 0L) {
    stop("format_no_gold_standard_prose: zero acute moves in [-2, +2] year window", call. = FALSE)
  }

  orig_before <- m_orig[grepl("BEFORE", m_orig$category), , drop = FALSE]
  orig_same   <- m_orig[grepl("SAME", m_orig$category), , drop = FALSE]
  orig_after  <- m_orig[grepl("AFTER", m_orig$category), , drop = FALSE]

  get_orig <- function(df_sub, col, default_val = 0) {
    if (nrow(df_sub) > 0L) df_sub[[col]][1] else default_val
  }

  n_tot_moves <- if ("total_panel_moves" %in% names(m_dest)) {
    m_dest$total_panel_moves[1]
  } else if ("total_nppes_moves" %in% names(m_dest)) {
    m_dest$total_nppes_moves[1]
  } else {
    m_dest$total_matched_moves[1]
  }

  p3 <- sprintf(
    paste0(
      "Finally, we evaluated exact administrative lead and lag times around interstate relocations by linking all %s consecutive-year %s state transitions ",
      "in the canonical cohort (2016--2026) to date-stamped state %s provider enrollment intervals (`ENRL_START_DT` and `ENRL_END_DT`). ",
      "Among the %s %s interstate moves with an observed destination-state %s enrollment segment, arrival and departure exhibited sharp administrative asymmetry. ",
      "On arrival, destination-state %s enrollment preceded or coincided with the %s state change in the vast majority of moves: ",
      "excluding pre-existing multistate enrollments ($\\ge 3$ years prior; $n=%s$) to focus on the acute window around the transition ($n=%s$), ",
      "%.1f%% ($n=%s$) of clinicians enrolled in their destination state's %s program 1 to 2 years *before* their %s practice state changed ",
      "and %.1f%% ($n=%s$) enrolled in the same calendar year, whereas only %.1f%% ($n=%s$) updated %s prior to %s enrollment ",
      "(a %.1f-to-1 lead ratio in favor of %s). ",
      "On departure, however, %s termination lagged substantially behind %s: among %s moves with prior origin-state %s enrollment, ",
      "%.1f%% ($n=%s$) remained open-ended or terminated 1 or more years *after* the clinician had already relocated her primary practice state in %s, ",
      "compared with %.1f%% ($n=%s$) terminating in the move year and %.1f%% ($n=%s$) terminating beforehand."
    ),
    fmt_n(n_tot_moves), panel_label, enrollment_label,
    fmt_n(m_dest$total_matched_moves[1]), panel_label, enrollment_label,
    enrollment_label, panel_label,
    fmt_n(sum(m_dest$n[grepl("3\\+ yrs", m_dest$category)])),
    fmt_n(n_acute),
    100 * n_med_lead_12 / n_acute, fmt_n(n_med_lead_12), enrollment_label, panel_label,
    100 * n_same_yr / n_acute, fmt_n(n_same_yr),
    100 * n_nppes_lead / n_acute, fmt_n(n_nppes_lead), panel_label, enrollment_label,
    n_med_lead_12 / pmax(1, n_nppes_lead), enrollment_label,
    enrollment_label, panel_label, fmt_n(m_orig$total_matched_moves[1]), enrollment_label,
    get_orig(orig_after, "pct_of_matched", 0), fmt_n(get_orig(orig_after, "n", 0L)), panel_label,
    get_orig(orig_same, "pct_of_matched", 0), fmt_n(get_orig(orig_same, "n", 0L)),
    get_orig(orig_before, "pct_of_matched", 0), fmt_n(get_orig(orig_before, "n", 0L))
  )

  list(
    paragraph_1_lead_lag   = p1,
    paragraph_2_freshness  = p2,
    paragraph_3_enrollment = p3,
    combined_text          = paste(p1, p2, p3, sep = "\n\n")
  )
}
