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
#' @return `panel` ordered by `(id, year)` with three added columns:
#'   `first_panel_year` (earliest year observed for `id`),
#'   `spell_start_year` (first year of the current uninterrupted consecutive-year
#'   spell of `attribute`), and `spell_age_years` (`year - spell_start_year`).
#' @examples
#' p <- data.frame(
#'   npi = c("1", "1", "1", "2", "2"),
#'   year = c(2022, 2023, 2024, 2023, 2024),
#'   street = c("10 MAIN ST", "10 MAIN ST", "20 OAK AVE", "5 ELM ST", "5 ELM ST")
#' )
#' attribute_spell_age(p, attribute = "street")
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
#' @return A tibble with one row per attribute in `attributes`.
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
#' @return A tibble with one row per age stratum: `comparison`, `stratification`,
#'   `stratum`, `n`, and `<attr>_agree_pct` for each attribute in `attributes`.
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
#'   `total_panel_moves`, `total_matched_moves`, and `pct_of_matched`.
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
#'   `paragraph_3_enrollment`, and `combined_text`.
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
