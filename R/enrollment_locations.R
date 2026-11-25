#' Link individual and group-reassigned enrollment edges to practice locations
#'
#' A physician's practice location can come from an administrative
#' enrollment record two different ways: the physician's own individual
#' enrollment, or a group practice's enrollment that reassigns its billing
#' (and, administratively, its locations) to the individual. Both paths are
#' routine and legitimate, but they are not the same kind of evidence -- a
#' physician linked only through a group's 40-location reassignment has no
#' stronger a claim to seeing patients at any one of those 40 addresses than
#' the group record alone establishes. This function builds the traceable
#' edge-to-location linkage for both paths without ever asserting the
#' stronger claim itself.
#'
#' @details
#' **Group linkage is not clinical-presence evidence.** Every linked row
#' gets `clinical_presence_verified = NA`, always, with no code path that
#' ever sets it otherwise -- this function establishes an administrative
#' association, not confirmation that the physician was ever actually seen
#' at that location. Determining clinical presence is a separate question
#' this function deliberately does not answer.
#'
#' **Resolve active intervals to one snapshot before calling.** Linkage is
#' keyed on `(year, enrollment_id)` alone; if an enrollment or a location
#' carries its own effective-date range, collapse it to the single snapshot
#' year being analyzed first -- this function does not interpret date
#' ranges.
#'
#' **A shared group enrollment fans out, correctly.** If two physicians both
#' reassign to the same group enrollment, and that enrollment lists several
#' locations, every physician links to every one of that group's locations
#' -- each physician-location pair is its own traceable row, not collapsed
#' or deduplicated, because each is independently a legitimate (if purely
#' administrative) association to confirm or rule out.
#'
#' @param individual_enrollments Data frame of direct enrollment edges: `id`,
#'   `year`, `enrollment_id`, one row per edge, unique and non-missing.
#' @param reassignments Data frame of group-reassignment edges, same shape as
#'   `individual_enrollments` -- `enrollment_id` here is the *receiving
#'   group's* enrollment, established by an explicit source link, not an
#'   inference this function draws.
#' @param locations Data frame of practice locations: `year`,
#'   `enrollment_id`, `location_id`, one row per location, unique and
#'   non-missing, plus any address columns to carry through (for example
#'   `practice_zip5`).
#' @param id,year Names of the identifier and year columns on
#'   `individual_enrollments`/`reassignments`. `locations` always uses
#'   `year` under this same name but has no identifier column of its own.
#'   Defaults `"npi"`, `"year"`.
#' @return A list: `links` (one row per enrollment edge x matching location,
#'   left-joined so an edge with no matching location still appears once
#'   with `location_id = NA`: `id`, `year`, `enrollment_id`, `linkage_path`
#'   (`"direct_enrollment"` or `"group_reassignment"`), `location_id`, any
#'   carried-through location columns, `location_linked`,
#'   `clinical_presence_verified` (always `NA`)); `unmatched_enrollments`
#'   (`links` filtered to `!location_linked`); `unlinked_locations` (rows of
#'   `locations` whose `(year, enrollment_id)` matches no edge at all).
#' @examples
#' individual <- data.frame(npi = "1", year = 2024, enrollment_id = "individual_a")
#' reassignments <- data.frame(npi = "1", year = 2024, enrollment_id = "group_a")
#' locations <- data.frame(
#'   year = 2024, enrollment_id = "group_a",
#'   location_id = c("site_a", "site_b"), practice_zip5 = c("80204", "80045")
#' )
#' linkage <- attribute_enrollment_locations(individual, reassignments, locations)
#' linkage$links[, c("enrollment_id", "linkage_path", "location_id", "location_linked")]
#' linkage$unmatched_enrollments # the individual enrollment has no matching location
#' @family no-gold-standard comparison
#' @export
attribute_enrollment_locations <- function(individual_enrollments, reassignments, locations,
                                             id = "npi", year = "year") {
  checkmate::assert_data_frame(individual_enrollments, min.rows = 1)
  checkmate::assert_data_frame(reassignments, min.rows = 1)
  checkmate::assert_data_frame(locations, min.rows = 1)
  checkmate::assert_string(id, min.chars = 1)
  checkmate::assert_string(year, min.chars = 1)

  require_cols <- function(df, cols, label) {
    missing_cols <- setdiff(cols, names(df))
    if (length(missing_cols) > 0) {
      stop(sprintf("attribute_enrollment_locations: %s is missing column(s): %s",
                   label, paste(missing_cols, collapse = ", ")), call. = FALSE)
    }
  }
  edge_cols <- c(id, year, "enrollment_id")
  require_cols(individual_enrollments, edge_cols, "individual_enrollments")
  require_cols(reassignments, edge_cols, "reassignments")
  require_cols(locations, c(year, "enrollment_id", "location_id"), "locations")

  assert_key <- function(df, cols, label) {
    keys <- df[, cols, drop = FALSE]
    blank <- vapply(keys, function(v) any(is.na(v) | !nzchar(trimws(as.character(v)))), logical(1))
    if (any(blank) || anyDuplicated(keys) > 0) {
      stop(sprintf("attribute_enrollment_locations: %s has missing/blank or duplicate keys.",
                   label), call. = FALSE)
    }
  }
  assert_key(individual_enrollments, edge_cols, "individual_enrollments")
  assert_key(reassignments, edge_cols, "reassignments")
  assert_key(locations, c(year, "enrollment_id", "location_id"), "locations")

  edge_frame <- function(df, path) {
    data.frame(
      id = trimws(as.character(df[[id]])), year = df[[year]],
      enrollment_id = trimws(as.character(df$enrollment_id)), linkage_path = path
    )
  }
  edges <- rbind(
    edge_frame(individual_enrollments, "direct_enrollment"),
    edge_frame(reassignments, "group_reassignment")
  )

  extra_cols <- setdiff(names(locations), c(year, "enrollment_id", "location_id"))
  loc <- data.frame(
    year = locations[[year]], enrollment_id = trimws(as.character(locations$enrollment_id)),
    location_id = trimws(as.character(locations$location_id))
  )
  for (col in extra_cols) loc[[col]] <- locations[[col]]

  linked <- merge(edges, loc, by = c("year", "enrollment_id"), all.x = TRUE)
  linked$location_linked <- !is.na(linked$location_id)
  linked$clinical_presence_verified <- NA
  linked <- linked[order(linked$id, linked$year, linked$enrollment_id, linked$location_id), ]
  linked <- tibble::as_tibble(linked[, c("id", "year", "enrollment_id", "linkage_path", "location_id",
                                          extra_cols, "location_linked", "clinical_presence_verified")])

  unmatched <- linked[!linked$location_linked, ]

  edge_key <- paste(edges$year, edges$enrollment_id, sep = "\r")
  loc_key <- paste(loc$year, loc$enrollment_id, sep = "\r")
  unlinked_locations <- tibble::as_tibble(locations[!loc_key %in% edge_key, , drop = FALSE])

  message(sprintf(
    "attribute_enrollment_locations: %d edge(s), %d linked row(s), %d unmatched edge(s), %d unlinked location(s)",
    nrow(edges), sum(linked$location_linked), nrow(unmatched), nrow(unlinked_locations)
  ))

  list(links = linked, unmatched_enrollments = unmatched, unlinked_locations = unlinked_locations)
}

#' Audit join cardinality and duplication in group-reassigned enrollment locations
#'
#' When individual clinicians are linked to practice locations through
#' group-reassignment tables (such as PECOS `ppef_enrol` x `ppef_reassign` x
#' `ppef_addr`), the resulting edge-location table can contain tens or
#' hundreds of thousands of rows. That raw row count mixes two very different
#' phenomena: (1) administrative duplication (multiple individual enrollment
#' IDs for the same clinician, or multiple street/suite rows within the same
#' 5-digit ZIP code) and (2) genuine organizational fan-out (a billing group
#' that operates dozens of clinic ZIP codes across one or more states). This
#' function decomposes the linked rows step by step from raw edge-location
#' rows down to distinct `(id, zip5)`, `(id, state)`, and `(id, enrollment_id)`
#' pairs so a manuscript can report both what was deduplicated and how many
#' unique locations remain per clinician.
#'
#' @param links Data frame returned in `attribute_enrollment_locations()$links`
#'   (or a data frame with `id`, `year`, `enrollment_id`, `linkage_path`,
#'   `location_id`, `location_linked`, and `zip_col`).
#' @param zip_col Name of the 5-digit ZIP column on `links`. Default
#'   `"practice_zip5"`.
#' @param state_col Optional name of the state column on `links`. Default
#'   `"practice_state"`. If absent from `links`, state-level aggregation steps
#'   are skipped.
#' @param city_col Optional name of the city column on `links`. Default
#'   `"practice_city"`. If absent from `links`, the city-state-ZIP step is
#'   skipped.
#' @param linkage_path_filter Character vector of `linkage_path` values to
#'   audit. Default `"group_reassignment"` (the path where organizational
#'   fan-out occurs); pass `c("direct_enrollment", "group_reassignment")` to
#'   pool both paths.
#' @param raw_suite_rows Optional integer count of raw pre-deduplication
#'   address rows (when the caller collapsed suite/floor rows before calling
#'   [attribute_enrollment_locations()]). Default `NULL`.
#' @param reference_state Optional data frame with `id`, `year`, and a state
#'   column (named by `reference_state_col`) representing each clinician's
#'   primary directory state (for example NPPES `practice_state`), used to
#'   compute the share of distinct `(id, zip5)` pairs that lie outside the
#'   clinician's reference state.
#' @param reference_state_col Column name of the state variable in
#'   `reference_state`. Default `"practice_state"`.
#' @return A tibble with one row per aggregation step: `step_order`,
#'   `aggregation_level`, `description`, `n_rows_or_pairs`, `n_clinicians`,
#'   `mean_per_clinician`, `sd_per_clinician`, `median_per_clinician`,
#'   `p25_per_clinician`, `p75_per_clinician`, `max_per_clinician`,
#'   `n_removed_vs_step1`, `pct_removed_vs_step1`, `n_out_of_state_pairs`,
#'   and `pct_out_of_state_pairs`.
#' @examples
#' individual <- data.frame(npi = c("1", "2"), year = 2019, enrollment_id = c("I1", "I2"))
#' reassignments <- data.frame(
#'   npi = c("1", "1", "2"), year = 2019, enrollment_id = c("G1", "G2", "G1")
#' )
#' locations <- data.frame(
#'   year = 2019,
#'   enrollment_id = c("G1", "G1", "G1", "G2"),
#'   location_id = c("L1", "L2", "L3", "L4"),
#'   practice_city = c("DENVER", "DENVER", "BOULDER", "CHEYENNE"),
#'   practice_state = c("CO", "CO", "CO", "WY"),
#'   practice_zip5 = c("80204", "80204", "80301", "82001")
#' )
#' linked <- attribute_enrollment_locations(individual, reassignments, locations)$links
#' attribute_enrollment_cardinality_audit(linked)
#' @seealso [attribute_enrollment_locations()], [attribute_location_specificity()].
#' @family no-gold-standard comparison
#' @export
attribute_enrollment_cardinality_audit <- function(links,
                                                   zip_col = "practice_zip5",
                                                   state_col = "practice_state",
                                                   city_col = "practice_city",
                                                   linkage_path_filter = "group_reassignment",
                                                   raw_suite_rows = NULL,
                                                   reference_state = NULL,
                                                   reference_state_col = "practice_state") {
  checkmate::assert_data_frame(links, min.rows = 1)
  checkmate::assert_string(zip_col, min.chars = 1)
  checkmate::assert_character(linkage_path_filter, min.len = 1, any.missing = FALSE)
  if (!is.null(raw_suite_rows)) {
    checkmate::assert_number(raw_suite_rows, lower = 1, finite = TRUE)
  }

  req <- c("id", "year", "enrollment_id", "linkage_path", "location_id", "location_linked", zip_col)
  miss <- setdiff(req, names(links))
  if (length(miss) > 0L) {
    stop(sprintf(
      "attribute_enrollment_cardinality_audit: links is missing column(s): %s",
      paste(miss, collapse = ", ")
    ), call. = FALSE)
  }

  sub <- links[links$location_linked & links$linkage_path %in% linkage_path_filter, , drop = FALSE]
  if (nrow(sub) == 0L) {
    stop("attribute_enrollment_cardinality_audit: no linked rows match linkage_path_filter.", call. = FALSE)
  }

  sub$id <- trimws(as.character(sub$id))
  sub$enrollment_id <- trimws(as.character(sub$enrollment_id))
  sub$location_id <- trimws(as.character(sub$location_id))
  sub$.zip5 <- trimws(as.character(sub[[zip_col]]))
  sub$.zip5[!nzchar(sub$.zip5)] <- NA_character_

  has_state <- !is.null(state_col) && state_col %in% names(sub)
  if (has_state) {
    sub$.state <- toupper(trimws(as.character(sub[[state_col]])))
    sub$.state[!nzchar(sub$.state)] <- NA_character_
  }
  has_city <- !is.null(city_col) && city_col %in% names(sub)
  if (has_city) {
    sub$.city <- toupper(trimws(as.character(sub[[city_col]])))
    sub$.city[!nzchar(sub$.city)] <- NA_character_
  }

  n_clin_total <- length(unique(paste(sub$id, sub$year, sep = "\r")))
  step1_n <- if (!is.null(raw_suite_rows)) as.integer(round(raw_suite_rows)) else nrow(sub)

  summarize_counts <- function(counts_vec, step_ord, level_name, desc,
                               out_n = NA_integer_, out_pct = NA_real_) {
    n_tot <- sum(counts_vec)
    n_clin <- length(counts_vec)
    qs <- stats::quantile(counts_vec, probs = c(0.25, 0.75), type = 7, names = FALSE)
    tibble::tibble(
      step_order = as.integer(step_ord),
      aggregation_level = level_name,
      description = desc,
      n_rows_or_pairs = as.integer(n_tot),
      n_clinicians = as.integer(n_clin),
      mean_per_clinician = round(mean(counts_vec), 2),
      sd_per_clinician = if (n_clin >= 2L) round(stats::sd(counts_vec), 2) else NA_real_,
      median_per_clinician = as.numeric(stats::median(counts_vec)),
      p25_per_clinician = as.numeric(qs[1]),
      p75_per_clinician = as.numeric(qs[2]),
      max_per_clinician = as.integer(max(counts_vec)),
      n_removed_vs_step1 = as.integer(step1_n - n_tot),
      pct_removed_vs_step1 = if (step1_n > 0L) round(100 * (step1_n - n_tot) / step1_n, 2) else NA_real_,
      n_out_of_state_pairs = as.integer(out_n),
      pct_out_of_state_pairs = out_pct
    )
  }

  rows_out <- list()
  ord <- 1L

  # Optional pre-deduplication raw suite/street row count
  if (!is.null(raw_suite_rows)) {
    scale_factor <- raw_suite_rows / nrow(sub)
    id_yr_edge <- paste(sub$id, sub$year, sep = "\r")
    approx_counts <- as.numeric(table(id_yr_edge)) * scale_factor
    rows_out[[length(rows_out) + 1L]] <- summarize_counts(
      approx_counts, ord, "raw_pre_dedup_address_rows",
      "Raw relational join including duplicate suite/floor/street rows prior to location deduplication"
    )
    rows_out[[length(rows_out)]]$n_rows_or_pairs <- as.integer(round(raw_suite_rows))
    rows_out[[length(rows_out)]]$n_removed_vs_step1 <- 0L
    rows_out[[length(rows_out)]]$pct_removed_vs_step1 <- 0
    ord <- ord + 1L
  }

  # Step: linked edge x location rows in `links`
  s_edge <- unique(sub[, c("id", "year", "enrollment_id", "location_id", ".zip5"), drop = FALSE])
  c_edge <- as.integer(table(paste(s_edge$id, s_edge$year, sep = "\r")))
  rows_out[[length(rows_out) + 1L]] <- summarize_counts(
    c_edge, ord, "linked_edge_location_rows",
    "Distinct (id, year, enrollment_id, location_id) rows returned by attribute_enrollment_locations()"
  )
  ord <- ord + 1L

  # Step: distinct (id, year, enrollment_id, zip5)
  s_grp_zip <- unique(sub[!is.na(sub$.zip5), c("id", "year", "enrollment_id", ".zip5"), drop = FALSE])
  c_grp_zip <- as.integer(table(paste(s_grp_zip$id, s_grp_zip$year, sep = "\r")))
  rows_out[[length(rows_out) + 1L]] <- summarize_counts(
    c_grp_zip, ord, "distinct_id_group_zip5",
    "Distinct (id, year, enrollment_id, zip5) clinician-group-ZIP5 combinations"
  )
  ord <- ord + 1L

  # Step: distinct (id, year, city, state, zip5) if city and state present
  if (has_city && has_state) {
    s_csz <- unique(sub[!is.na(sub$.zip5), c("id", "year", ".city", ".state", ".zip5"), drop = FALSE])
    c_csz <- as.integer(table(paste(s_csz$id, s_csz$year, sep = "\r")))
    rows_out[[length(rows_out) + 1L]] <- summarize_counts(
      c_csz, ord, "distinct_id_city_state_zip5",
      "Distinct (id, year, city, state, zip5) across all billing organizations"
    )
    ord <- ord + 1L
  }

  # Step: distinct (id, year, zip5) + optional out-of-state check
  cols_zip <- if (has_state) c("id", "year", ".zip5", ".state") else c("id", "year", ".zip5")
  s_zip_full <- unique(sub[!is.na(sub$.zip5), cols_zip, drop = FALSE])
  s_zip <- unique(s_zip_full[, c("id", "year", ".zip5"), drop = FALSE])
  c_zip <- as.integer(table(paste(s_zip$id, s_zip$year, sep = "\r")))

  out_n <- NA_integer_
  out_pct <- NA_real_
  if (!is.null(reference_state) && has_state) {
    checkmate::assert_data_frame(reference_state, min.rows = 1)
    if (!all(c("id", "year", reference_state_col) %in% names(reference_state))) {
      stop(sprintf(
        "attribute_enrollment_cardinality_audit: reference_state must contain 'id', 'year', and '%s'.",
        reference_state_col
      ), call. = FALSE)
    }
    ref_df <- data.frame(
      id = trimws(as.character(reference_state$id)),
      year = reference_state$year,
      .ref_state = toupper(trimws(as.character(reference_state[[reference_state_col]])))
    )
    ref_df <- unique(ref_df[!is.na(ref_df$.ref_state) & nzchar(ref_df$.ref_state), , drop = FALSE])
    m_ref <- merge(s_zip_full, ref_df, by = c("id", "year"))
    if (nrow(m_ref) > 0L) {
      # Collapse to unique (id, year, .zip5)
      by_pair <- split(m_ref$.state != m_ref$.ref_state, paste(m_ref$id, m_ref$year, m_ref$.zip5, sep = "\r"))
      is_oos <- vapply(by_pair, all, logical(1))
      out_n <- as.integer(sum(is_oos, na.rm = TRUE))
      out_pct <- round(100 * out_n / length(is_oos), 2)
    }
  }

  rows_out[[length(rows_out) + 1L]] <- summarize_counts(
    c_zip, ord, "distinct_id_zip5",
    "Distinct (id, year, zip5) unique 5-digit ZIP codes associated with each clinician",
    out_n = out_n, out_pct = out_pct
  )
  ord <- ord + 1L

  # Step: distinct (id, year, state)
  if (has_state) {
    s_st <- unique(sub[!is.na(sub$.state), c("id", "year", ".state"), drop = FALSE])
    c_st <- as.integer(table(paste(s_st$id, s_st$year, sep = "\r")))
    rows_out[[length(rows_out) + 1L]] <- summarize_counts(
      c_st, ord, "distinct_id_state",
      "Distinct (id, year, state) unique states associated with each clinician"
    )
    ord <- ord + 1L
  }

  # Step: distinct (id, year, enrollment_id)
  s_grp <- unique(sub[, c("id", "year", "enrollment_id"), drop = FALSE])
  c_grp <- as.integer(table(paste(s_grp$id, s_grp$year, sep = "\r")))
  rows_out[[length(rows_out) + 1L]] <- summarize_counts(
    c_grp, ord, "distinct_id_group_enrollment",
    "Distinct (id, year, enrollment_id) clinician-billing organization relationships"
  )

  dplyr::bind_rows(rows_out)
}

#' Classify enrolled clinicians into a 6-tier location-specificity hierarchy and 4-stage evidence funnel
#'
#' Following group-reassignment links in Medicare PECOS recovers associated
#' practice addresses for nearly all group-billing clinicians, but those
#' recovered addresses range from a single clinic ZIP code to a 50-site
#' hospital or telehealth network spanning multiple states. Without using a
#' secondary directory to cherry-pick a winner, this function classifies every
#' enrolled clinician into one of six mutually exclusive location-specificity
#' tiers based solely on the enrollment linkage itself, and then optionally
#' evaluates each tier against one or more external corroboration directories
#' (for example NPPES or CMS Physician Compare / Doctors and Clinicians) to
#' build a four-stage evidence funnel from enrollment to corroborated clinical
#' site.
#'
#' @details
#' **The six mutually exclusive specificity tiers:**
#' \enumerate{
#'   \item `"Tier 1: Direct Individual Location"` -- Clinician has at least one
#'     practice location linked directly to their individual enrollment record
#'     (`linkage_path == "direct_enrollment"`).
#'   \item `"Tier 2: Unique Group Location (1 ZIP5, 1 State)"` -- No direct
#'     individual location; billing-group reassignment links to exactly 1
#'     unique 5-digit ZIP code in 1 state.
#'   \item `"Tier 3: Small Intrastate Group (2-5 ZIP5s, 1 State)"` -- Group
#'     reassignment links to 2--5 unique 5-digit ZIP codes within a single
#'     state.
#'   \item `"Tier 4: Large Intrastate Group (>5 ZIP5s, 1 State)"` -- Group
#'     reassignment links to 6 or more unique 5-digit ZIP codes within a
#'     single state.
#'   \item `"Tier 5: Multi-State Billing Group (>=2 States)"` -- Group
#'     reassignment links to practice locations spanning 2 or more states.
#'   \item `"Tier 6: Unlinked"` -- Clinician has no linked direct or
#'     group-reassigned location (`!location_linked` or all ZIP5s missing).
#' }
#'
#' **Pinpoint vs. multi-site attribution.** Tiers 1 and 2 (when Tier 1 also has
#' 1 unique ZIP5) identify a single pinpoint practice ZIP5 from the enrollment
#' registry alone (`is_pinpoint_site == TRUE`). Tiers 3--5 identify a candidate
#' set of organizational locations that requires an independent corroborating
#' source to narrow to the clinician's specific work site.
#'
#' @param links Data frame returned in `attribute_enrollment_locations()$links`.
#' @param zip_col Name of the 5-digit ZIP column on `links`. Default
#'   `"practice_zip5"`.
#' @param state_col Name of the state column on `links`. Default
#'   `"practice_state"`.
#' @param corroboration_sources Optional named list of external reference data
#'   frames (for example `list(nppes = df_nppes, physician_compare = df_pc)`),
#'   each containing `id`, `year`, and `zip_col` (plus optional `state_col` or
#'   a logical `hospital_affiliated` column).
#' @param provider_plural Plural noun used in `plain_english_summary` (for
#'   example `"nurse-midwives"` or `"physicians"`). Default `"clinicians"`.
#' @param conf_level Confidence level for Wilson intervals via [wilson_ci()].
#'   Default `0.95`.
#' @return A list of class `"mysteryroster_location_specificity"`:
#'   \describe{
#'     \item{`clinicians`}{Tibble with one row per `(id, year)` in `links`:
#'       `id`, `year`, `has_direct`, `has_group`, `location_linked`,
#'       `n_edge_location_rows`, `n_unique_groups`, `n_unique_zip5`,
#'       `n_unique_states`, `specificity_tier` (ordered factor),
#'       `is_pinpoint_site` (logical), plus corroboration flags when
#'       `corroboration_sources` is supplied (`unambiguous_pinpoint_match`,
#'       `any_source_corroborated`, `all_sources_corroborated`, and per-source
#'       `<source>_present` / `<source>_zip5_match`).}
#'     \item{`tiers`}{Tibble with one row per specificity tier (plus a `"Total"`
#'       row): `specificity_tier`, `n_clinicians`, `pct_of_cohort`,
#'       `median_unique_zip5`, `p25_unique_zip5`, `p75_unique_zip5`,
#'       `mean_unique_zip5`, `max_unique_zip5`, and corroboration counts,
#'       proportions, Wilson CIs, and sensitivity bounds.}
#'     \item{`funnel`}{Tibble of the 4-stage evidence funnel (`stage_order`,
#'       `stage_code`, `stage_label`, `n_clinicians`, `pct_of_cohort`,
#'       `plain_english_note`).}
#'     \item{`plain_english_summary`}{Single manuscript-ready paragraph.}
#'   }
#' @examples
#' individual <- data.frame(npi = c("1", "2", "3", "4"), year = 2019,
#'                          enrollment_id = c("I1", "I2", "I3", "I4"))
#' reassignments <- data.frame(
#'   npi = c("2", "3", "3"), year = 2019, enrollment_id = c("G1", "G2", "G3")
#' )
#' locations <- data.frame(
#'   year = 2019,
#'   enrollment_id = c("I1", "G1", "G2", "G3"),
#'   location_id = c("L1", "L2", "L3", "L4"),
#'   practice_state = c("CO", "CO", "CO", "WY"),
#'   practice_zip5 = c("80204", "80301", "80204", "82001")
#' )
#' linked <- attribute_enrollment_locations(individual, reassignments, locations)$links
#' ref <- list(nppes = data.frame(
#'   id = c("1", "2", "3"), year = 2019, practice_zip5 = c("80204", "80301", "80204")
#' ))
#' spec <- attribute_location_specificity(linked, corroboration_sources = ref,
#'                                        provider_plural = "nurse-midwives")
#' spec$tiers[, c("specificity_tier", "n_clinicians", "pct_of_cohort")]
#' spec$funnel[, c("stage_order", "stage_label", "n_clinicians", "pct_of_cohort")]
#' spec$plain_english_summary
#' @seealso [attribute_enrollment_locations()], [attribute_enrollment_cardinality_audit()].
#' @family no-gold-standard comparison
#' @export
attribute_location_specificity <- function(links,
                                           zip_col = "practice_zip5",
                                           state_col = "practice_state",
                                           corroboration_sources = NULL,
                                           provider_plural = "clinicians",
                                           conf_level = 0.95) {
  checkmate::assert_data_frame(links, min.rows = 1)
  checkmate::assert_string(zip_col, min.chars = 1)
  checkmate::assert_string(state_col, min.chars = 1)
  checkmate::assert_string(provider_plural, min.chars = 1)
  checkmate::assert_number(conf_level, lower = 0, upper = 1)
  if (conf_level <= 0 || conf_level >= 1) {
    stop("attribute_location_specificity: conf_level must be strictly between 0 and 1.", call. = FALSE)
  }

  req <- c("id", "year", "enrollment_id", "linkage_path", "location_id", "location_linked", zip_col, state_col)
  miss <- setdiff(req, names(links))
  if (length(miss) > 0L) {
    stop(sprintf(
      "attribute_location_specificity: links is missing column(s): %s",
      paste(miss, collapse = ", ")
    ), call. = FALSE)
  }

  df <- links
  df$id <- trimws(as.character(df$id))
  df$enrollment_id <- trimws(as.character(df$enrollment_id))
  df$.zip5 <- trimws(as.character(df[[zip_col]]))
  df$.zip5[!nzchar(df$.zip5)] <- NA_character_
  df$.state <- toupper(trimws(as.character(df[[state_col]])))
  df$.state[!nzchar(df$.state)] <- NA_character_

  tier_levels <- c(
    "Tier 1: Direct Individual Location",
    "Tier 2: Unique Group Location (1 ZIP5, 1 State)",
    "Tier 3: Small Intrastate Group (2-5 ZIP5s, 1 State)",
    "Tier 4: Large Intrastate Group (>5 ZIP5s, 1 State)",
    "Tier 5: Multi-State Billing Group (>=2 States)",
    "Tier 6: Unlinked"
  )

  by_clin <- split(df, paste(df$id, df$year, sep = "\r"), drop = TRUE)
  clin_list <- lapply(by_clin, function(g) {
    lnk <- g[g$location_linked & !is.na(g$.zip5), , drop = FALSE]
    has_dir <- any(lnk$linkage_path == "direct_enrollment")
    has_grp <- any(lnk$linkage_path == "group_reassignment")
    zips <- unique(lnk$.zip5)
    states <- unique(lnk$.state[!is.na(lnk$.state)])
    grps <- unique(lnk$enrollment_id[lnk$linkage_path == "group_reassignment"])
    n_z <- length(zips)
    n_s <- length(states)

    tier <- if (n_z == 0L) {
      tier_levels[6]
    } else if (has_dir) {
      tier_levels[1]
    } else if (n_s >= 2L) {
      tier_levels[5]
    } else if (n_z == 1L) {
      tier_levels[2]
    } else if (n_z <= 5L) {
      tier_levels[3]
    } else {
      tier_levels[4]
    }

    # For Tier 1, check how many direct ZIPs exist for pinpoint status
    dir_zips <- unique(lnk$.zip5[lnk$linkage_path == "direct_enrollment"])
    is_pinpoint <- (tier == tier_levels[1] && length(dir_zips) == 1L) || (tier == tier_levels[2])

    tibble::tibble(
      id = g$id[1],
      year = g$year[1],
      has_direct = has_dir,
      has_group = has_grp,
      location_linked = n_z > 0L,
      n_edge_location_rows = nrow(lnk),
      n_unique_groups = length(grps),
      n_unique_zip5 = n_z,
      n_unique_states = n_s,
      .candidate_zips = paste(sort(zips), collapse = "|"),
      .direct_zips = paste(sort(dir_zips), collapse = "|"),
      specificity_tier = factor(tier, levels = tier_levels, ordered = TRUE),
      is_pinpoint_site = is_pinpoint
    )
  })
  clin <- dplyr::bind_rows(clin_list)
  clin <- clin[order(clin$id, clin$year), , drop = FALSE]

  src_names <- character(0)
  if (!is.null(corroboration_sources)) {
    checkmate::assert_list(corroboration_sources, min.len = 1, names = "unique")
    src_names <- names(corroboration_sources)

    for (sname in src_names) {
      sdf <- corroboration_sources[[sname]]
      checkmate::assert_data_frame(sdf, min.rows = 1)
      id_c <- if ("id" %in% names(sdf)) "id" else if ("npi" %in% names(sdf)) "npi" else NULL
      if (is.null(id_c) || !all(c("year", zip_col) %in% names(sdf))) {
        stop(sprintf(
          "attribute_location_specificity: corroboration source '%s' must contain id/npi, year, and '%s'.",
          sname, zip_col
        ), call. = FALSE)
      }
      s_clean <- data.frame(
        id = trimws(as.character(sdf[[id_c]])),
        year = sdf$year,
        .z = trimws(as.character(sdf[[zip_col]]))
      )
      s_clean <- s_clean[!is.na(s_clean$.z) & nzchar(s_clean$.z), , drop = FALSE]
      by_s <- split(s_clean$.z, paste(s_clean$id, s_clean$year, sep = "\r"))
      s_map <- vapply(by_s, function(v) paste(sort(unique(v)), collapse = "|"), character(1))

      ckey <- paste(clin$id, clin$year, sep = "\r")
      ref_zips_str <- unname(s_map[ckey])
      present_flag <- !is.na(ref_zips_str) & nzchar(ref_zips_str)

      match_flag <- vapply(seq_len(nrow(clin)), function(i) {
        if (!present_flag[i] || !clin$location_linked[i]) return(FALSE)
        cz <- strsplit(clin$.candidate_zips[i], "\\|")[[1]]
        rz <- strsplit(ref_zips_str[i], "\\|")[[1]]
        length(intersect(cz, rz)) > 0L
      }, logical(1))

      pinpoint_1to1 <- vapply(seq_len(nrow(clin)), function(i) {
        if (!present_flag[i] || !clin$is_pinpoint_site[i]) return(FALSE)
        target_z <- if (clin$has_direct[i]) {
          strsplit(clin$.direct_zips[i], "\\|")[[1]]
        } else {
          strsplit(clin$.candidate_zips[i], "\\|")[[1]]
        }
        rz <- strsplit(ref_zips_str[i], "\\|")[[1]]
        length(target_z) == 1L && target_z %in% rz
      }, logical(1))

      clin[[paste0(sname, "_present")]] <- present_flag
      clin[[paste0(sname, "_zip5_match")]] <- match_flag
      clin[[paste0(sname, "_pinpoint_1to1")]] <- pinpoint_1to1
    }

    match_mat <- as.matrix(clin[, paste0(src_names, "_zip5_match"), drop = FALSE])
    pres_mat <- as.matrix(clin[, paste0(src_names, "_present"), drop = FALSE])
    pin_mat <- as.matrix(clin[, paste0(src_names, "_pinpoint_1to1"), drop = FALSE])

    clin$any_source_present <- rowSums(pres_mat) > 0L
    clin$any_source_corroborated <- rowSums(match_mat) > 0L
    clin$all_sources_present <- rowSums(pres_mat) == length(src_names)
    clin$all_sources_corroborated <- rowSums(match_mat) == length(src_names)
    clin$unambiguous_pinpoint_match <- clin$is_pinpoint_site & (rowSums(pin_mat) > 0L)
  } else {
    clin$any_source_present <- FALSE
    clin$any_source_corroborated <- NA
    clin$all_sources_present <- FALSE
    clin$all_sources_corroborated <- NA
    clin$unambiguous_pinpoint_match <- NA
  }

  n_total <- nrow(clin)
  summarize_tier_block <- function(sub_df, label) {
    n_c <- nrow(sub_df)
    if (n_c == 0L) {
      return(tibble::tibble(
        specificity_tier = label,
        n_clinicians = 0L,
        pct_of_cohort = 0,
        median_unique_zip5 = NA_real_,
        p25_unique_zip5 = NA_real_,
        p75_unique_zip5 = NA_real_,
        mean_unique_zip5 = NA_real_,
        max_unique_zip5 = 0L,
        n_pinpoint_1to1 = 0L,
        pct_pinpoint_1to1 = NA_real_,
        n_any_corroborated = 0L,
        pct_any_corroborated_among_present = NA_real_,
        ci_low_corroborated = NA_real_,
        ci_high_corroborated = NA_real_,
        n_all_sources_corroborated = 0L,
        pct_all_sources_corroborated = NA_real_,
        worst_case_corroborated_pct = NA_real_,
        best_case_corroborated_pct = NA_real_
      ))
    }
    qs <- stats::quantile(sub_df$n_unique_zip5, probs = c(0.25, 0.75), type = 7, names = FALSE)
    n_pres <- sum(sub_df$any_source_present, na.rm = TRUE)
    n_any_corr <- sum(sub_df$any_source_corroborated, na.rm = TRUE)
    n_all_corr <- sum(sub_df$all_sources_corroborated, na.rm = TRUE)
    n_pin_corr <- sum(sub_df$unambiguous_pinpoint_match, na.rm = TRUE)
    n_unres <- n_c - n_pres

    if (!is.null(corroboration_sources) && n_pres > 0L) {
      ci <- wilson_ci(n_any_corr, n_pres, conf_level = conf_level, as_percent = TRUE)
      pct_pres <- round(100 * n_any_corr / n_pres, 2)
      ci_l <- round(ci$ci_low, 2)
      ci_h <- round(ci$ci_high, 2)
      worst_pct <- round(100 * n_any_corr / n_c, 2)
      best_pct <- round(100 * (n_any_corr + n_unres) / n_c, 2)
      pct_all <- round(100 * n_all_corr / n_c, 2)
      pct_pin <- round(100 * n_pin_corr / n_c, 2)
    } else {
      pct_pres <- NA_real_; ci_l <- NA_real_; ci_h <- NA_real_
      worst_pct <- NA_real_; best_pct <- NA_real_; pct_all <- NA_real_; pct_pin <- NA_real_
    }

    tibble::tibble(
      specificity_tier = label,
      n_clinicians = as.integer(n_c),
      pct_of_cohort = round(100 * n_c / n_total, 2),
      median_unique_zip5 = as.numeric(stats::median(sub_df$n_unique_zip5)),
      p25_unique_zip5 = as.numeric(qs[1]),
      p75_unique_zip5 = as.numeric(qs[2]),
      mean_unique_zip5 = round(mean(sub_df$n_unique_zip5), 2),
      max_unique_zip5 = as.integer(max(sub_df$n_unique_zip5)),
      n_pinpoint_1to1 = as.integer(n_pin_corr),
      pct_pinpoint_1to1 = pct_pin,
      n_any_corroborated = as.integer(n_any_corr),
      pct_any_corroborated_among_present = pct_pres,
      ci_low_corroborated = ci_l,
      ci_high_corroborated = ci_h,
      n_all_sources_corroborated = as.integer(n_all_corr),
      pct_all_sources_corroborated = pct_all,
      worst_case_corroborated_pct = worst_pct,
      best_case_corroborated_pct = best_pct
    )
  }

  tier_rows <- lapply(tier_levels, function(tl) {
    summarize_tier_block(clin[clin$specificity_tier == tl, , drop = FALSE], tl)
  })
  tiers_tbl <- dplyr::bind_rows(tier_rows, summarize_tier_block(clin, "Total"))

  # Build 4-Stage Evidence Funnel
  n_linked <- sum(clin$location_linked)
  n_direct <- sum(clin$has_direct & clin$location_linked)
  n_grp_only <- n_linked - n_direct
  n_pinpoint <- sum(clin$is_pinpoint_site)
  n_large_fanout <- sum(clin$specificity_tier %in% tier_levels[4:5])
  grp_linked_df <- clin[clin$has_group & clin$location_linked, , drop = FALSE]
  mean_grp_zip <- if (nrow(grp_linked_df) > 0L) round(mean(grp_linked_df$n_unique_zip5), 1) else 0
  med_grp_zip <- if (nrow(grp_linked_df) > 0L) stats::median(grp_linked_df$n_unique_zip5) else 0

  funnel_rows <- list(
    tibble::tibble(
      stage_order = 1L,
      stage_code = "enrolled_cohort",
      stage_label = "Stage 1: Enrolled Cohort in Administrative Registry",
      n_clinicians = as.integer(n_total),
      pct_of_cohort = 100.0,
      plain_english_note = sprintf("All %s enrolled %s in the target cohort.", format(n_total, big.mark = ","), provider_plural)
    ),
    tibble::tibble(
      stage_order = 2L,
      stage_code = "associated_location_linked",
      stage_label = "Stage 2: Linked to >=1 Associated Location (Direct or Billing Group)",
      n_clinicians = as.integer(n_linked),
      pct_of_cohort = round(100 * n_linked / n_total, 2),
      plain_english_note = sprintf(
        "%s (%.1f%%) had a directly linked location; %s (%.1f%%) were linked only through billing organizations.",
        format(n_direct, big.mark = ","), 100 * n_direct / n_total,
        format(n_grp_only, big.mark = ","), 100 * n_grp_only / n_total
      )
    ),
    tibble::tibble(
      stage_order = 3L,
      stage_code = "pinpoint_single_zip5_site",
      stage_label = "Stage 3: Linked to a Single Pinpoint Practice ZIP5 (1 ZIP5 in 1 State)",
      n_clinicians = as.integer(n_pinpoint),
      pct_of_cohort = round(100 * n_pinpoint / n_total, 2),
      plain_english_note = sprintf(
        "Only %s (%.1f%%) mapped to a single pinpoint ZIP5, whereas %s (%.1f%%) fanned out across large multi-site or multi-state billing organizations (mean %.1f, median %s unique ZIP5s).",
        format(n_pinpoint, big.mark = ","), 100 * n_pinpoint / n_total,
        format(n_large_fanout, big.mark = ","), 100 * n_large_fanout / n_total,
        mean_grp_zip, as.character(med_grp_zip)
      )
    )
  )

  if (!is.null(corroboration_sources)) {
    n_pin_corr <- sum(clin$unambiguous_pinpoint_match, na.rm = TRUE)
    n_all_corr <- sum(clin$all_sources_corroborated, na.rm = TRUE)
    funnel_rows[[4L]] <- tibble::tibble(
      stage_order = 4L,
      stage_code = "independently_corroborated_site",
      stage_label = "Stage 4: Independently Corroborated Practice Location",
      n_clinicians = as.integer(n_pin_corr),
      pct_of_cohort = round(100 * n_pin_corr / n_total, 2),
      plain_english_note = sprintf(
        "%s (%.1f%% of cohort; %.1f%% of pinpoint sites) had a corroborated pinpoint ZIP5, and %s (%.1f%%) had a practice ZIP5 corroborated across all supplied reference sources.",
        format(n_pin_corr, big.mark = ","), 100 * n_pin_corr / n_total,
        if (n_pinpoint > 0L) 100 * n_pin_corr / n_pinpoint else 0,
        format(n_all_corr, big.mark = ","), 100 * n_all_corr / n_total
      )
    )
  }
  funnel_tbl <- dplyr::bind_rows(funnel_rows)

  summary_sent <- sprintf(
    paste0(
      "Practice-location information was directly linked to %.1f%% of %s (%s of %s), ",
      "whereas linking clinicians to their billing organizations identified associated locations for %.1f%% (%s of %s). ",
      "However, these organizational addresses were not independently verified as the clinicians' actual practice sites: ",
      "only %.1f%% (%s of %s) mapped to a single pinpoint practice ZIP code, while %.1f%% (%s of %s) were linked ",
      "through large multi-site or multi-state billing organizations (mean %.1f, median %s unique ZIP codes per group-linked clinician)."
    ),
    100 * n_direct / n_total, provider_plural,
    format(n_direct, big.mark = ","), format(n_total, big.mark = ","),
    100 * n_linked / n_total,
    format(n_linked, big.mark = ","), format(n_total, big.mark = ","),
    100 * n_pinpoint / n_total,
    format(n_pinpoint, big.mark = ","), format(n_total, big.mark = ","),
    100 * n_large_fanout / n_total,
    format(n_large_fanout, big.mark = ","), format(n_total, big.mark = ","),
    mean_grp_zip, as.character(med_grp_zip)
  )
  if (!is.null(corroboration_sources)) {
    n_pin_corr <- sum(clin$unambiguous_pinpoint_match, na.rm = TRUE)
    n_all_corr <- sum(clin$all_sources_corroborated, na.rm = TRUE)
    summary_sent <- paste(
      summary_sent,
      sprintf(
        "External sources corroborated a pinpoint single-site ZIP code for %s %s (%.1f%% of cohort; %.1f%% of pinpoint sites), and corroborated a practice ZIP code across all reference sources for %s (%.1f%%); cross-source agreement among administrative registries is not independent clinical verification.",
        format(n_pin_corr, big.mark = ","), provider_plural,
        100 * n_pin_corr / n_total,
        if (n_pinpoint > 0L) 100 * n_pin_corr / n_pinpoint else 0,
        format(n_all_corr, big.mark = ","), 100 * n_all_corr / n_total
      )
    )
  }

  clin$.candidate_zips <- NULL
  clin$.direct_zips <- NULL

  structure(
    list(
      clinicians = tibble::as_tibble(clin),
      tiers = tibble::as_tibble(tiers_tbl),
      funnel = tibble::as_tibble(funnel_tbl),
      plain_english_summary = summary_sent
    ),
    class = "mysteryroster_location_specificity"
  )
}

#' Organization-level billing-group network topology and Shannon location entropy
#'
#' While [attribute_location_specificity()] evaluates group reassignment fan-out
#' from the *clinician's* perspective, health-services researchers also need to
#' audit the **billing organizations (`enrollment_id` / PAC IDs) themselves**:
#' distinguishing a solo single-clinic group from a local multi-clinic OB/GYN
#' group, a regional hospital system, or a national multi-state staffing/telehealth
#' aggregator.
#'
#' For every group-reassigned `enrollment_id` in [attribute_enrollment_locations()]'s
#' `links` table, this function computes the number of linked clinicians,
#' unique locations, unique 5-digit ZIP codes (`n_unique_zip5`), unique 3-digit
#' SCF prefixes (`n_unique_scf3`), unique states (`n_unique_states`), **Shannon
#' location entropy** $H(g) = -\sum_z p_z \ln(p_z)$, the **effective number of
#' locations** $N_{\text{eff}}(g) = \exp(H(g))$ (Hill number of order 1), and an
#' inverse-fanout **location specificity weight** $w_{\text{info}}(g) = 1 / N_{\text{eff}}(g) \in (0, 1]$
#' that down-weights massive multi-site billing umbrellas when estimating
#' spatial practice footprints.
#'
#' @param links Data frame returned in `attribute_enrollment_locations(...)$links`,
#'   containing `id`, `year`, `enrollment_id`, `linkage_path`, `location_id`,
#'   `location_linked`, `zip_col`, and `state_col`.
#' @param zip_col Name of the 5-digit ZIP column in `links`. Default
#'   `"practice_zip5"`.
#' @param state_col Name of the state column in `links`. Default
#'   `"practice_state"`.
#' @param linkage_path_filter Character vector of `linkage_path` values to
#'   include. Default `"group_reassignment"`.
#' @return A list with three tibbles:
#'   \describe{
#'     \item{`groups`}{One row per `(enrollment_id, year)` with `n_clinicians`,
#'       `n_locations`, `n_unique_zip5`, `n_unique_scf3`, `n_unique_states`,
#'       `clinician_to_zip_ratio`, `shannon_entropy_nats`, `effective_n_zip5`,
#'       `location_specificity_weight`, and structural `group_archetype`.}
#'     \item{`archetype_summary`}{Summary across the 5 mutually exclusive
#'       organizational archetypes reporting `n_groups`, `pct_of_groups`,
#'       `n_clinician_links`, `pct_of_clinician_links`, and `median_unique_zip5`.}
#'     \item{`clinician_weights`}{One row per linked `(id, year)` reporting the
#'       clinician's `n_groups`, `max_group_specificity_weight`,
#'       `mean_group_specificity_weight`, and `best_group_archetype` (for
#'       filtering or weighting spatial access analyses).}
#'   }
#' @examples
#' individual <- data.frame(npi = c("1", "2", "3"), year = 2019, enrollment_id = c("I1", "I2", "I3"))
#' reassignments <- data.frame(
#'   npi = c("1", "2", "3"), year = 2019, enrollment_id = c("G_SOLO", "G_LOCAL", "G_MULTI")
#' )
#' locations <- data.frame(
#'   year = 2019,
#'   enrollment_id = c("G_SOLO", "G_LOCAL", "G_LOCAL", "G_MULTI", "G_MULTI"),
#'   location_id = c("L1", "L2", "L3", "L4", "L5"),
#'   practice_state = c("CO", "CO", "CO", "CO", "WY"),
#'   practice_zip5 = c("80204", "80204", "80206", "80204", "82001")
#' )
#' linked <- attribute_enrollment_locations(individual, reassignments, locations)$links
#' topo <- attribute_billing_group_topology(linked)
#' topo$groups[, c("enrollment_id", "n_unique_zip5", "effective_n_zip5", "group_archetype")]
#' topo$archetype_summary
#' @seealso [attribute_enrollment_locations()], [attribute_location_specificity()],
#'   [attribute_enrollment_cardinality_audit()].
#' @family no-gold-standard comparison
#' @export
attribute_billing_group_topology <- function(links,
                                             zip_col = "practice_zip5",
                                             state_col = "practice_state",
                                             linkage_path_filter = "group_reassignment") {
  checkmate::assert_data_frame(links, min.rows = 1)
  checkmate::assert_string(zip_col, min.chars = 1)
  checkmate::assert_string(state_col, min.chars = 1)
  checkmate::assert_character(linkage_path_filter, min.len = 1, any.missing = FALSE)

  req <- c("id", "year", "enrollment_id", "linkage_path", "location_id", "location_linked", zip_col, state_col)
  miss <- setdiff(req, names(links))
  if (length(miss) > 0L) {
    stop(sprintf("attribute_billing_group_topology: links is missing column(s): %s.",
                 paste(miss, collapse = ", ")), call. = FALSE)
  }

  sub <- links[links$location_linked & (links$linkage_path %in% linkage_path_filter), , drop = FALSE]
  sub$id <- trimws(as.character(sub$id))
  sub$enrollment_id <- trimws(as.character(sub$enrollment_id))
  sub$location_id <- trimws(as.character(sub$location_id))
  sub$.zip5 <- substr(gsub("[^0-9]", "", trimws(as.character(sub[[zip_col]]))), 1L, 5L)
  sub$.zip5[nchar(sub$.zip5) < 5L] <- NA_character_
  sub$.state <- toupper(trimws(as.character(sub[[state_col]])))
  sub$.state[!nzchar(sub$.state)] <- NA_character_
  sub <- sub[!is.na(sub$.zip5), , drop = FALSE]

  if (nrow(sub) == 0L) {
    stop("attribute_billing_group_topology: no linked rows with valid 5-digit ZIP codes match linkage_path_filter.", call. = FALSE)
  }

  archetype_levels <- c(
    "1. Single-Site Practice (1 ZIP5, 1 State)",
    "2. Local Multi-Site Group (2-5 ZIP5s, 1 SCF3, 1 State)",
    "3. Intrastate Regional Group (2-20 ZIP5s across >=2 SCF3s, 1 State)",
    "4. Large Intrastate Health System (>20 ZIP5s, 1 State)",
    "5. Multi-State System / National Aggregator (>=2 States)"
  )

  by_grp <- split(sub, paste(sub$enrollment_id, sub$year, sep = "\r"), drop = TRUE)
  grp_rows <- lapply(by_grp, function(g) {
    n_clin <- length(unique(g$id))
    loc_zips <- unique(g[, c("location_id", ".zip5", ".state"), drop = FALSE])
    n_locs <- nrow(loc_zips)
    u_zips <- unique(loc_zips$.zip5)
    n_z <- length(u_zips)
    n_scf <- length(unique(substr(u_zips, 1L, 3L)))
    n_st <- length(unique(loc_zips$.state[!is.na(loc_zips$.state)]))
    if (n_st == 0L) n_st <- 1L

    # Shannon entropy across distinct locations' ZIP5 distribution
    z_tab <- as.numeric(table(loc_zips$.zip5))
    p_z <- z_tab / sum(z_tab)
    h_nats <- -sum(p_z * log(p_z))
    eff_z <- exp(h_nats)
    spec_wt <- 1 / eff_z

    arch <- dplyr::case_when(
      n_st >= 2L ~ archetype_levels[5],
      n_z == 1L ~ archetype_levels[1],
      n_z <= 5L & n_scf == 1L ~ archetype_levels[2],
      n_z <= 20L ~ archetype_levels[3],
      TRUE ~ archetype_levels[4]
    )

    tibble::tibble(
      enrollment_id = g$enrollment_id[1],
      year = g$year[1],
      n_clinicians = as.integer(n_clin),
      n_locations = as.integer(n_locs),
      n_unique_zip5 = as.integer(n_z),
      n_unique_scf3 = as.integer(n_scf),
      n_unique_states = as.integer(n_st),
      clinician_to_zip_ratio = round(n_clin / n_z, 2),
      shannon_entropy_nats = round(h_nats, 4),
      effective_n_zip5 = round(eff_z, 2),
      location_specificity_weight = round(spec_wt, 4),
      group_archetype = factor(arch, levels = archetype_levels, ordered = TRUE)
    )
  })
  groups_df <- dplyr::bind_rows(grp_rows)
  groups_df <- groups_df[order(groups_df$year, groups_df$enrollment_id), , drop = FALSE]

  n_tot_grps <- nrow(groups_df)
  n_tot_clinks <- sum(groups_df$n_clinicians)

  arch_rows <- lapply(archetype_levels, function(al) {
    sub_g <- groups_df[groups_df$group_archetype == al, , drop = FALSE]
    n_g <- nrow(sub_g)
    n_cl <- sum(sub_g$n_clinicians)
    med_z <- if (n_g > 0L) as.numeric(stats::median(sub_g$n_unique_zip5)) else NA_real_
    tibble::tibble(
      group_archetype = al,
      n_groups = as.integer(n_g),
      pct_of_groups = round(100 * n_g / n_tot_grps, 2),
      n_clinician_links = as.integer(n_cl),
      pct_of_clinician_links = if (n_tot_clinks > 0L) round(100 * n_cl / n_tot_clinks, 2) else NA_real_,
      median_unique_zip5 = med_z
    )
  })
  arch_summary <- dplyr::bind_rows(arch_rows)

  # Map back to clinicians (id, year)
  clin_grp <- unique(sub[, c("id", "year", "enrollment_id"), drop = FALSE])
  clin_grp <- merge(clin_grp, groups_df, by = c("enrollment_id", "year"))
  by_id_yr <- split(clin_grp, paste(clin_grp$id, clin_grp$year, sep = "\r"), drop = TRUE)
  clin_wt_rows <- lapply(by_id_yr, function(cg) {
    best_idx <- which.max(cg$location_specificity_weight)[1]
    tibble::tibble(
      id = cg$id[1],
      year = cg$year[1],
      n_groups = as.integer(nrow(cg)),
      max_group_specificity_weight = round(max(cg$location_specificity_weight), 4),
      mean_group_specificity_weight = round(mean(cg$location_specificity_weight), 4),
      min_group_unique_zip5 = as.integer(min(cg$n_unique_zip5)),
      max_group_unique_zip5 = as.integer(max(cg$n_unique_zip5)),
      best_enrollment_id = cg$enrollment_id[best_idx],
      best_group_archetype = as.character(cg$group_archetype[best_idx])
    )
  })
  clin_weights_df <- dplyr::bind_rows(clin_wt_rows)
  clin_weights_df <- clin_weights_df[order(clin_weights_df$id, clin_weights_df$year), , drop = FALSE]

  list(
    groups = tibble::as_tibble(groups_df),
    archetype_summary = tibble::as_tibble(arch_summary),
    clinician_weights = tibble::as_tibble(clin_weights_df)
  )
}

