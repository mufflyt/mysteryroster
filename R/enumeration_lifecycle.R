#' Cross-classify professional certification/licensure lifecycle against NPI enumeration and billing activity
#'
#' An active National Provider Identifier (`NPI`) in NPPES is an administrative
#' enumeration record, not a real-time professional certification or clinical
#' activity registry. A clinician whose specialty board certification or state
#' license has expired or who has retired from clinical practice is not
#' automatically required to deactivate her NPI unless she is deceased or
#' fraudulent. Treating every clinician with an expired certification and an
#' active NPI as a "stale directory error" conflates professional certification
#' currency with NPI enumeration status.
#'
#' Conversely, when a certifying board or state licensing roster records a
#' clinician as `DECEASED` (or when CMS's NPPES Deactivated NPI Report shows a
#' deactivation date), any subsequent appearance in active NPPES snapshots or
#' downstream claims/enrollment feeds (`PECOS`, `T-MSIS`, `Open Payments`) is a
#' genuine post-mortem or post-deactivation administrative persistence event.
#'
#' This function separates certification status (`ACTIVE`, `LAPSED/RETIRED`,
#' `DECEASED`, `REVOKED/SUSPENDED`) from NPI enumeration status (`Active` vs.
#' `Deactivated`), audits deceased clinicians stratified by linkage-confidence
#' tier (`Tier 1 Exact` vs. `Tier 2 Fuzzy/Probabilistic`), and checks optional
#' downstream billing/enrollment panels for post-event ghost activity.
#'
#' @param roster Data frame of professional board certification or state
#'   licensing records containing `id`, `status_col`, optional `tier_col`, and
#'   optional `event_year_col` (e.g. year of death or certification expiration).
#' @param active_enumeration Data frame or character vector of NPIs currently
#'   present in the active NPPES directory extract. If a data frame, must
#'   contain `id`.
#' @param deactivated_enumeration Optional data frame of deactivated NPIs from
#'   the CMS NPPES Deactivated NPI Report containing `id` and optional
#'   `deactivation_date_col`.
#' @param activity_panels Optional named list of longitudinal or cross-sectional
#'   downstream source data frames (e.g. `list(pecos = pecos_df, tmsis = tmsis_df)`),
#'   each containing `id` and optional `year`, used to check whether clinicians
#'   appear in billing/enrollment feeds *after* death or NPI deactivation.
#' @param id Name of the identifier column. Default `"npi"`.
#' @param status_col Name of the certification/licensure status column in
#'   `roster`. Default `"cert_status"`. Values are classified case-insensitively
#'   into `"active"`, `"deceased"`, `"revoked_or_suspended"`, or
#'   `"lapsed_or_retired"`.
#' @param tier_col Optional name of the record-linkage confidence tier column in
#'   `roster` (e.g. `"match_tier"` with values such as `"Tier 1"` or `"Tier 2"`).
#'   Default `"match_tier"`.
#' @param event_year_col Optional name of an integer year column in `roster`
#'   indicating when death or expiration occurred, used to test whether
#'   `activity_panels` rows occurred strictly after the event (`year > event_year`).
#' @param deactivation_date_col Optional column name in
#'   `deactivated_enumeration` giving the deactivation date or year.
#' @param conf_level Confidence level for Wilson intervals via [wilson_ci()].
#'   Default `0.95`.
#' @return A list with four tibbles:
#'   \describe{
#'     \item{`clinicians`}{One row per unique `id` in `roster` with normalized
#'       `lifecycle_status`, `npi_enumeration_status` (`"active_nppes"`,
#'       `"confirmed_deactivated"`, or `"not_in_active_or_deactivated"`),
#'       `any_downstream_activity`, `post_event_downstream_activity`, and
#'       `joint_classification`.}
#'     \item{`matrix_summary`}{Cross-tabulation of `lifecycle_status` by
#'       `npi_enumeration_status` with counts, percentages of the roster, and
#'       Wilson CIs.}
#'     \item{`deceased_audit`}{Stratified audit of `DECEASED` clinicians by
#'       `match_tier` (`Tier 1` vs `Tier 2` vs `Overall`), reporting how many
#'       remain active in NPPES, how many are confirmed in the CMS Deactivated
#'       NPI file, and how many appear in downstream `activity_panels`.}
#'     \item{`downstream_persistence`}{Per-source summary across
#'       `activity_panels` showing active-certification vs lapsed/retired vs
#'       deceased clinician counts.}
#'   }
#' @examples
#' roster <- data.frame(
#'   npi = c("1", "2", "3", "4", "5"),
#'   cert_status = c("ACTIVE", "EXPIRED", "RETIRED", "DECEASED", "DECEASED"),
#'   match_tier = c("Tier 1", "Tier 1", "Tier 1", "Tier 1", "Tier 2"),
#'   event_year = c(NA, 2020, 2021, 2018, 2019)
#' )
#' active_nppes <- data.frame(npi = c("1", "2", "3", "5"))
#' deact_nppes <- data.frame(npi = "4", deactivation_date = "2018-09-01")
#' panels <- list(
#'   pecos = data.frame(npi = c("1", "2"), year = c(2024, 2024)),
#'   tmsis = data.frame(npi = c("1", "3"), year = c(2024, 2020))
#' )
#' life <- attribute_enumeration_lifecycle(
#'   roster, active_nppes,
#'   deactivated_enumeration = deact_nppes,
#'   activity_panels = panels,
#'   event_year_col = "event_year"
#' )
#' life$matrix_summary[, c("lifecycle_status", "npi_enumeration_status", "n", "pct")]
#' life$deceased_audit
#' @seealso [attribute_record_presence()], [attribute_oig_audit_prose()].
#' @family no-gold-standard comparison
#' @export
attribute_enumeration_lifecycle <- function(roster,
                                            active_enumeration,
                                            deactivated_enumeration = NULL,
                                            activity_panels = NULL,
                                            id = "npi",
                                            status_col = "cert_status",
                                            tier_col = "match_tier",
                                            event_year_col = NULL,
                                            deactivation_date_col = "deactivation_date",
                                            conf_level = 0.95) {
  checkmate::assert_data_frame(roster, min.rows = 1)
  checkmate::assert_string(id, min.chars = 1)
  checkmate::assert_string(status_col, min.chars = 1)
  checkmate::assert_number(conf_level, lower = 0, upper = 1)
  if (conf_level <= 0 || conf_level >= 1) {
    stop("attribute_enumeration_lifecycle: conf_level must be strictly between 0 and 1.", call. = FALSE)
  }

  for (col in c(id, status_col)) {
    if (!col %in% names(roster)) {
      stop(sprintf("attribute_enumeration_lifecycle: roster is missing column '%s'.", col), call. = FALSE)
    }
  }

  r_id <- trimws(as.character(roster[[id]]))
  if (any(is.na(r_id) | !nzchar(r_id))) {
    stop("attribute_enumeration_lifecycle: roster has missing or blank id values.", call. = FALSE)
  }
  if (anyDuplicated(r_id)) {
    stop("attribute_enumeration_lifecycle: roster must have one row per unique id.", call. = FALSE)
  }

  raw_status <- toupper(trimws(as.character(roster[[status_col]])))
  lifecycle_status <- dplyr::case_when(
    grepl("DECEAS|DEAD|DIED", raw_status) ~ "deceased",
    grepl("REVOK|SUSPEND|SANCTION|BARRED|DISCIPLIN", raw_status) ~ "revoked_or_suspended",
    grepl("^ACTIVE|CURRENT|VALID|CERTIFIED|IN FORCE", raw_status) & !grepl("INACTIVE|EXPIRED|LAPSED|RETIRED", raw_status) ~ "active",
    TRUE ~ "lapsed_or_retired"
  )

  has_tier <- !is.null(tier_col) && (tier_col %in% names(roster))
  match_tier <- if (has_tier) trimws(as.character(roster[[tier_col]])) else rep("All", length(r_id))
  match_tier[is.na(match_tier) | !nzchar(match_tier)] <- "Unclassified"

  has_ev_yr <- !is.null(event_year_col) && (event_year_col %in% names(roster))
  event_year <- if (has_ev_yr) suppressWarnings(as.integer(roster[[event_year_col]])) else rep(NA_integer_, length(r_id))

  if (is.data.frame(active_enumeration)) {
    if (!id %in% names(active_enumeration)) {
      stop(sprintf("attribute_enumeration_lifecycle: active_enumeration is missing column '%s'.", id), call. = FALSE)
    }
    act_ids <- unique(trimws(as.character(active_enumeration[[id]])))
  } else {
    act_ids <- unique(trimws(as.character(active_enumeration)))
  }
  act_ids <- act_ids[!is.na(act_ids) & nzchar(act_ids)]

  deact_ids <- character(0)
  deact_dates <- stats::setNames(character(0), character(0))
  if (!is.null(deactivated_enumeration)) {
    checkmate::assert_data_frame(deactivated_enumeration)
    if (!id %in% names(deactivated_enumeration)) {
      stop(sprintf("attribute_enumeration_lifecycle: deactivated_enumeration is missing column '%s'.", id), call. = FALSE)
    }
    d_id <- trimws(as.character(deactivated_enumeration[[id]]))
    valid_d <- !is.na(d_id) & nzchar(d_id)
    deact_ids <- unique(d_id[valid_d])
    if (!is.null(deactivation_date_col) && (deactivation_date_col %in% names(deactivated_enumeration))) {
      d_dt <- trimws(as.character(deactivated_enumeration[[deactivation_date_col]]))[valid_d]
      deact_dates <- stats::setNames(d_dt, d_id[valid_d])
    }
  }

  in_active <- r_id %in% act_ids
  in_deact <- r_id %in% deact_ids

  npi_enum_status <- dplyr::case_when(
    in_deact & !in_active ~ "confirmed_deactivated",
    in_deact & in_active ~ "active_and_deactivated_conflict",
    in_active ~ "active_nppes",
    TRUE ~ "not_in_active_or_deactivated"
  )

  clin <- tibble::tibble(
    id = r_id,
    raw_status = raw_status,
    lifecycle_status = lifecycle_status,
    match_tier = match_tier,
    event_year = event_year,
    in_active_nppes = in_active,
    in_deactivated_nppes = in_deact,
    deactivation_date = unname(deact_dates[r_id]),
    npi_enumeration_status = npi_enum_status
  )

  any_downstream <- rep(FALSE, nrow(clin))
  post_event_downstream <- rep(FALSE, nrow(clin))
  ds_rows <- list()

  if (!is.null(activity_panels)) {
    checkmate::assert_list(activity_panels, min.len = 1, names = "unique")
    for (p_name in names(activity_panels)) {
      p_df <- activity_panels[[p_name]]
      checkmate::assert_data_frame(p_df)
      if (!id %in% names(p_df)) {
        stop(sprintf("attribute_enumeration_lifecycle: activity_panels$%s is missing column '%s'.", p_name, id), call. = FALSE)
      }
      p_ids <- trimws(as.character(p_df[[id]]))
      in_p <- clin$id %in% unique(p_ids[!is.na(p_ids) & nzchar(p_ids)])
      any_downstream <- any_downstream | in_p

      post_p <- rep(FALSE, nrow(clin))
      if ("year" %in% names(p_df) && has_ev_yr) {
        p_sub <- data.frame(
          id = p_ids,
          year = suppressWarnings(as.integer(p_df[["year"]])),
          stringsAsFactors = FALSE
        )
        p_sub <- p_sub[!is.na(p_sub$id) & !is.na(p_sub$year), , drop = FALSE]
        if (nrow(p_sub) > 0L) {
          max_yr_by_id <- tapply(p_sub$year, p_sub$id, max)
          max_p_yr <- as.integer(unname(max_yr_by_id[clin$id]))
          post_p <- !is.na(max_p_yr) & !is.na(clin$event_year) & (max_p_yr > clin$event_year)
        }
      }
      post_event_downstream <- post_event_downstream | post_p

      ds_rows[[p_name]] <- tibble::tibble(
        source = p_name,
        n_roster_present = as.integer(sum(in_p)),
        n_active_cert_present = as.integer(sum(in_p & clin$lifecycle_status == "active")),
        n_lapsed_or_retired_present = as.integer(sum(in_p & clin$lifecycle_status == "lapsed_or_retired")),
        n_deceased_present = as.integer(sum(in_p & clin$lifecycle_status == "deceased")),
        n_post_event_present = as.integer(sum(post_p))
      )
    }
  }

  clin$any_downstream_activity <- any_downstream
  clin$post_event_downstream_activity <- post_event_downstream
  clin$joint_classification <- paste0(clin$lifecycle_status, "__", clin$npi_enumeration_status)

  n_tot <- nrow(clin)
  combos <- unique(clin[, c("lifecycle_status", "npi_enumeration_status"), drop = FALSE])
  combos <- combos[order(combos$lifecycle_status, combos$npi_enumeration_status), , drop = FALSE]

  mat_rows <- lapply(seq_len(nrow(combos)), function(i) {
    ls_i <- combos$lifecycle_status[i]
    es_i <- combos$npi_enumeration_status[i]
    mask <- clin$lifecycle_status == ls_i & clin$npi_enumeration_status == es_i
    n_k <- sum(mask)
    ci <- wilson_ci(n_k, n_tot, conf_level = conf_level, as_percent = TRUE)
    tibble::tibble(
      lifecycle_status = ls_i,
      npi_enumeration_status = es_i,
      n = as.integer(n_k),
      n_roster = as.integer(n_tot),
      pct = round(100 * n_k / n_tot, 2),
      ci_low = round(ci$ci_low, 2),
      ci_high = round(ci$ci_high, 2),
      n_any_downstream = as.integer(sum(clin$any_downstream_activity[mask])),
      n_post_event_downstream = as.integer(sum(clin$post_event_downstream_activity[mask]))
    )
  })
  matrix_summary <- dplyr::bind_rows(mat_rows)

  dec_df <- clin[clin$lifecycle_status == "deceased", , drop = FALSE]
  tiers <- sort(unique(dec_df$match_tier))
  make_dec_row <- function(t_lbl, sub_d) {
    n_d <- nrow(sub_d)
    tibble::tibble(
      match_tier = t_lbl,
      n_deceased = as.integer(n_d),
      n_confirmed_deactivated = as.integer(sum(sub_d$in_deactivated_nppes)),
      n_still_active_nppes = as.integer(sum(sub_d$in_active_nppes)),
      pct_still_active_nppes = if (n_d > 0L) round(100 * sum(sub_d$in_active_nppes) / n_d, 1) else NA_real_,
      n_any_downstream_activity = as.integer(sum(sub_d$any_downstream_activity)),
      n_post_event_downstream_activity = as.integer(sum(sub_d$post_event_downstream_activity))
    )
  }
  dec_rows <- lapply(tiers, function(tl) make_dec_row(tl, dec_df[dec_df$match_tier == tl, , drop = FALSE]))
  deceased_audit <- dplyr::bind_rows(dec_rows, make_dec_row("Overall", dec_df))

  downstream_persistence <- if (length(ds_rows) > 0L) {
    dplyr::bind_rows(ds_rows)
  } else {
    tibble::tibble(
      source = character(0),
      n_roster_present = integer(0),
      n_active_cert_present = integer(0),
      n_lapsed_or_retired_present = integer(0),
      n_deceased_present = integer(0),
      n_post_event_present = integer(0)
    )
  }

  list(
    clinicians = tibble::as_tibble(clin),
    matrix_summary = tibble::as_tibble(matrix_summary),
    deceased_audit = tibble::as_tibble(deceased_audit),
    downstream_persistence = tibble::as_tibble(downstream_persistence)
  )
}
