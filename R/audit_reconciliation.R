#' Automated accounting-identity validator and version-to-version audit reconciliation log
#'
#' Complex multi-source provider directory audits evolve across manuscript
#' revisions as denominator definitions, sub-feed vintages, or deduplication
#' rules are refined. Two classes of numerical errors commonly arise during
#' revisions:
#' \enumerate{
#'   \item **Accounting-identity violations**: A table described as a mutually
#'     exclusive partition sums to more or less than `100.0%` (e.g., overlapping
#'     ZIP5 and SCF3 match categories summing to `103.6%`), or a child stage in
#'     an evidence funnel exceeds its parent denominator.
#'   \item **Unreconciled version drift**: Numbers change between an initial
#'     draft (`baseline`) and a revised run (`revised`) without a structured log
#'     documenting `old_value`, `new_value`, `delta`, and the methodological
#'     `reconciliation_reason`.
#' }
#'
#' This function validates accounting identities across a named list of
#' mutually exclusive tables (`partitions`), compares `baseline` and `revised`
#' audit metrics, and generates both a structured tibble (`diff_table`) and a
#' publication-ready **Section 9 Numerical Reconciliation Log** in Markdown and
#' HTML (`markdown_table`, `html_table`).
#'
#' @param baseline,revised Data frames (or named numeric vectors) of audit
#'   metrics to compare. When data frames are supplied, they must contain
#'   `metric_id` and `value` (plus optional `label`, `unit`, and
#'   `reconciliation_reason` on `revised`). When named numeric vectors are
#'   supplied, names are used as `metric_id`.
#' @param partitions Optional named list of data frames representing mutually
#'   exclusive decompositions to validate (for example, outputs from
#'   [attribute_geographic_concordance()], [attribute_location_specificity()],
#'   or [attribute_disagreement_trajectories()]). For each table, rows excluding
#'   subtotal/total rows (`Total`, `Subtotal`, `total`, `resolved_total`) are
#'   checked to verify that their counts sum to the table's total and their
#'   percentages sum to `100.0%` within `tol_pct`.
#' @param reasons Optional named character vector mapping `metric_id` to a
#'   human-readable methodological explanation for why the metric changed between
#'   `baseline` and `revised`.
#' @param tol_pct Tolerance in percentage points for checking `100.0%` partition
#'   sums. Default `0.15` (allows standard rounding across categories).
#' @param tol_value Numerical tolerance below which `abs(new_value - old_value)`
#'   is classified as `"unchanged"`. Default `1e-6`.
#' @return A list with four elements:
#'   \describe{
#'     \item{`diff_table`}{Tibble with one row per metric: `metric_id`, `label`,
#'       `old_value`, `new_value`, `raw_delta`, `pct_change`, `status`
#'       (`"unchanged"`, `"reconciled_change"`, `"unexplained_change"`,
#'       `"added_in_revised"`, `"removed_in_revised"`), and
#'       `reconciliation_reason`.}
#'     \item{`partition_checks`}{Tibble with one row per table in `partitions`:
#'       `partition_name`, `n_categories`, `sum_n`, `expected_n`, `sum_pct`,
#'       `pct_deviation_from_100`, and `identity_valid` (`TRUE`/`FALSE`).}
#'     \item{`markdown_table`}{GitHub-flavored Markdown table of all changed
#'       metrics, ready to paste into a manuscript supplement or artifact.}
#'     \item{`html_table`}{Self-contained HTML `<table class="oig-table">`
#'       rendering the reconciliation log.}
#'   }
#' @examples
#' base_m <- c(
#'   pecos_linkage_pct = 95.7,
#'   board_zip5_concordance_sum_pct = 103.6,
#'   medicare_2018_coverage_pct = 22.7
#' )
#' rev_m <- c(
#'   pecos_linkage_pct = 95.7,
#'   board_zip5_concordance_sum_pct = 100.0,
#'   medicare_2018_coverage_pct = 46.84
#' )
#' reasons <- c(
#'   board_zip5_concordance_sum_pct = "Replaced overlapping marginal ZIP3/mailing counts with 5-way mutually exclusive hierarchy.",
#'   medicare_2018_coverage_pct = "Merged 2018 DAC historical release with 2018 PECOS snapshot."
#' )
#' part_tbl <- data.frame(
#'   category = c("Cat 1", "Cat 2", "Total"),
#'   n = c(60, 40, 100),
#'   pct = c(60.0, 40.0, 100.0)
#' )
#' rec <- attribute_audit_diff(
#'   base_m, rev_m,
#'   partitions = list(board_concordance = part_tbl),
#'   reasons = reasons
#' )
#' rec$diff_table[, c("metric_id", "old_value", "new_value", "raw_delta", "status")]
#' rec$partition_checks
#' @seealso [attribute_geographic_concordance()], [attribute_location_specificity()],
#'   [attribute_disagreement_trajectories()].
#' @family no-gold-standard comparison
#' @export
attribute_audit_diff <- function(baseline,
                                 revised,
                                 partitions = NULL,
                                 reasons = NULL,
                                 tol_pct = 0.15,
                                 tol_value = 1e-6) {
  checkmate::assert_number(tol_pct, lower = 0, finite = TRUE)
  checkmate::assert_number(tol_value, lower = 0, finite = TRUE)

  norm_metrics <- function(x, arg_name) {
    if (is.numeric(x) && !is.null(names(x))) {
      return(tibble::tibble(
        metric_id = trimws(names(x)),
        label = trimws(names(x)),
        value = as.numeric(unname(x)),
        reason = NA_character_
      ))
    }
    checkmate::assert_data_frame(x, min.rows = 1)
    if (!all(c("metric_id", "value") %in% names(x))) {
      stop(sprintf("attribute_audit_diff: %s must be a named numeric vector or a data frame with 'metric_id' and 'value'.",
                   arg_name), call. = FALSE)
    }
    m_id <- trimws(as.character(x$metric_id))
    if (anyDuplicated(m_id)) {
      stop(sprintf("attribute_audit_diff: %s has duplicate metric_id values.", arg_name), call. = FALSE)
    }
    lbl <- if ("label" %in% names(x)) trimws(as.character(x$label)) else m_id
    rsn <- if ("reconciliation_reason" %in% names(x)) trimws(as.character(x$reconciliation_reason)) else rep(NA_character_, nrow(x))
    tibble::tibble(
      metric_id = m_id,
      label = lbl,
      value = as.numeric(x$value),
      reason = rsn
    )
  }

  b_df <- norm_metrics(baseline, "baseline")
  r_df <- norm_metrics(revised, "revised")

  all_ids <- unique(c(b_df$metric_id, r_df$metric_id))
  b_map_val <- stats::setNames(b_df$value, b_df$metric_id)
  r_map_val <- stats::setNames(r_df$value, r_df$metric_id)
  lbl_map <- stats::setNames(c(b_df$label, r_df$label), c(b_df$metric_id, r_df$metric_id))
  rsn_map <- stats::setNames(r_df$reason, r_df$metric_id)

  if (!is.null(reasons)) {
    checkmate::assert_character(reasons, names = "unique")
    for (nm in names(reasons)) {
      rsn_map[nm] <- unname(reasons[nm])
    }
  }

  diff_rows <- lapply(all_ids, function(mid) {
    old_v <- unname(b_map_val[mid])
    new_v <- unname(r_map_val[mid])
    delta <- if (!is.na(old_v) && !is.na(new_v)) round(new_v - old_v, 6) else NA_real_
    pct_ch <- if (!is.na(old_v) && !is.na(new_v) && abs(old_v) > 1e-12) round(100 * (new_v - old_v) / abs(old_v), 2) else NA_real_
    rsn <- unname(rsn_map[mid])
    if (is.na(rsn) || !nzchar(rsn)) rsn <- NA_character_

    st <- dplyr::case_when(
      is.na(old_v) & !is.na(new_v) ~ "added_in_revised",
      !is.na(old_v) & is.na(new_v) ~ "removed_in_revised",
      abs(new_v - old_v) <= tol_value ~ "unchanged",
      !is.na(rsn) ~ "reconciled_change",
      TRUE ~ "unexplained_change"
    )

    tibble::tibble(
      metric_id = mid,
      label = unname(lbl_map[mid]),
      old_value = old_v,
      new_value = new_v,
      raw_delta = delta,
      pct_change = pct_ch,
      status = st,
      reconciliation_reason = ifelse(is.na(rsn), if (st == "unchanged") "No change" else "Reason not specified", rsn)
    )
  })
  diff_table <- dplyr::bind_rows(diff_rows)

  # Validate mutually exclusive partitions if supplied
  part_rows <- list()
  if (!is.null(partitions)) {
    checkmate::assert_list(partitions, min.len = 1, names = "unique")
    for (p_name in names(partitions)) {
      p_df <- partitions[[p_name]]
      checkmate::assert_data_frame(p_df, min.rows = 1)
      # Detect count column (n or n_clinicians) and pct column (pct or pct_of_cohort)
      n_col <- intersect(c("n", "n_clinicians", "count"), names(p_df))[1]
      pct_col <- intersect(c("pct", "pct_of_cohort", "percentage"), names(p_df))[1]
      lbl_col <- names(p_df)[1]
      if (is.na(n_col) || is.na(pct_col)) {
        stop(sprintf("attribute_audit_diff: partitions$%s must contain a count column ('n' or 'n_clinicians') and a percent column ('pct' or 'pct_of_cohort').",
                     p_name), call. = FALSE)
      }
      row_labels <- tolower(trimws(as.character(p_df[[lbl_col]])))
      is_tot <- row_labels %in% c("total", "overall", "all") | grepl("^total\\b", row_labels)
      is_sub <- grepl("^subtotal\\b|resolved_total", row_labels)
      if ("trajectory_code" %in% names(p_df)) {
        tc <- tolower(trimws(as.character(p_df$trajectory_code)))
        is_tot <- is_tot | (tc == "total")
        is_sub <- is_sub | (tc == "resolved_total")
      }
      comp_rows <- p_df[!is_tot & !is_sub, , drop = FALSE]
      tot_rows <- p_df[is_tot, , drop = FALSE]

      sum_n <- sum(comp_rows[[n_col]], na.rm = TRUE)
      exp_n <- if (nrow(tot_rows) > 0L) tot_rows[[n_col]][1] else sum_n
      sum_pct <- round(sum(comp_rows[[pct_col]], na.rm = TRUE), 2)
      dev_pct <- round(abs(sum_pct - 100.0), 2)
      ok <- (sum_n == exp_n) && (dev_pct <= tol_pct)

      part_rows[[p_name]] <- tibble::tibble(
        partition_name = p_name,
        n_categories = as.integer(nrow(comp_rows)),
        sum_n = as.integer(sum_n),
        expected_n = as.integer(exp_n),
        sum_pct = sum_pct,
        pct_deviation_from_100 = dev_pct,
        identity_valid = ok
      )
    }
  }
  partition_checks <- if (length(part_rows) > 0L) {
    dplyr::bind_rows(part_rows)
  } else {
    tibble::tibble(
      partition_name = character(0),
      n_categories = integer(0),
      sum_n = integer(0),
      expected_n = integer(0),
      sum_pct = numeric(0),
      pct_deviation_from_100 = numeric(0),
      identity_valid = logical(0)
    )
  }

  fmt_v <- function(v) ifelse(is.na(v), "NA", format(round(v, 3), trim = TRUE, scientific = FALSE))
  fmt_d <- function(v) {
    ifelse(is.na(v), "—", ifelse(v > 0, paste0("+", format(round(v, 3), trim = TRUE, scientific = FALSE)),
                                 format(round(v, 3), trim = TRUE, scientific = FALSE)))
  }

  md_lines <- c(
    "| Metric | Baseline | Revised | Delta | Status | Reconciliation Reason |",
    "| :--- | ---: | ---: | ---: | :--- | :--- |"
  )
  html_rows <- character(0)
  for (i in seq_len(nrow(diff_table))) {
    r <- diff_table[i, ]
    md_lines <- c(
      md_lines,
      sprintf("| `%s` | %s | %s | %s | %s | %s |",
              r$metric_id, fmt_v(r$old_value), fmt_v(r$new_value),
              fmt_d(r$raw_delta), r$status, r$reconciliation_reason)
    )
    html_rows <- c(
      html_rows,
      sprintf(
        "<tr><td><code>%s</code></td><td class=\"num\">%s</td><td class=\"num\"><strong>%s</strong></td><td class=\"num\">%s</td><td>%s</td><td>%s</td></tr>",
        .oig_html_escape(r$metric_id),
        .oig_html_escape(fmt_v(r$old_value)),
        .oig_html_escape(fmt_v(r$new_value)),
        .oig_html_escape(fmt_d(r$raw_delta)),
        .oig_html_escape(r$status),
        .oig_html_escape(r$reconciliation_reason)
      )
    )
  }

  html_table <- paste0(
    "<table class=\"oig-table\">",
    "<thead><tr><th>Metric</th><th class=\"num\">Baseline</th><th class=\"num\">Revised</th>",
    "<th class=\"num\">Delta</th><th>Status</th><th>Reconciliation Reason</th></tr></thead>",
    "<tbody>", paste(html_rows, collapse = "\n"), "</tbody></table>"
  )

  list(
    diff_table = tibble::as_tibble(diff_table),
    partition_checks = tibble::as_tibble(partition_checks),
    markdown_table = paste(md_lines, collapse = "\n"),
    html_table = html_table
  )
}
