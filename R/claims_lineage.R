#' Collapse same-lineage sources into one effective vote
#'
#' [attribute_truth_fusion()] treats every distinct `source_id` as an
#' independent voter. That assumption breaks when two or more sources
#' share an upstream feed -- for example DAC (Doctors and Clinicians) and
#' PECOS revalidation, which the `midwifery` project's ablation findings
#' (`docs/midwifery-ablation-findings.md`) document as sharing lineage,
#' or a hospital-facility-affiliation extract built from the same DAC
#' snapshot it is vintage-matched to. Counting both as independent votes
#' double-counts one underlying record as two pieces of evidence. This
#' function applies one explicit rule, uniformly, to any declared group
#' of same-lineage `source_id`s, *before* the claims reach
#' [attribute_claims_validate()] or [attribute_truth_fusion()]: if every
#' source in the group reports the same value for an entity-attribute-year,
#' collapse them to a single vote (cast by the family itself); if they
#' disagree, drop the group's claims for that entity-attribute-year
#' entirely (no vote, not an arbitrary pick); if only one of the group's
#' sources has a claim there, pass it through unchanged under the family
#' `source_id`. Sources named in no group are never touched.
#'
#' @details
#' **Why drop on disagreement instead of picking one.** A tie-break would
#' quietly re-introduce exactly the single-source dependence this
#' function exists to remove -- the family would always resolve toward
#' whichever member happens to break ties. Dropping is the same choice
#' the `midwifery` prototype made for DAC/PECOS, carried here as a
#' general rule rather than a one-off.
#'
#' **This does not detect lineage.** Which `source_id`s share lineage is
#' a provenance fact about how the data was built, not something
#' recoverable from the claims table itself -- `lineage_groups` must be
#' supplied explicitly. Two sources agreeing often is not evidence they
#' share lineage (independent accurate sources agree often too), and two
#' sources disagreeing sometimes is not evidence they don't.
#'
#' **One family vote still competes on equal footing.** After collapse,
#' the family's single surviving vote is indistinguishable from any other
#' source's vote to [attribute_truth_fusion()] -- it is not weighted up
#' or down for having absorbed multiple original sources.
#'
#' @param claims A data frame shaped like [attribute_claims_validate()]'s
#'   input: at least `entity_id`, `attribute`, `source_id`,
#'   `claimed_value`, `reference_year`. Any other columns are carried
#'   through on single-source-present and passthrough rows, and dropped
#'   on collapsed-agreement rows (the collapsed row's provenance is the
#'   family, not any one original source's extra columns).
#' @param lineage_groups A named list, one element per family:
#'   `list(dac_pecos = c("dac", "pecos_revalidation"), ...)`. Every
#'   element needs 2+ `source_id` values (a family of one is not a
#'   lineage group); a `source_id` must not appear in more than one
#'   family.
#' @param on_conflict `"drop"` (default; see Details) or `"flag"` -- keep
#'   the original, uncollapsed rows for a disagreeing group instead of
#'   removing them, so a caller can inspect `report`'s `conflicting_groups`
#'   and decide case by case.
#' @return `claims` as a tibble, with each lineage group's rows replaced
#'   per the rule above (`source_id` becomes the family name for a
#'   collapsed-agreement or single-source-present row) and a `report`
#'   attribute: a tibble of `check`/`n` rows for `families_applied`,
#'   `entity_attribute_years_collapsed_agree`,
#'   `entity_attribute_years_dropped_conflict`,
#'   `entity_attribute_years_single_source`.
#' @examples
#' claims <- data.frame(
#'   entity_id = c("1", "1", "1", "2", "2", "2"), attribute = "state",
#'   source_id = c("dac", "pecos", "nppes", "dac", "pecos", "nppes"),
#'   claimed_value = c("CO", "CO", "CO", "CO", "NM", "NM"), reference_year = 2024
#' )
#' collapsed <- attribute_claims_collapse_lineage(
#'   claims, lineage_groups = list(dac_pecos = c("dac", "pecos"))
#' )
#' collapsed
#' attr(collapsed, "report")
#' @seealso [attribute_claims_validate()]; [attribute_truth_fusion()];
#'   `docs/midwifery-ablation-findings.md` for the DAC/PECOS case this
#'   generalizes.
#' @family truth fusion
#' @export
attribute_claims_collapse_lineage <- function(claims, lineage_groups,
                                               on_conflict = c("drop", "flag")) {
  checkmate::assert_data_frame(claims, min.rows = 1)
  checkmate::assert_list(lineage_groups, min.len = 1, names = "unique")
  on_conflict <- match.arg(on_conflict)

  required_cols <- c("entity_id", "attribute", "source_id", "claimed_value", "reference_year")
  missing_cols <- setdiff(required_cols, names(claims))
  if (length(missing_cols) > 0) {
    stop(sprintf("attribute_claims_collapse_lineage: claims is missing required column(s): %s",
                 paste(missing_cols, collapse = ", ")), call. = FALSE)
  }

  all_members <- unlist(lineage_groups, use.names = FALSE)
  if (length(all_members) != length(unique(all_members))) {
    dup <- unique(all_members[duplicated(all_members)])
    stop(sprintf("attribute_claims_collapse_lineage: source_id(s) in more than one lineage group: %s",
                 paste(dup, collapse = ", ")), call. = FALSE)
  }
  too_small <- names(lineage_groups)[lengths(lineage_groups) < 2L]
  if (length(too_small) > 0) {
    stop(sprintf("attribute_claims_collapse_lineage: lineage group(s) with fewer than 2 members: %s",
                 paste(too_small, collapse = ", ")), call. = FALSE)
  }
  if (any(!nzchar(names(lineage_groups)))) {
    stop("attribute_claims_collapse_lineage: every lineage group must be named", call. = FALSE)
  }

  claims_out <- tibble::as_tibble(claims)
  claims_out$entity_id <- trimws(as.character(claims_out$entity_id))
  claims_out$attribute <- trimws(as.character(claims_out$attribute))
  claims_out$source_id <- trimws(as.character(claims_out$source_id))
  claims_out$claimed_value <- as.character(claims_out$claimed_value)
  claims_out$reference_year <- suppressWarnings(as.integer(claims_out$reference_year))

  n_collapsed_agree <- 0L
  n_dropped_conflict <- 0L
  n_single_source <- 0L
  n_families_applied <- 0L
  conflicting_groups <- vector("list", 0)

  untouched <- claims_out[!claims_out$source_id %in% all_members, , drop = FALSE]
  kept_rows <- list(untouched)

  for (family in names(lineage_groups)) {
    members <- lineage_groups[[family]]
    group_claims <- claims_out[claims_out$source_id %in% members, , drop = FALSE]
    if (!nrow(group_claims)) next
    n_families_applied <- n_families_applied + 1L

    key_cols <- c("entity_id", "attribute", "reference_year")
    key_strings <- do.call(paste0, group_claims[key_cols])
    by_key <- split(group_claims, key_strings)

    resolved_rows <- vector("list", length(by_key))
    for (i in seq_along(by_key)) {
      g <- by_key[[i]]
      distinct_sources <- unique(g$source_id)
      distinct_values <- unique(g$claimed_value)

      if (length(distinct_sources) == 1L) {
        n_single_source <- n_single_source + 1L
        row_out <- g[1, , drop = FALSE]
        row_out$source_id <- family
        resolved_rows[[i]] <- row_out
      } else if (length(distinct_values) == 1L) {
        n_collapsed_agree <- n_collapsed_agree + 1L
        row_out <- g[1, required_cols, drop = FALSE]
        row_out$source_id <- family
        resolved_rows[[i]] <- row_out
      } else {
        n_dropped_conflict <- n_dropped_conflict + 1L
        conflicting_groups[[length(conflicting_groups) + 1L]] <- tibble::tibble(
          family = family, entity_id = g$entity_id[1], attribute = g$attribute[1],
          reference_year = g$reference_year[1],
          values = paste(sort(distinct_values), collapse = "|")
        )
        if (on_conflict == "flag") resolved_rows[[i]] <- g
      }
    }
    kept_rows[[length(kept_rows) + 1L]] <- dplyr::bind_rows(resolved_rows)
  }

  claims_collapsed <- dplyr::bind_rows(kept_rows)

  conflicting_groups_df <- if (length(conflicting_groups) > 0) {
    dplyr::bind_rows(conflicting_groups)
  } else {
    tibble::tibble(family = character(0), entity_id = character(0), attribute = character(0),
                    reference_year = integer(0), values = character(0))
  }

  report <- tibble::tibble(
    check = c("families_applied", "entity_attribute_years_collapsed_agree",
              "entity_attribute_years_dropped_conflict", "entity_attribute_years_single_source"),
    n = c(n_families_applied, n_collapsed_agree, n_dropped_conflict, n_single_source)
  )

  message(sprintf(
    paste0("attribute_claims_collapse_lineage: %d famil%s applied (%d collapsed on agreement, ",
           "%d dropped on conflict, %d single-source passthrough)"),
    n_families_applied, if (n_families_applied == 1L) "y" else "ies",
    n_collapsed_agree, n_dropped_conflict, n_single_source
  ))

  attr(claims_collapsed, "report") <- report
  attr(claims_collapsed, "conflicting_groups") <- conflicting_groups_df
  claims_collapsed
}
