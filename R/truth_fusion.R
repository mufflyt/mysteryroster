#' Validate a standardized multi-source attribute-claim table
#'
#' Every truth-fusion function in this package expects claims in one long
#' format: one row per `(entity_id, attribute, source_id, reference_year)`
#' claim. This checks that shape without silently repairing it -- a missing
#' or empty `entity_id`/`attribute`/`source_id` stops the call, since
#' nothing downstream can be trusted once identity is ambiguous. A missing
#' `claimed_value` is a real "no claim," never treated as a negative claim;
#' such rows are kept (so a caller can still see the source was present)
#' but excluded from the validation report's informative-row counts.
#'
#' @details
#' **Duplicates vs. legitimate multiple values.** A source submitting the
#' *same* value twice for one entity-attribute-year is a duplicate row. A
#' source submitting *different* values for the same entity-attribute-year
#' (for example two real practice locations) is not an error -- it is
#' counted separately in the report as `multi_value_source_years` and
#' retained rather than collapsed to one row. `on_duplicate = "error"`
#' stops the call if any duplicate key exists at all, without
#' distinguishing the two cases; use the report (see `@return`) to inspect
#' which kind you actually have before choosing that. (Whether a multi-value
#' source's claims actually vote for more than one value in
#' [attribute_truth_fusion()] depends on *that* function's own
#' `on_source_conflict` argument, not on this one.)
#'
#' **Dates.** If `valid_from`/`valid_to` columns are present, rows where
#' `valid_to` is before `valid_from` are flagged (`invalid_interval`) and,
#' with `on_invalid_interval = "error"`, stop the call.
#'
#' **No transformation of claimed values.** This function validates and
#' reports; it does not standardize, bin, or geocode `claimed_value`. That
#' is left to the caller or to a later, attribute-specific step.
#'
#' @param claims A data frame with at least `entity_id`, `attribute`,
#'   `source_id`, `claimed_value`, and `reference_year`.
#' @param on_duplicate `"flag"` (default) or `"error"` if any
#'   entity-attribute-source-year combination has more than one row.
#' @param on_invalid_interval `"flag"` (default) or `"error"` if any row
#'   has `valid_to` before `valid_from` (only checked when both columns
#'   are present).
#' @return `claims` as a tibble (identifiers trimmed to character,
#'   `reference_year` coerced to integer; `claimed_value` otherwise
#'   untouched), with a `report` attribute: a tibble of `check`/`n` rows
#'   for `rows`, `entities`, `attributes`, `sources`,
#'   `missing_claimed_value`, `duplicate_source_years`,
#'   `multi_value_source_years`, and `invalid_interval`.
#' @examples
#' claims <- data.frame(
#'   entity_id = c("1", "1", "1", "2"), attribute = "state",
#'   source_id = c("nppes", "dac", "dac", "nppes"),
#'   claimed_value = c("CO", "CO", "NM", "NY"), reference_year = 2024
#' )
#' validated <- attribute_claims_validate(claims)
#' attr(validated, "report")
#' @seealso [attribute_truth_fusion()]; `vignette("consensus-and-corroboration")`
#'   for the pairwise/n-way relative-claim functions this complements.
#' @family truth fusion
#' @export
attribute_claims_validate <- function(claims, on_duplicate = c("flag", "error"),
                                       on_invalid_interval = c("flag", "error")) {
  checkmate::assert_data_frame(claims, min.rows = 1)
  on_duplicate <- match.arg(on_duplicate)
  on_invalid_interval <- match.arg(on_invalid_interval)

  required_cols <- c("entity_id", "attribute", "source_id", "claimed_value", "reference_year")
  missing_cols <- setdiff(required_cols, names(claims))
  if (length(missing_cols) > 0) {
    stop(sprintf("attribute_claims_validate: claims is missing required column(s): %s",
                 paste(missing_cols, collapse = ", ")), call. = FALSE)
  }

  claims_valid <- tibble::as_tibble(claims)
  claims_valid$entity_id <- trimws(as.character(claims_valid$entity_id))
  claims_valid$attribute <- trimws(as.character(claims_valid$attribute))
  claims_valid$source_id <- trimws(as.character(claims_valid$source_id))
  claims_valid$claimed_value <- as.character(claims_valid$claimed_value)
  claims_valid$reference_year <- suppressWarnings(as.integer(claims_valid$reference_year))

  bad_id <- is.na(claims_valid$entity_id) | !nzchar(claims_valid$entity_id)
  bad_attribute <- is.na(claims_valid$attribute) | !nzchar(claims_valid$attribute)
  bad_source <- is.na(claims_valid$source_id) | !nzchar(claims_valid$source_id)
  if (any(bad_id | bad_attribute | bad_source)) {
    stop(sprintf(
      "attribute_claims_validate: %d row(s) have a missing or empty entity_id/attribute/source_id",
      sum(bad_id | bad_attribute | bad_source)
    ), call. = FALSE)
  }

  n_missing_value <- sum(is.na(claims_valid$claimed_value) | !nzchar(claims_valid$claimed_value))

  key_cols <- c("entity_id", "attribute", "source_id", "reference_year")
  key_strings <- do.call(paste0, claims_valid[key_cols])
  duplicate_keys <- unique(key_strings[duplicated(key_strings)])
  n_duplicate_source_years <- length(duplicate_keys)
  n_multi_value_source_years <- 0L
  if (n_duplicate_source_years > 0L) {
    duplicated_rows <- claims_valid[key_strings %in% duplicate_keys, , drop = FALSE]
    duplicated_rows_key <- key_strings[key_strings %in% duplicate_keys]
    distinct_values_per_key <- tapply(duplicated_rows$claimed_value, duplicated_rows_key,
                                       function(values) length(unique(values)))
    n_multi_value_source_years <- sum(distinct_values_per_key > 1L)
    if (on_duplicate == "error") {
      stop(sprintf(
        "attribute_claims_validate: %d entity-attribute-source-year combination(s) have duplicate rows",
        n_duplicate_source_years
      ), call. = FALSE)
    }
  }

  n_invalid_interval <- 0L
  if (all(c("valid_from", "valid_to") %in% names(claims_valid))) {
    interval_start <- suppressWarnings(as.Date(claims_valid$valid_from))
    interval_end <- suppressWarnings(as.Date(claims_valid$valid_to))
    invalid_interval <- !is.na(interval_start) & !is.na(interval_end) & interval_end < interval_start
    n_invalid_interval <- sum(invalid_interval)
    if (n_invalid_interval > 0L && on_invalid_interval == "error") {
      stop(sprintf("attribute_claims_validate: %d row(s) have valid_to before valid_from",
                   n_invalid_interval), call. = FALSE)
    }
  }

  validation_report <- tibble::tibble(
    check = c("rows", "entities", "attributes", "sources", "missing_claimed_value",
              "duplicate_source_years", "multi_value_source_years", "invalid_interval"),
    n = c(nrow(claims_valid), dplyr::n_distinct(claims_valid$entity_id),
          dplyr::n_distinct(claims_valid$attribute), dplyr::n_distinct(claims_valid$source_id),
          n_missing_value, n_duplicate_source_years, n_multi_value_source_years, n_invalid_interval)
  )

  message(sprintf(
    paste0("attribute_claims_validate: %s rows, %s entities, %s attributes, %s sources ",
           "(%s duplicate source-years, %s multi-value, %s invalid intervals)"),
    format(nrow(claims_valid), big.mark = ","),
    format(dplyr::n_distinct(claims_valid$entity_id), big.mark = ","),
    dplyr::n_distinct(claims_valid$attribute), dplyr::n_distinct(claims_valid$source_id),
    n_duplicate_source_years, n_multi_value_source_years, n_invalid_interval
  ))

  attr(claims_valid, "report") <- validation_report
  claims_valid
}

#' Which truth-fusion backends and methods are available right now
#'
#' A static capability report, not a live probe -- Phase 1 of this
#' package's truth-fusion work implements only `"majority"`, entirely in
#' R, regardless of whether Python or the optional `crowd-kit`/
#' `truthdiscovery` libraries happen to be installed. This never reports
#' a method as available before it is actually wired up, so a caller can
#' trust `attribute_truth_fusion()` not to silently fall back to a
#' different algorithm than the one requested.
#'
#' @return A tibble: `backend`, `available` (logical), `methods_supported`,
#'   `status`.
#' @examples
#' attribute_truth_backends()
#' @family truth fusion
#' @export
attribute_truth_backends <- function() {
  tibble::tibble(
    backend = c("r_native", "crowd_kit", "truthdiscovery"),
    available = c(TRUE, FALSE, FALSE),
    methods_supported = c(
      "majority",
      "dawid_skene, one_coin_dawid_skene",
      "majority, sums, average_log, investment, pooled_investment, truthfinder"
    ),
    status = c(
      "implemented",
      "not yet implemented in this package version",
      "not yet implemented in this package version"
    )
  )
}

#' Infer the most plausible value of a disputed attribute, with abstention
#'
#' Every other function in this package's no-gold-standard family
#' deliberately never names a value as "correct" -- only relative claims
#' about agreement, timing, or stability (see
#' `vignette("consensus-and-corroboration")`'s design note on exactly this
#' point). `attribute_truth_fusion()` is a different kind of thing: it
#' *does* predict a value, with an explicit score and the option to
#' abstain rather than guess. Phase 1 implements exactly one method,
#' `"majority"`, entirely in R; `method` and `backend` are validated
#' against a fixed, currently-implemented set (see
#' [attribute_truth_backends()]) rather than silently substituting a
#' different algorithm than the one requested.
#'
#' @details
#' **Voting unit.** A vote is cast by a *source*, not a *row*: votes are
#' counted on distinct `(source_id, claimed_value)` pairs per
#' entity-attribute-year, so a source that submitted the same value twice
#' (a duplicate row) only votes once.
#'
#' **A source with more than one distinct value for one entity-attribute-year
#' (`on_source_conflict`).** `"single_value"` (the default) treats this as
#' an internally conflicted, unresolved claim: that source is excluded
#' entirely from voting for that entity-year (never arbitrarily resolved,
#' never allowed to cast competing votes), and the exclusion is counted in
#' `n_sources_excluded`. If *every* source for an entity-year is excluded
#' this way, the call abstains with `abstention_reason =
#' "no_single_value_sources"` rather than erroring. `"multi_value"` instead
#' keeps every distinct value as its own vote, appropriate when more than
#' one true value can legitimately coexist (for example an entity with
#' multiple real practice locations) -- but note that under
#' `"multi_value"`, `model_score` is a share of *votes cast*, not a
#' probability that a single categorical value is correct, since one
#' source can contribute to more than one candidate's count.
#'
#' **Grouping.** Entity-years are grouped with `split(x, list(entity_id,
#' reference_year))`, never a hand-built pasted string key -- `entity_id`
#' and `reference_year` are both already guaranteed non-missing by this
#' point, so there is no risk of the all-`NA`-key failure mode a pasted or
#' `interaction()`-based key can hit (see `attribute_consensus_tier()`'s
#' own fix for exactly that bug), and no risk of two different
#' entity-years colliding onto the same pasted string.
#'
#' **Abstention.** An exact tie for the largest vote share always
#' abstains (`abstention_reason = "tied_vote"`) -- there is no principled
#' winner to report. If `abstain_threshold` is set and the winning share
#' is below it even without a tie, the call also abstains
#' (`"below_abstain_threshold"`). `predicted_value` is `NA` whenever
#' `abstained` is `TRUE`.
#'
#' **One attribute per call.** If `claims` contains more than one
#' distinct `attribute` value, `attribute_name` must select one; this is
#' deliberate, not a missing convenience, so an "active/inactive" claim
#' and a "practice state" claim are never silently pooled into one vote.
#' Because `claims` is filtered to exactly one `attribute` before any
#' grouping happens, votes can never mix across attributes regardless of
#' how entity-years are grouped.
#'
#' **Arguments majority doesn't use.** `implication_function`,
#' `max_iterations`, and `convergence_tolerance` exist for methods Phase 1
#' does not implement yet; passing a non-default value with
#' `method = "majority"` emits a message noting it has no effect, rather
#' than silently ignoring it.
#'
#' @param claims A data frame shaped like [attribute_claims_validate()]'s
#'   input/output.
#' @param method Inference method. Only `"majority"` is implemented in
#'   this version.
#' @param attribute_name Which `attribute` value in `claims` to fuse.
#'   Required if `claims` contains more than one.
#' @param backend `"auto"` (default) resolves to the one backend that
#'   implements `method`; passing a specific backend that does not
#'   implement `method` is an error, never a silent substitution.
#' @param implication_function,max_iterations,convergence_tolerance
#'   Reserved for methods not yet implemented; see Details.
#' @param seed Stored in `model_diagnostics` for reproducibility
#'   record-keeping; `"majority"` itself is deterministic and does not
#'   use it.
#' @param abstain_threshold Optional minimum winning vote share (`0`-`1`);
#'   below it, abstain even without a tie. `NULL` (default) disables this.
#' @param on_source_conflict `"single_value"` (default) or `"multi_value"`;
#'   see Details.
#' @return An object of class `mysteryroster_truth`, a list with:
#'   \describe{
#'     \item{`predictions`}{One row per entity-attribute-year:
#'       `entity_id`, `attribute`, `reference_year`, `predicted_value`,
#'       `method`, `model_score`, `score_type`, `n_sources` (votes actually
#'       counted, after excluding conflicted sources under
#'       `"single_value"`), `n_sources_excluded`, `n_candidate_values`,
#'       `abstained`, `abstention_reason` (also `"no_single_value_sources"`
#'       when every source for that entity-year was excluded).}
#'     \item{`candidate_scores`}{One row per candidate value per
#'       entity-attribute-year, ranked by `vote_share` descending.}
#'     \item{`source_diagnostics`}{Zero rows for `"majority"`, which
#'       learns no source-level reliability; populated by later methods.}
#'     \item{`model_diagnostics`}{One row: `method`, `backend`,
#'       `converged`, `n_iterations`, `runtime_sec`, `seed`,
#'       `n_warnings`.}
#'     \item{`claims`}{The input claims actually used (filtered to
#'       `attribute_name`, non-missing), unmodified.}
#'   }
#' @examples
#' claims <- data.frame(
#'   entity_id = c("1", "1", "1", "2", "2"), attribute = "state",
#'   source_id = c("nppes", "dac", "op", "nppes", "dac"),
#'   claimed_value = c("CO", "CO", "NM", "NY", "NJ"), reference_year = 2024
#' )
#' fusion_result <- attribute_truth_fusion(claims, method = "majority")
#' fusion_result$predictions
#' fusion_result$candidate_scores
#' @seealso [attribute_claims_validate()]; [attribute_truth_backends()];
#'   `vignette("consensus-and-corroboration")` for the relative-claim
#'   functions that never predict a value.
#' @family truth fusion
#' @export
attribute_truth_fusion <- function(claims, method = "majority", attribute_name = NULL,
                                    backend = "auto", implication_function = NULL,
                                    seed = 2026L, max_iterations = 100L,
                                    convergence_tolerance = 0.001, abstain_threshold = NULL,
                                    on_source_conflict = c("single_value", "multi_value")) {
  checkmate::assert_data_frame(claims, min.rows = 1)
  checkmate::assert_string(method, min.chars = 1)
  checkmate::assert_string(backend, min.chars = 1)
  checkmate::assert_int(seed)
  if (!is.null(abstain_threshold)) checkmate::assert_number(abstain_threshold, lower = 0, upper = 1)
  on_source_conflict <- match.arg(on_source_conflict)

  implemented_methods <- c(majority = "r_native")
  if (!method %in% names(implemented_methods)) {
    stop(sprintf(
      "attribute_truth_fusion: method '%s' is not yet implemented in this version; available: %s",
      method, paste(names(implemented_methods), collapse = ", ")
    ), call. = FALSE)
  }
  required_backend <- implemented_methods[[method]]
  resolved_backend <- if (identical(backend, "auto")) required_backend else backend
  if (!identical(resolved_backend, required_backend)) {
    stop(sprintf("attribute_truth_fusion: method '%s' requires backend '%s', not '%s'",
                 method, required_backend, resolved_backend), call. = FALSE)
  }

  if (!is.null(implication_function)) {
    message("attribute_truth_fusion: 'implication_function' is not used by method = 'majority'")
  }
  if (!isTRUE(all.equal(max_iterations, 100L))) {
    message("attribute_truth_fusion: 'max_iterations' is not used by method = 'majority'")
  }
  if (!isTRUE(all.equal(convergence_tolerance, 0.001))) {
    message("attribute_truth_fusion: 'convergence_tolerance' is not used by method = 'majority'")
  }

  required_cols <- c("entity_id", "attribute", "source_id", "claimed_value", "reference_year")
  missing_cols <- setdiff(required_cols, names(claims))
  if (length(missing_cols) > 0) {
    stop(sprintf("attribute_truth_fusion: claims is missing required column(s): %s",
                 paste(missing_cols, collapse = ", ")), call. = FALSE)
  }

  claims_in <- tibble::as_tibble(claims)
  claims_in$entity_id <- trimws(as.character(claims_in$entity_id))
  claims_in$attribute <- trimws(as.character(claims_in$attribute))
  claims_in$source_id <- trimws(as.character(claims_in$source_id))
  claims_in$claimed_value <- as.character(claims_in$claimed_value)
  claims_in$reference_year <- suppressWarnings(as.integer(claims_in$reference_year))

  present_attributes <- unique(claims_in$attribute)
  if (is.null(attribute_name)) {
    if (length(present_attributes) != 1L) {
      stop(sprintf(
        paste0("attribute_truth_fusion: claims contains %d distinct attributes (%s); ",
               "pass attribute_name to select one"),
        length(present_attributes), paste(present_attributes, collapse = ", ")
      ), call. = FALSE)
    }
    attribute_name <- present_attributes
  } else {
    checkmate::assert_string(attribute_name, min.chars = 1)
    if (!attribute_name %in% present_attributes) {
      stop(sprintf("attribute_truth_fusion: attribute_name '%s' is not present in claims", attribute_name),
           call. = FALSE)
    }
  }

  # Filtering to exactly one attribute HERE, before any grouping below,
  # is what guarantees votes can never mix across attributes -- grouping
  # by (entity_id, reference_year) alone is therefore safe, not an
  # oversight; attribute is constant within claims_used by construction.
  claims_used <- claims_in[claims_in$attribute == attribute_name, , drop = FALSE]
  claims_used <- claims_used[
    !is.na(claims_used$claimed_value) & nzchar(claims_used$claimed_value) &
      !is.na(claims_used$entity_id) & nzchar(claims_used$entity_id) &
      !is.na(claims_used$source_id) & nzchar(claims_used$source_id) &
      !is.na(claims_used$reference_year),
    , drop = FALSE
  ]
  if (!nrow(claims_used)) {
    stop("attribute_truth_fusion: no non-missing claims remain for attribute_name after filtering",
         call. = FALSE)
  }

  started_at <- proc.time()[["elapsed"]]

  # One vote row per distinct (entity, year, source, value) -- not per
  # input row -- so a source repeating the same value twice (a duplicate
  # row) only casts one vote. entity_id/reference_year are both already
  # guaranteed non-missing above, so grouping via split(x, list(...))
  # never hits the all-NA-key failure mode a pasted string key can, and
  # never risks two different entity-years colliding onto one key.
  vote_cols <- c("entity_id", "reference_year", "source_id", "claimed_value")
  votes_long <- claims_used[!duplicated(claims_used[vote_cols]), vote_cols, drop = FALSE]
  vote_groups <- split(votes_long, list(votes_long$entity_id, votes_long$reference_year), drop = TRUE)

  prediction_rows <- vector("list", length(vote_groups))
  candidate_rows <- vector("list", length(vote_groups))
  for (i in seq_along(vote_groups)) {
    full_group <- vote_groups[[i]]
    group_entity_id <- full_group$entity_id[1]
    group_reference_year <- full_group$reference_year[1]

    # on_source_conflict: a source appears more than once in full_group
    # only by having a DIFFERENT claimed_value each time (exact-duplicate
    # rows were already collapsed above). "single_value" excludes such a
    # source from voting here entirely -- unresolved, not arbitrarily
    # picked -- rather than letting it cast competing votes.
    source_value_counts <- table(full_group$source_id)
    conflicted_sources <- names(source_value_counts)[source_value_counts > 1L]
    n_sources_excluded <- 0L
    group_votes <- full_group
    if (on_source_conflict == "single_value" && length(conflicted_sources) > 0L) {
      n_sources_excluded <- length(conflicted_sources)
      group_votes <- full_group[!full_group$source_id %in% conflicted_sources, , drop = FALSE]
    }

    if (!nrow(group_votes)) {
      prediction_rows[[i]] <- tibble::tibble(
        entity_id = group_entity_id, attribute = attribute_name, reference_year = group_reference_year,
        predicted_value = NA_character_, method = method, model_score = NA_real_,
        score_type = "vote_share", n_sources = 0L, n_sources_excluded = n_sources_excluded,
        n_candidate_values = 0L, abstained = TRUE, abstention_reason = "no_single_value_sources"
      )
      candidate_rows[[i]] <- tibble::tibble(
        entity_id = character(0), attribute = character(0), reference_year = integer(0),
        candidate_value = character(0), n_votes = integer(0), vote_share = numeric(0)
      )
      next
    }

    tally <- table(group_votes$claimed_value)
    n_sources_group <- dplyr::n_distinct(group_votes$source_id)
    winner_n <- max(tally)
    is_tie <- sum(tally == winner_n) > 1L
    winner_value <- names(tally)[which.max(tally)]
    vote_share <- winner_n / n_sources_group

    abstained <- is_tie
    abstention_reason <- if (is_tie) "tied_vote" else NA_character_
    if (!abstained && !is.null(abstain_threshold) && vote_share < abstain_threshold) {
      abstained <- TRUE
      abstention_reason <- "below_abstain_threshold"
    }

    prediction_rows[[i]] <- tibble::tibble(
      entity_id = group_entity_id, attribute = attribute_name, reference_year = group_reference_year,
      predicted_value = if (abstained) NA_character_ else winner_value,
      method = method, model_score = round(vote_share, 4), score_type = "vote_share",
      n_sources = n_sources_group, n_sources_excluded = n_sources_excluded,
      n_candidate_values = length(tally),
      abstained = abstained, abstention_reason = abstention_reason
    )
    candidate_rows[[i]] <- tibble::tibble(
      entity_id = group_entity_id, attribute = attribute_name, reference_year = group_reference_year,
      candidate_value = names(tally), n_votes = as.integer(tally),
      vote_share = round(as.integer(tally) / n_sources_group, 4)
    )
  }

  predictions_tbl <- dplyr::bind_rows(prediction_rows)
  predictions_tbl <- predictions_tbl[order(predictions_tbl$entity_id, predictions_tbl$reference_year), ]
  candidate_scores_tbl <- dplyr::bind_rows(candidate_rows)
  if (nrow(candidate_scores_tbl) > 0L) {
    candidate_scores_tbl <- candidate_scores_tbl[order(
      candidate_scores_tbl$entity_id, candidate_scores_tbl$reference_year, -candidate_scores_tbl$vote_share
    ), ]
  }

  elapsed_sec <- round(proc.time()[["elapsed"]] - started_at, 3)
  n_abstained <- sum(predictions_tbl$abstained)
  message(sprintf(
    "attribute_truth_fusion: method = '%s', on_source_conflict = '%s', %s entity-years, %s abstained (%.1f%%), %.3fs",
    method, on_source_conflict, format(nrow(predictions_tbl), big.mark = ","), n_abstained,
    100 * n_abstained / nrow(predictions_tbl), elapsed_sec
  ))

  structure(
    list(
      predictions = tibble::as_tibble(predictions_tbl),
      candidate_scores = tibble::as_tibble(candidate_scores_tbl),
      source_diagnostics = tibble::tibble(source_id = character(0), reliability_score = numeric(0)),
      model_diagnostics = tibble::tibble(
        method = method, backend = resolved_backend, on_source_conflict = on_source_conflict,
        converged = TRUE, n_iterations = 1L, runtime_sec = elapsed_sec, seed = seed, n_warnings = 0L
      ),
      claims = claims_used
    ),
    class = "mysteryroster_truth"
  )
}

#' @export
print.mysteryroster_truth <- function(x, ...) {
  n_abstained <- sum(x$predictions$abstained)
  cat(sprintf(
    "<mysteryroster_truth>\n  method: %s (backend: %s, on_source_conflict: %s)\n  %s entity-years predicted, %s abstained (%.1f%%)\n",
    x$model_diagnostics$method[1], x$model_diagnostics$backend[1], x$model_diagnostics$on_source_conflict[1],
    format(nrow(x$predictions), big.mark = ","), n_abstained,
    100 * n_abstained / nrow(x$predictions)
  ))
  invisible(x)
}
