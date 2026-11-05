#' Reject on_source_conflict = "multi_value" for a probabilistic backend
#'
#' Internal, not exported. Shared by the `crowd_kit` and `truthdiscovery`
#' backends: both assume exactly one claim per source-task, so a source with
#' internally conflicting claims has no well-defined single contribution --
#' "multi_value" is an error, never a silent pick of one of its claims.
#'
#' @noRd
.mysteryroster_fusion_require_single_value <- function(on_source_conflict, method, backend_label) {
  if (on_source_conflict != "single_value") {
    stop(sprintf(paste0(
      "attribute_truth_fusion: on_source_conflict = '%s' is not supported by backend ",
      "'%s' -- %s assumes exactly one claim per source-task, so a source with internally ",
      "conflicting claims has no well-defined contribution. Use on_source_conflict = ",
      "'single_value' (the default), or use method = 'majority' if you need 'multi_value' ",
      "semantics."
    ), on_source_conflict, backend_label, method), call. = FALSE)
  }
}

#' Build a collision-safe task table and apply single_value exclusion
#'
#' Internal, not exported. Shared task-construction logic for every backend
#' that fits jointly across entity-years (`crowd_kit`, `truthdiscovery`):
#' dedup to one vote per (entity, year, source, value), assign a
#' `merge()`-built integer task surrogate key (never a pasted string, for
#' the same collision reasons as the majority backend's own grouping), and
#' exclude any source that claims more than one distinct value for a task
#' under `on_source_conflict = "single_value"` (the only policy these
#' backends accept; see `.mysteryroster_fusion_require_single_value()`).
#'
#' @return A list: `task_key_tbl` (`entity_id`, `reference_year`,
#'   `task_id`, one row per task, in `task_id` order), `votes_used` (the
#'   surviving, non-excluded vote rows, with `task_id`), `excluded_per_task`
#'   (a vector named by `as.character(task_id)` of the number of distinct
#'   sources excluded for that task).
#' @noRd
.mysteryroster_fusion_build_tasks <- function(claims_used, on_source_conflict) {
  vote_cols <- c("entity_id", "reference_year", "source_id", "claimed_value")
  votes_long <- claims_used[!duplicated(claims_used[vote_cols]), vote_cols, drop = FALSE]

  task_key_tbl <- unique(votes_long[c("entity_id", "reference_year")])
  task_key_tbl <- task_key_tbl[order(task_key_tbl$entity_id, task_key_tbl$reference_year), , drop = FALSE]
  task_key_tbl$task_id <- seq_len(nrow(task_key_tbl))
  votes_long <- merge(votes_long, task_key_tbl, by = c("entity_id", "reference_year"), sort = FALSE)

  flagged_groups <- lapply(split(votes_long, votes_long$task_id), function(task_rows) {
    source_value_counts <- table(task_rows$source_id)
    task_rows$excluded <- task_rows$source_id %in% names(source_value_counts)[source_value_counts > 1L]
    task_rows
  })
  votes_flagged <- do.call(rbind, flagged_groups)
  excluded_per_task <- stats::setNames(rep(0L, nrow(task_key_tbl)), as.character(task_key_tbl$task_id))
  excluded_counts <- vapply(flagged_groups, function(task_rows) {
    length(unique(task_rows$source_id[task_rows$excluded]))
  }, integer(1))
  excluded_per_task[names(excluded_counts)] <- as.integer(excluded_counts)
  votes_used <- votes_flagged[!votes_flagged$excluded, setdiff(names(votes_flagged), "excluded"), drop = FALSE]

  list(task_key_tbl = task_key_tbl, votes_used = votes_used, excluded_per_task = excluded_per_task)
}

#' Count connected components of the worker-task bipartite graph
#'
#' Internal, not exported. Shared by every backend that estimates one
#' reliability value per source by pooling evidence across tasks
#' (`crowd_kit`'s Dawid-Skene family, `truthdiscovery`'s trust-propagation
#' family): if the worker-task bipartite graph splits into more than one
#' connected component, parameters in different components share no
#' evidence and are not jointly identifiable from this call alone.
#'
#' @param task_ids,worker_ids Parallel vectors, one entry per rated
#'   (task, worker) edge.
#' @return An integer count of connected components.
#' @noRd
.mysteryroster_bipartite_components <- function(task_ids, worker_ids) {
  unique_tasks <- sort(unique(task_ids))
  unique_workers <- sort(unique(worker_ids))
  task_code <- match(task_ids, unique_tasks)
  worker_code <- match(worker_ids, unique_workers) + length(unique_tasks)
  n_nodes <- length(unique_tasks) + length(unique_workers)

  parent <- seq_len(n_nodes)
  find_root <- function(x) {
    while (parent[x] != x) x <- parent[x]
    x
  }
  for (i in seq_along(task_code)) {
    root_a <- find_root(task_code[i])
    root_b <- find_root(worker_code[i])
    if (root_a != root_b) parent[max(root_a, root_b)] <- min(root_a, root_b)
  }
  roots <- vapply(seq_len(n_nodes), find_root, integer(1))
  length(unique(roots))
}

#' Build the task-level fallback rows for entirely-excluded tasks
#'
#' Internal, not exported. A task where every source was excluded under
#' `on_source_conflict = "single_value"` never reaches the Python model at
#' all -- this builds its `predictions`/`candidate_scores` rows directly,
#' identically for every joint-fitting backend.
#'
#' @noRd
.mysteryroster_fusion_empty_task_rows <- function(task_key_tbl, attribute_name, method,
                                                   excluded_per_task, score_type) {
  empty_prediction_row <- function(task_id) {
    task_info <- task_key_tbl[task_key_tbl$task_id == task_id, , drop = FALSE]
    tibble::tibble(
      entity_id = task_info$entity_id, attribute = attribute_name,
      reference_year = task_info$reference_year, predicted_value = NA_character_,
      method = method, model_score = NA_real_, score_type = score_type,
      n_sources = 0L, n_sources_excluded = excluded_per_task[[as.character(task_id)]],
      n_candidate_values = 0L, abstained = TRUE, abstention_reason = "no_single_value_sources"
    )
  }
  empty_candidate_rows <- function() {
    tibble::tibble(
      entity_id = character(0), attribute = character(0), reference_year = integer(0),
      candidate_value = character(0), n_votes = integer(0), vote_share = numeric(0),
      score_type = character(0)
    )
  }
  list(prediction_row = empty_prediction_row, candidate_rows = empty_candidate_rows)
}
