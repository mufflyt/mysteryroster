#' Is the optional Crowd-Kit (Python) backend usable right now
#'
#' Internal, not exported. Never installs or configures anything -- it only
#' probes what the caller has already set up, and does so without letting
#' `reticulate` provision an ephemeral Python environment on its own (see
#' Details). [attribute_truth_backends()] and [attribute_truth_fusion()] are
#' the only callers.
#'
#' @details
#' Recent `reticulate` versions will, by default, silently download and
#' manage their own Python via `uv` the first time Python is touched, if no
#' Python installation is otherwise found. That is an automatic Python
#' install, which this package must never do. This probe therefore sets
#' `RETICULATE_USE_MANAGED_VENV = "no"` for the scope of the check (restoring
#' whatever the caller already had on exit) so `reticulate` only ever
#' *discovers* a Python the user configured themselves -- `RETICULATE_PYTHON`,
#' `reticulate::use_python()`/`use_virtualenv()`/`use_condaenv()`, or a
#' `python3` already on `PATH` -- and never provisions one on its own.
#'
#' @return A list: `available` (logical), `reason` (character, `NA` when
#'   available), `python_version`, `crowdkit_version`.
#' @noRd
.mysteryroster_crowdkit_probe <- function() {
  unavailable <- function(reason, python_version = NA_character_) {
    list(available = FALSE, reason = reason, python_version = python_version,
         crowdkit_version = NA_character_)
  }

  if (!requireNamespace("reticulate", quietly = TRUE)) {
    return(unavailable(paste(
      "the 'reticulate' package is not installed.",
      "Install it yourself (install.packages(\"reticulate\")) --",
      "mysteryroster never installs R or Python packages automatically."
    )))
  }

  old_managed_venv <- Sys.getenv("RETICULATE_USE_MANAGED_VENV", unset = NA_character_)
  Sys.setenv(RETICULATE_USE_MANAGED_VENV = "no")
  on.exit({
    if (is.na(old_managed_venv)) Sys.unsetenv("RETICULATE_USE_MANAGED_VENV")
    else Sys.setenv(RETICULATE_USE_MANAGED_VENV = old_managed_venv)
  }, add = TRUE)

  python_ok <- tryCatch(reticulate::py_available(initialize = TRUE), error = function(e) FALSE)
  if (!isTRUE(python_ok)) {
    return(unavailable(paste(
      "no Python installation was found (mysteryroster disables reticulate's automatic",
      "ephemeral-environment provisioning, so it will never download one on your behalf).",
      "Configure a Python yourself -- e.g. reticulate::use_python(),",
      "reticulate::use_virtualenv(), or the RETICULATE_PYTHON environment variable --",
      "then install crowd-kit yourself (pip install crowd-kit)."
    )))
  }

  python_version <- tryCatch(as.character(reticulate::py_config()$version), error = function(e) NA_character_)
  crowdkit_ok <- tryCatch(reticulate::py_module_available("crowdkit"), error = function(e) FALSE)
  if (!isTRUE(crowdkit_ok)) {
    return(unavailable(paste(
      "Python was found, but the 'crowdkit' module is not installed in it.",
      "Install it yourself: pip install crowd-kit (mysteryroster never installs",
      "Python packages automatically)."
    ), python_version))
  }

  crowdkit_version <- tryCatch({
    crowdkit_module <- reticulate::import("crowdkit", delay_load = FALSE, convert = FALSE)
    reticulate::py_to_r(crowdkit_module$`__version__`)
  }, error = function(e) NA_character_)

  list(available = TRUE, reason = NA_character_, python_version = python_version,
       crowdkit_version = crowdkit_version)
}

#' Fit a Crowd-Kit Dawid-Skene-family model and standardize its output
#'
#' Internal, not exported. Called only by [attribute_truth_fusion()] once it
#' has resolved `backend = "crowd_kit"`; `claims_used` is already filtered to
#' exactly one `attribute` and to non-missing identifiers, exactly as the
#' `"majority"` path receives it.
#'
#' @noRd
.mysteryroster_crowdkit_fit <- function(claims_used, attribute_name, method,
                                         max_iterations, convergence_tolerance,
                                         on_source_conflict, abstain_threshold) {
  .mysteryroster_fusion_require_single_value(on_source_conflict, method, "crowd_kit")

  probe <- .mysteryroster_crowdkit_probe()
  if (!isTRUE(probe$available)) {
    stop(sprintf("attribute_truth_fusion: backend 'crowd_kit' is not available: %s", probe$reason),
         call. = FALSE)
  }

  built <- .mysteryroster_fusion_build_tasks(claims_used, on_source_conflict)
  task_key_tbl <- built$task_key_tbl
  votes_used <- built$votes_used
  excluded_per_task <- built$excluded_per_task
  empty_rows <- .mysteryroster_fusion_empty_task_rows(
    task_key_tbl, attribute_name, method, excluded_per_task, "posterior_probability"
  )
  empty_prediction_row <- empty_rows$prediction_row
  empty_candidate_rows <- empty_rows$candidate_rows

  n_total_tasks <- nrow(task_key_tbl)
  tasks_with_votes <- sort(unique(votes_used$task_id))

  if (!length(tasks_with_votes)) {
    predictions_tbl <- dplyr::bind_rows(lapply(task_key_tbl$task_id, empty_prediction_row))
    return(list(
      predictions = predictions_tbl, candidate_scores = empty_candidate_rows(),
      source_diagnostics = tibble::tibble(
        source_id = character(0), attribute = character(0), true_label = character(0),
        observed_label = character(0), error_probability = numeric(0), reliability_score = numeric(0)
      ),
      converged = NA, n_iterations = 0L, n_disconnected_components = NA_integer_,
      backend_version = sprintf("python %s; crowd-kit %s", probe$python_version, probe$crowdkit_version)
    ))
  }

  n_disconnected_components <- .mysteryroster_bipartite_components(votes_used$task_id, votes_used$source_id)
  if (n_disconnected_components > 1L) {
    warning(sprintf(paste0(
      "attribute_truth_fusion: the worker-task graph for attribute '%s' has %d disconnected ",
      "components -- sources in different components share no task in common, so their ",
      "estimated reliabilities (and the resulting posterior probabilities) are not jointly ",
      "identifiable from this call alone."
    ), attribute_name, n_disconnected_components), call. = FALSE)
  }

  old_managed_venv <- Sys.getenv("RETICULATE_USE_MANAGED_VENV", unset = NA_character_)
  Sys.setenv(RETICULATE_USE_MANAGED_VENV = "no")
  on.exit({
    if (is.na(old_managed_venv)) Sys.unsetenv("RETICULATE_USE_MANAGED_VENV")
    else Sys.setenv(RETICULATE_USE_MANAGED_VENV = old_managed_venv)
  }, add = TRUE)

  crowdkit_agg <- reticulate::import("crowdkit.aggregation", delay_load = FALSE, convert = FALSE)
  estimator <- switch(method,
    dawid_skene = crowdkit_agg$DawidSkene(n_iter = as.integer(max_iterations), tol = convergence_tolerance),
    one_coin_dawid_skene = crowdkit_agg$OneCoinDawidSkene(n_iter = as.integer(max_iterations), tol = convergence_tolerance)
  )

  py_votes <- reticulate::r_to_py(data.frame(
    task = as.integer(votes_used$task_id), worker = votes_used$source_id,
    label = votes_used$claimed_value, stringsAsFactors = FALSE
  ), convert = FALSE)

  fitted <- tryCatch(estimator$fit(py_votes), error = function(e) {
    stop(sprintf("attribute_truth_fusion: crowd-kit '%s' failed to fit: %s", method, conditionMessage(e)),
         call. = FALSE)
  })

  loss_history <- reticulate::py_to_r(fitted$loss_history_)
  converged <- length(loss_history) < max_iterations
  n_iterations_used <- length(loss_history)
  if (!converged) {
    warning(sprintf(paste0(
      "attribute_truth_fusion: crowd-kit '%s' did not converge within max_iterations = %d ",
      "for attribute '%s' (tol = %s); treat model_score as provisional."
    ), method, max_iterations, attribute_name, format(convergence_tolerance)), call. = FALSE)
  }

  probas_obj <- fitted$probas_
  proba_task_ids <- reticulate::py_to_r(probas_obj$index$tolist())
  proba_labels <- reticulate::py_to_r(probas_obj$columns$astype("str")$tolist())
  proba_matrix <- reticulate::py_to_r(probas_obj$to_numpy()$tolist())

  task_row_for <- function(task_id) match(task_id, proba_task_ids)

  prediction_rows <- vector("list", n_total_tasks)
  candidate_rows <- vector("list", n_total_tasks)
  for (i in seq_len(n_total_tasks)) {
    task_id <- task_key_tbl$task_id[i]
    task_info <- task_key_tbl[i, , drop = FALSE]
    task_votes <- votes_used[votes_used$task_id == task_id, , drop = FALSE]

    if (!nrow(task_votes)) {
      prediction_rows[[i]] <- empty_prediction_row(task_id)
      candidate_rows[[i]] <- empty_candidate_rows()
      next
    }

    probas_row <- unlist(proba_matrix[[task_row_for(task_id)]])
    winner_idx <- which.max(probas_row)
    winner_value <- proba_labels[[winner_idx]]
    winner_score <- probas_row[winner_idx]
    is_tie <- sum(abs(probas_row - winner_score) < 1e-9) > 1L

    abstained <- is_tie
    abstention_reason <- if (is_tie) "tied_vote" else NA_character_
    if (!abstained && !is.null(abstain_threshold) && winner_score < abstain_threshold) {
      abstained <- TRUE
      abstention_reason <- "below_abstain_threshold"
    }

    local_tally <- table(task_votes$claimed_value)
    prediction_rows[[i]] <- tibble::tibble(
      entity_id = task_info$entity_id, attribute = attribute_name,
      reference_year = task_info$reference_year,
      predicted_value = if (abstained) NA_character_ else winner_value,
      method = method, model_score = round(winner_score, 4), score_type = "posterior_probability",
      n_sources = dplyr::n_distinct(task_votes$source_id),
      n_sources_excluded = excluded_per_task[[as.character(task_id)]],
      n_candidate_values = length(local_tally), abstained = abstained, abstention_reason = abstention_reason
    )
    candidate_rows[[i]] <- tibble::tibble(
      entity_id = task_info$entity_id, attribute = attribute_name,
      reference_year = task_info$reference_year, candidate_value = names(local_tally),
      n_votes = as.integer(local_tally),
      vote_share = round(probas_row[match(names(local_tally), proba_labels)], 4),
      score_type = "posterior_probability"
    )
  }

  source_diagnostics_tbl <- if (method == "one_coin_dawid_skene") {
    skills_obj <- fitted$skills_
    source_ids <- reticulate::py_to_r(skills_obj$index$tolist())
    skill_values <- reticulate::py_to_r(skills_obj$to_numpy()$tolist())
    tibble::tibble(
      source_id = source_ids, attribute = attribute_name, true_label = NA_character_,
      observed_label = NA_character_, error_probability = NA_real_,
      reliability_score = round(unlist(skill_values), 4)
    )
  } else {
    errors_obj <- fitted$errors_
    error_index <- reticulate::py_to_r(errors_obj$index$tolist())
    error_cols <- reticulate::py_to_r(errors_obj$columns$astype("str")$tolist())
    error_matrix <- reticulate::py_to_r(errors_obj$to_numpy()$tolist())
    long_rows <- vector("list", length(error_index) * length(error_cols))
    k <- 0L
    for (row_i in seq_along(error_index)) {
      worker_observed <- error_index[[row_i]]
      row_values <- unlist(error_matrix[[row_i]])
      for (col_j in seq_along(error_cols)) {
        k <- k + 1L
        long_rows[[k]] <- tibble::tibble(
          source_id = worker_observed[[1]], attribute = attribute_name,
          true_label = error_cols[[col_j]], observed_label = worker_observed[[2]],
          error_probability = round(row_values[col_j], 4)
        )
      }
    }
    error_sparse <- dplyr::bind_rows(long_rows)

    # crowd-kit's errors_ only has a (worker, observed_label) row for labels
    # that worker actually produced somewhere in the data -- a worker who
    # NEVER reports a given label is simply absent for it, not present with
    # a near-zero probability. Completing the full source x true_label x
    # observed_label grid and filling those absent combinations with an
    # exact 0 (not NA, not silently dropped) keeps reliability_score honest
    # for a source like a constant-output one that never reports one class
    # at all -- leaving the gap would let such a source look reliable by
    # only ever being scored on the one diagonal cell it happens to have.
    full_grid <- expand.grid(
      source_id = sort(unique(votes_used$source_id)), true_label = proba_labels,
      observed_label = proba_labels, stringsAsFactors = FALSE
    )
    error_long <- merge(full_grid, error_sparse, by = c("source_id", "true_label", "observed_label"), all.x = TRUE)
    error_long$error_probability[is.na(error_long$error_probability)] <- 0
    error_long$attribute <- attribute_name

    diag_rows <- error_long[error_long$true_label == error_long$observed_label, , drop = FALSE]
    reliability_by_source <- stats::aggregate(error_probability ~ source_id, diag_rows, mean)
    names(reliability_by_source) <- c("source_id", "reliability_score")
    reliability_by_source$reliability_score <- round(reliability_by_source$reliability_score, 4)
    merge(error_long, reliability_by_source, by = "source_id", sort = FALSE)
  }

  list(
    predictions = dplyr::bind_rows(prediction_rows),
    candidate_scores = dplyr::bind_rows(candidate_rows),
    source_diagnostics = tibble::as_tibble(source_diagnostics_tbl),
    converged = converged, n_iterations = n_iterations_used,
    n_disconnected_components = n_disconnected_components,
    backend_version = sprintf("python %s; crowd-kit %s", probe$python_version, probe$crowdkit_version)
  )
}
