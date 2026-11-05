#' Is the optional truthdiscovery (Python, GPL-3.0) backend usable right now
#'
#' Internal, not exported. Mirrors `.mysteryroster_crowdkit_probe()` exactly
#' -- same safety contract (never provisions a Python environment), same
#' return shape -- for the `joesingo/truthdiscovery` package instead of
#' `crowd-kit`.
#'
#' @details
#' `truthdiscovery` is GPL-3.0 licensed (unlike `crowd-kit`'s Apache-2.0).
#' mysteryroster (MIT) never vendors or incorporates any of its source --
#' this probe and `.mysteryroster_truthdiscovery_fit()` only call the
#' installed package at arm's length, through `reticulate`, exactly as
#' they would call any other optional runtime dependency the user installs
#' and manages themselves. If you redistribute a bundle that includes the
#' actual `truthdiscovery` package alongside mysteryroster, GPL-3.0's
#' copyleft terms apply to that redistributed bundle; they do not convert
#' mysteryroster's own source. This is a factual description of how the
#' dependency is used, not legal advice -- get a real legal review before
#' any formal redistribution (e.g. a CRAN submission bundling Python
#' dependencies).
#'
#' `truthdiscovery`'s last release (1.0.4) predates Python's removal of
#' `distutils`, so installing it with `pip` on Python >= 3.12 fails at
#' build time (`ModuleNotFoundError: No module named 'distutils'`) even
#' though the installed package itself imports and runs fine once
#' installed into an older interpreter (verified directly: Python 3.11).
#' If you want both this backend and `crowd_kit` available at once, they
#' need *separate* Python environments -- `crowd-kit` is fine on recent
#' Python, `truthdiscovery` currently is not.
#'
#' @return A list: `available` (logical), `reason` (character, `NA` when
#'   available), `python_version`, `truthdiscovery_version`.
#' @noRd
.mysteryroster_truthdiscovery_probe <- function() {
  unavailable <- function(reason, python_version = NA_character_) {
    list(available = FALSE, reason = reason, python_version = python_version,
         truthdiscovery_version = NA_character_)
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
      "then install truthdiscovery yourself (pip install truthdiscovery; requires",
      "Python < 3.12, since its last release predates the removal of distutils)."
    )))
  }

  python_version <- tryCatch(as.character(reticulate::py_config()$version), error = function(e) NA_character_)
  truthdiscovery_ok <- tryCatch(reticulate::py_module_available("truthdiscovery"), error = function(e) FALSE)
  if (!isTRUE(truthdiscovery_ok)) {
    return(unavailable(paste(
      "Python was found, but the 'truthdiscovery' module is not installed in it.",
      "Install it yourself: pip install truthdiscovery (mysteryroster never installs",
      "Python packages automatically). Note: truthdiscovery's last release predates the",
      "removal of distutils, so it requires Python < 3.12 -- a separate environment from",
      "crowd-kit if you use both."
    ), python_version))
  }

  truthdiscovery_version <- tryCatch({
    pkg_resources <- reticulate::import("importlib.metadata", delay_load = FALSE, convert = FALSE)
    reticulate::py_to_r(pkg_resources$version("truthdiscovery"))
  }, error = function(e) NA_character_)

  list(available = TRUE, reason = NA_character_, python_version = python_version,
       truthdiscovery_version = truthdiscovery_version)
}

#' Fit a truthdiscovery trust-propagation model and standardize its output
#'
#' Internal, not exported. Called only by [attribute_truth_fusion()] once it
#' has resolved `backend = "truthdiscovery"`; `claims_used` is already
#' filtered to exactly one `attribute` and to non-missing identifiers.
#'
#' @details
#' `truthdiscovery`'s belief scores are **not** probabilities -- unlike
#' `crowd-kit`'s `probas_`, a variable's belief scores across candidate
#' values do not sum to 1 (verified directly against the installed Python
#' package: e.g. one fit gave `inactive = 0.799` and `active = 0.666` for
#' the same task, summing to 1.465). `score_type = "uncalibrated_belief"`
#' reflects this; never label these `"posterior_probability"`.
#'
#' `implication_function` (an R function `(var, val1, val2) -> numeric in
#' [-1, 1]` or `NULL`) is passed through to `truthdiscovery`'s
#' `Dataset(..., implication_function = ...)` -- the mechanism TruthFinder
#' uses to let belief in one claim influence belief in a related claim
#' (e.g. geographic proximity between two candidate locations). It is
#' `NULL` (disabled) by default and is evaluated, not polished: every call
#' crosses the R/Python boundary, so it is slow, and it has only been
#' exercised in this package's own tests, not tuned for production use.
#'
#' @noRd
.mysteryroster_truthdiscovery_fit <- function(claims_used, attribute_name, method,
                                               max_iterations, convergence_tolerance,
                                               on_source_conflict, abstain_threshold,
                                               implication_function) {
  .mysteryroster_fusion_require_single_value(on_source_conflict, method, "truthdiscovery")

  probe <- .mysteryroster_truthdiscovery_probe()
  if (!isTRUE(probe$available)) {
    stop(sprintf("attribute_truth_fusion: backend 'truthdiscovery' is not available: %s", probe$reason),
         call. = FALSE)
  }

  built <- .mysteryroster_fusion_build_tasks(claims_used, on_source_conflict)
  task_key_tbl <- built$task_key_tbl
  votes_used <- built$votes_used
  excluded_per_task <- built$excluded_per_task
  empty_rows <- .mysteryroster_fusion_empty_task_rows(
    task_key_tbl, attribute_name, method, excluded_per_task, "uncalibrated_belief"
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
      backend_version = sprintf("python %s; truthdiscovery %s", probe$python_version, probe$truthdiscovery_version)
    ))
  }

  n_disconnected_components <- .mysteryroster_bipartite_components(votes_used$task_id, votes_used$source_id)
  if (n_disconnected_components > 1L) {
    warning(sprintf(paste0(
      "attribute_truth_fusion: the source-task graph for attribute '%s' has %d disconnected ",
      "components -- sources in different components share no task in common, so their ",
      "estimated trust (and the resulting belief scores) are not jointly identifiable from ",
      "this call alone."
    ), attribute_name, n_disconnected_components), call. = FALSE)
  }

  old_managed_venv <- Sys.getenv("RETICULATE_USE_MANAGED_VENV", unset = NA_character_)
  Sys.setenv(RETICULATE_USE_MANAGED_VENV = "no")
  on.exit({
    if (is.na(old_managed_venv)) Sys.unsetenv("RETICULATE_USE_MANAGED_VENV")
    else Sys.setenv(RETICULATE_USE_MANAGED_VENV = old_managed_venv)
  }, add = TRUE)

  td <- reticulate::import("truthdiscovery", delay_load = FALSE, convert = FALSE)

  triples <- reticulate::r_to_py(lapply(seq_len(nrow(votes_used)), function(i) {
    reticulate::tuple(votes_used$source_id[[i]], as.integer(votes_used$task_id[[i]]), votes_used$claimed_value[[i]])
  }))
  py_implication <- if (is.null(implication_function)) NULL else reticulate::r_to_py(implication_function)
  dataset <- tryCatch(
    td$Dataset(triples, allow_multiple = FALSE, implication_function = py_implication),
    error = function(e) stop(sprintf(
      "attribute_truth_fusion: truthdiscovery dataset construction failed: %s", conditionMessage(e)
    ), call. = FALSE)
  )

  iterator <- td$ConvergenceIterator(td$DistanceMeasures$L2, convergence_tolerance, limit = as.integer(max_iterations))
  estimator <- switch(method,
    sums = td$Sums(iterator = iterator),
    average_log = td$AverageLog(iterator = iterator),
    investment = td$Investment(iterator = iterator),
    pooled_investment = td$PooledInvestment(iterator = iterator),
    truthfinder = td$TruthFinder(iterator = iterator)
  )

  fitted <- tryCatch(
    estimator$run(dataset),
    error = function(e) {
      # Unlike crowd-kit's Dawid-Skene, truthdiscovery's ConvergenceIterator
      # raises instead of returning a partial Result once it exhausts
      # max_iterations without reaching convergence_tolerance -- there is no
      # trust/belief estimate at all to salvage, for any task or source, not
      # just a provisional one. Converting it to a warning (never silent)
      # plus an explicit all-abstained result keeps that honest, rather than
      # surfacing a generic stop() that looks like a real failure.
      if (inherits(e, "truthdiscovery.exceptions.ConvergenceError")) {
        warning(sprintf(paste0(
          "attribute_truth_fusion: truthdiscovery '%s' did not converge within ",
          "max_iterations = %d for attribute '%s' (tol = %s) -- no trust/belief estimate ",
          "was produced for any task; every prediction abstains."
        ), method, max_iterations, attribute_name, format(convergence_tolerance)), call. = FALSE)
        return(NULL)
      }
      stop(sprintf("attribute_truth_fusion: truthdiscovery '%s' failed to run: %s", method, conditionMessage(e)),
           call. = FALSE)
    }
  )

  if (is.null(fitted)) {
    # Not "no_single_value_sources" -- these tasks had sources, the fit
    # itself simply never produced a result at all.
    predictions_tbl <- tibble::tibble(
      entity_id = task_key_tbl$entity_id, attribute = attribute_name,
      reference_year = task_key_tbl$reference_year, predicted_value = NA_character_,
      method = method, model_score = NA_real_, score_type = "uncalibrated_belief",
      n_sources = vapply(task_key_tbl$task_id, function(id) {
        dplyr::n_distinct(votes_used$source_id[votes_used$task_id == id])
      }, integer(1)),
      n_sources_excluded = excluded_per_task[as.character(task_key_tbl$task_id)],
      n_candidate_values = NA_integer_, abstained = TRUE, abstention_reason = "model_did_not_converge"
    )
    return(list(
      predictions = predictions_tbl, candidate_scores = empty_candidate_rows(),
      source_diagnostics = tibble::tibble(
        source_id = character(0), attribute = character(0), true_label = character(0),
        observed_label = character(0), error_probability = numeric(0), reliability_score = numeric(0)
      ),
      converged = FALSE, n_iterations = max_iterations, n_disconnected_components = n_disconnected_components,
      backend_version = sprintf("python %s; truthdiscovery %s", probe$python_version, probe$truthdiscovery_version)
    ))
  }

  n_iterations_used <- reticulate::py_to_r(fitted$iterations)
  converged <- if (is.null(n_iterations_used) || is.na(n_iterations_used)) NA else n_iterations_used < max_iterations

  # Plain Python dicts of {str: float} (trust) and {int: {str: float}}
  # (belief) convert cleanly via py_to_r -- unlike crowd-kit's pandas
  # Series/DataFrame output, there is no sparse-numpy-string-dtype issue
  # here to work around.
  trust_by_source <- unlist(reticulate::py_to_r(fitted$trust))
  belief_by_task <- reticulate::py_to_r(fitted$belief)
  belief_for_task <- function(task_id) unlist(belief_by_task[[as.character(as.integer(task_id))]])

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

    belief_row <- belief_for_task(task_id)
    winner_idx <- which.max(belief_row)
    winner_value <- names(belief_row)[winner_idx]
    winner_score <- belief_row[winner_idx]
    is_tie <- sum(abs(belief_row - winner_score) < 1e-9) > 1L

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
      method = method, model_score = round(unname(winner_score), 4), score_type = "uncalibrated_belief",
      n_sources = dplyr::n_distinct(task_votes$source_id),
      n_sources_excluded = excluded_per_task[[as.character(task_id)]],
      n_candidate_values = length(local_tally), abstained = abstained, abstention_reason = abstention_reason
    )
    candidate_rows[[i]] <- tibble::tibble(
      entity_id = task_info$entity_id, attribute = attribute_name,
      reference_year = task_info$reference_year, candidate_value = names(local_tally),
      n_votes = as.integer(local_tally),
      vote_share = round(unname(belief_row[names(local_tally)]), 4),
      score_type = "uncalibrated_belief"
    )
  }

  source_ids_present <- sort(unique(votes_used$source_id))
  source_diagnostics_tbl <- tibble::tibble(
    source_id = source_ids_present, attribute = attribute_name, true_label = NA_character_,
    observed_label = NA_character_, error_probability = NA_real_,
    reliability_score = round(unname(trust_by_source[source_ids_present]), 4)
  )

  list(
    predictions = dplyr::bind_rows(prediction_rows),
    candidate_scores = dplyr::bind_rows(candidate_rows),
    source_diagnostics = source_diagnostics_tbl,
    converged = converged, n_iterations = if (is.null(n_iterations_used)) NA_integer_ else as.integer(n_iterations_used),
    n_disconnected_components = n_disconnected_components,
    backend_version = sprintf("python %s; truthdiscovery %s", probe$python_version, probe$truthdiscovery_version)
  )
}
