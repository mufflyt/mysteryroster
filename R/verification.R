#' The six OIG-style verification response categories, mapped to accuracy
#'
#' A telephone or document-based directory audit asks a respondent to
#' classify one source's claimed value, not simply say "right" or "wrong."
#' `"formerly_correct"` and `"never_correct"` are both ultimately incorrect
#' for the stated reference period, but they are not the same finding --
#' one source was once right and went stale, the other was never right. This
#' table is the single place that maps each of the six response categories
#' a respondent can give to one of four coarser accuracy buckets.
#'
#' @return A tibble, one row per response category: `answer`, `meaning`,
#'   `accuracy` (one of `"correct"`, `"incorrect"`, `"unresolved"`,
#'   `"not_applicable"`).
#' @examples
#' attribute_verification_codebook()
#' @seealso [attribute_verification_questionnaire()], and the companion
#'   nonresponse-reconciliation function this codebook feeds.
#' @family no-gold-standard comparison
#' @export
attribute_verification_codebook <- function() {
  tibble::tribble(
    ~answer, ~meaning, ~accuracy,
    "correct", "Correct for the stated reference period", "correct",
    "partially_incorrect", "Contains a specific error", "incorrect",
    "formerly_correct", "Was correct, but not for the reference period", "incorrect",
    "never_correct", "Was never correct", "incorrect",
    "unable_to_verify", "Respondent cannot determine correctness", "unresolved",
    "not_applicable", "Field does not apply; document the reason", "not_applicable"
  )
}

#' One frozen verification question per distinct sampled assertion
#'
#' Independent verification needs a frozen record of exactly what was
#' asked, generated once and never regenerated after answers start coming
#' in -- otherwise a later change to the source claims could silently
#' reshuffle which question a given answer is reconciled against. This
#' builds that frozen table: one question per distinct `(id, year, source,
#' attribute, value)` assertion for every sampled identifier-year, including
#' a question for a missing value (never skipped, and never treated as
#' automatically wrong -- a respondent must say so explicitly).
#'
#' @details
#' **One question per distinct value, not per attribute.** An attribute that
#' legitimately carries more than one value from the same source in the
#' same identifier-year (the same multi-value case
#' [attribute_set_agreement()] compares) gets one question per distinct
#' value, each independently verifiable -- this does not assume an attribute
#' is single-valued.
#'
#' **Every sampled identifier-year needs at least one question.** If a
#' sampled identifier-year has no claim at all for some source/attribute
#' combination the study tracks, include an explicit row with a missing
#' `value` rather than omitting it -- an identifier-year silently absent
#' from `claims` would otherwise generate no question at all, which this
#' function treats as an error, not as "nothing to verify."
#'
#' @param sample Data frame of sampled identifier-years: `id`, `year`,
#'   unique and non-missing.
#' @param claims Data frame of raw assertions: `id`, `year`, `source`,
#'   `attribute`, `value` (character; `NA` or blank marks a missing value).
#' @param id,year Names of the identifier and year columns on both `sample`
#'   and `claims`. Defaults `"npi"`, `"year"`.
#' @return A tibble, one row per distinct sampled assertion: `id`, `year`,
#'   `source`, `attribute`, `value`, `question_id` (`"Q0000001"`-style,
#'   unique within this table only -- not a globally stable identifier),
#'   `value_missing`, `prompt`, `instructions`, and empty `corrected_value`,
#'   `valid_from`, `valid_to`, `notes` columns for a respondent to fill in.
#' @examples
#' sample <- data.frame(npi = c("1", "2"), year = 2024)
#' claims <- data.frame(
#'   npi = c("1", "2"), year = 2024, source = "nppes",
#'   attribute = "practice_zip5", value = c("80204", NA)
#' )
#' questions <- attribute_verification_questionnaire(sample, claims)
#' questions[, c("id", "value", "value_missing", "prompt")]
#' @seealso [attribute_verification_codebook()], and the companion
#'   nonresponse-reconciliation function that reconciles answers against
#'   this frozen table.
#' @family no-gold-standard comparison
#' @export
attribute_verification_questionnaire <- function(sample, claims, id = "npi", year = "year") {
  checkmate::assert_data_frame(sample, min.rows = 1)
  checkmate::assert_data_frame(claims, min.rows = 1)
  checkmate::assert_string(id, min.chars = 1)
  checkmate::assert_string(year, min.chars = 1)

  require_cols <- function(df, cols, label) {
    missing_cols <- setdiff(cols, names(df))
    if (length(missing_cols) > 0) {
      stop(sprintf("attribute_verification_questionnaire: %s is missing column(s): %s",
                   label, paste(missing_cols, collapse = ", ")), call. = FALSE)
    }
  }
  require_cols(sample, c(id, year), "sample")
  require_cols(claims, c(id, year, "source", "attribute", "value"), "claims")
  checkmate::assert_character(claims$value)

  assert_key <- function(df, cols, label) {
    keys <- df[, cols, drop = FALSE]
    blank <- vapply(keys, function(v) any(is.na(v) | !nzchar(trimws(as.character(v)))), logical(1))
    if (any(blank) || anyDuplicated(keys) > 0) {
      stop(sprintf("attribute_verification_questionnaire: %s has missing/blank or duplicate keys.",
                   label), call. = FALSE)
    }
  }
  assert_key(sample, c(id, year), "sample")

  samp <- data.frame(id = trimws(as.character(sample[[id]])), year = sample[[year]])
  cl <- data.frame(
    id = trimws(as.character(claims[[id]])), year = claims[[year]],
    source = trimws(as.character(claims$source)), attribute = trimws(as.character(claims$attribute)),
    value = claims$value
  )

  samp_key <- paste(samp$id, samp$year, sep = "\r")
  cl_key <- paste(cl$id, cl$year, sep = "\r")
  scoped <- unique(cl[cl_key %in% samp_key, c("id", "year", "source", "attribute", "value")])
  scoped <- scoped[order(scoped$id, scoped$year, scoped$source, scoped$attribute, scoped$value), ]
  rownames(scoped) <- NULL

  scoped$question_id <- sprintf("Q%07d", seq_len(nrow(scoped)))
  scoped$value_missing <- is.na(scoped$value) | !nzchar(trimws(scoped$value))
  scoped$prompt <- sprintf(
    "For reference year %s, %s records %s as: %s. Is this correct for that period?",
    scoped$year, scoped$source, scoped$attribute, ifelse(scoped$value_missing, "[missing]", scoped$value)
  )
  scoped$instructions <- ifelse(
    scoped$value_missing,
    "Document applicability and the missing value. Do not guess accuracy.",
    "Choose correct, partially_incorrect, formerly_correct, never_correct, unable_to_verify, or not_applicable. Record correction and dates."
  )
  scoped$corrected_value <- NA_character_
  scoped$valid_from <- as.Date(NA_character_)
  scoped$valid_to <- as.Date(NA_character_)
  scoped$notes <- NA_character_

  scoped_key <- paste(scoped$id, scoped$year, sep = "\r")
  if (any(!samp_key %in% scoped_key)) {
    stop(paste0("attribute_verification_questionnaire: every sampled identifier-year needs explicit ",
                "question row(s); include a missing-value claim row if there is nothing to verify."),
         call. = FALSE)
  }

  message(sprintf(
    "attribute_verification_questionnaire: %d frozen question(s); keep this exact table when entering answers.",
    nrow(scoped)
  ))

  tibble::as_tibble(scoped[, c("id", "year", "source", "attribute", "value", "question_id", "value_missing",
                                "prompt", "instructions", "corrected_value", "valid_from", "valid_to", "notes")])
}
