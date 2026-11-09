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
  samp <- data.frame(id = trimws(as.character(sample[[id]])), year = sample[[year]])
  cl <- data.frame(
    id = trimws(as.character(claims[[id]])), year = claims[[year]],
    source = trimws(as.character(claims$source)), attribute = trimws(as.character(claims$attribute)),
    value = claims$value
  )
  # Checked on the normalized (trimmed) frame, not the raw input -- otherwise
  # whitespace-padded duplicates (" 1 " vs "1") pass validation here and then
  # silently collide once trimws() above treats them as the same key.
  assert_key(samp, c("id", "year"), "sample")

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

#' Reconcile verification answers, contact attempts, and nonresponse
#'
#' Accuracy conditional on getting an answer is a different number from
#' accuracy adjusted for who never answered -- conflating the two silently
#' assumes nonresponse is random. This reconciles a frozen questionnaire
#' ([attribute_verification_questionnaire()]'s output) against whatever
#' answers and contact attempts actually came back, classifies every
#' sampled identifier-year into one exhaustive response status, and reports
#' a weighted response rate with a denominator that keeps unknown-eligibility
#' cases rather than silently dropping them -- without ever adjusting the
#' accuracy numbers themselves for nonresponse.
#'
#' @details
#' **Refusal is not incorrectness, and silence is not refusal.** A refused
#' contact contributes zero answered questions, the same as a provider never
#' reached at all -- but `response_status` keeps `"refused"` and
#' `"contacted_no_response"` apart, because a provider who refused was
#' reached and chose not to answer, which is a different nonresponse
#' mechanism than one nobody ever got through to. Neither counts as an
#' `incorrect` answer to any question -- `items$incorrect` is `TRUE` only for
#' a genuinely definitive incorrect answer.
#'
#' **The response-rate denominator is conservative.** `eligibility_status ==
#' "confirmed_ineligible"` is excluded from `w_denominator`; `"unknown"` is
#' kept in it. A provider merely *reported* retired or deceased by a contact
#' attempt does not become `confirmed_ineligible` here -- that is a
#' `contacts$outcome` fact, not an eligibility determination, and this
#' function never promotes one to the other on its own.
#'
#' **Three guards this function enforces that a careless hand-rolled
#' version easily misses:** an answer's `verified_at` cannot be in the
#' future (it cannot verify a period that hasn't happened yet); `questions`
#' cannot contain the same `(id, year, source, attribute, value)` assertion
#' twice under two different `question_id`s (a duplicate assertion would
#' double-count one claim as two questions); and `questions$value_missing`
#' must agree with whether `value` is actually blank (an inconsistent flag
#' would misclassify a field that genuinely has a value as missing, or vice
#' versa).
#'
#' @param sample Data frame of sampled identifier-years: `id`, `year`,
#'   `weight` (positive, finite), `eligibility_status` (one of `"eligible"`,
#'   `"unknown"`, `"confirmed_ineligible"`). Unique and non-missing on `(id,
#'   year)`.
#' @param questions [attribute_verification_questionnaire()]'s output (or
#'   anything with the same `id`, `year`, `source`, `attribute`, `value`,
#'   `question_id`, `value_missing` columns). `question_id` unique.
#' @param answers Data frame of answers received: `question_id`, `answer`
#'   (one of [attribute_verification_codebook()]'s `answer` values),
#'   `verified_at` (`Date`/`POSIXct`, non-missing, not in the future),
#'   `respondent_role` (non-blank). `question_id` unique -- one answer per
#'   question, never updated in place; a corrected answer replaces the row.
#' @param contacts Data frame of contact attempts: `id`, `year`,
#'   `contacted_at` (`Date`/`POSIXct`, non-missing), `channel` (non-blank),
#'   `outcome` (one of `"no_answer"`, `"voicemail"`, `"invalid_contact"`,
#'   `"refused"`, `"callback_requested"`, `"reached"`, `"retired_reported"`,
#'   `"deceased_reported"`). Multiple rows per identifier-year are expected
#'   (repeated attempts); no uniqueness is required.
#' @param id,year Names of the identifier and year columns on `sample` and
#'   `contacts` only -- `questions` always carries `id`/`year` as named by
#'   [attribute_verification_questionnaire()]. Defaults `"npi"`, `"year"`.
#' @return A list: `sample`, `questions`, `contacts` (passed through
#'   unchanged); `items` (`questions` left-joined to `answers` and the
#'   codebook, plus `answered`, `definitive`, `incorrect`); `providers`
#'   (`sample` plus `n_questions`, `n_answered`, `n_definitive`,
#'   `n_attempts`, `first_attempt`, `last_attempt`, `ever_refused`,
#'   `invalid_contact_reported`, `retired_reported`, `deceased_reported`,
#'   `response_status` -- one of `"complete_response"`, `"partial_response"`,
#'   `"refused"`, `"contacted_no_response"`, `"not_attempted"`); `flow`
#'   (`year`, `eligibility_status`, `response_status`, `n_providers`,
#'   `weighted_total`); `response_rates` (`year`, `n_sampled`,
#'   `n_confirmed_ineligible`, `w_denominator`, `w_complete`,
#'   `w_any_response`, `pct_complete`, `pct_any_response` -- the last two
#'   `NA` when `w_denominator` is `0`).
#' @examples
#' sample <- data.frame(
#'   npi = c("1", "2", "3"), year = 2026, weight = c(2, 3, 5), eligibility_status = "eligible"
#' )
#' claims <- data.frame(npi = sample$npi, year = 2026, source = "nppes",
#'                       attribute = "practice_zip5", value = "80204")
#' questions <- attribute_verification_questionnaire(sample, claims)
#' answers <- data.frame(
#'   question_id = questions$question_id[1], answer = "formerly_correct",
#'   verified_at = as.Date("2026-11-08"), respondent_role = "physician"
#' )
#' contacts <- data.frame(
#'   npi = sample$npi[1:2], year = 2026, contacted_at = as.Date("2026-11-08"),
#'   channel = "telephone", outcome = c("reached", "refused")
#' )
#' audit <- attribute_nonresponse_audit(sample, questions, answers, contacts)
#' audit$providers[, c("id", "response_status")]
#' audit$response_rates[, c("pct_complete", "pct_any_response")]
#' @seealso [attribute_verification_questionnaire()],
#'   [attribute_verification_codebook()].
#' @family no-gold-standard comparison
#' @export
attribute_nonresponse_audit <- function(sample, questions, answers, contacts, id = "npi", year = "year") {
  checkmate::assert_data_frame(sample, min.rows = 1)
  checkmate::assert_data_frame(questions, min.rows = 1)
  checkmate::assert_data_frame(answers)
  checkmate::assert_data_frame(contacts)
  checkmate::assert_string(id, min.chars = 1)
  checkmate::assert_string(year, min.chars = 1)

  require_cols <- function(df, cols, label) {
    missing_cols <- setdiff(cols, names(df))
    if (length(missing_cols) > 0) {
      stop(sprintf("attribute_nonresponse_audit: %s is missing column(s): %s",
                   label, paste(missing_cols, collapse = ", ")), call. = FALSE)
    }
  }
  require_cols(sample, c(id, year, "weight", "eligibility_status"), "sample")
  require_cols(questions, c("id", "year", "source", "attribute", "value", "question_id", "value_missing"),
               "questions")
  require_cols(answers, c("question_id", "answer", "verified_at", "respondent_role"), "answers")
  require_cols(contacts, c(id, year, "contacted_at", "channel", "outcome"), "contacts")

  assert_key <- function(df, cols, label) {
    keys <- df[, cols, drop = FALSE]
    blank <- vapply(keys, function(v) any(is.na(v) | !nzchar(trimws(as.character(v)))), logical(1))
    if (any(blank) || anyDuplicated(keys) > 0) {
      stop(sprintf("attribute_nonresponse_audit: %s has missing/blank or duplicate keys.",
                   label), call. = FALSE)
    }
  }
  assert_key(questions, "question_id", "questions")
  if (nrow(answers) > 0) assert_key(answers, "question_id", "answers")

  codebook <- attribute_verification_codebook()
  eligibility_levels <- c("eligible", "unknown", "confirmed_ineligible")
  outcome_levels <- c("no_answer", "voicemail", "invalid_contact", "refused", "callback_requested",
                      "reached", "retired_reported", "deceased_reported")

  if (!is.numeric(sample$weight) || any(!is.finite(sample$weight) | sample$weight <= 0)) {
    stop("attribute_nonresponse_audit: sample$weight must be finite and positive.", call. = FALSE)
  }
  if (any(!sample$eligibility_status %in% eligibility_levels)) {
    stop("attribute_nonresponse_audit: sample$eligibility_status must be eligible, unknown, or confirmed_ineligible.",
         call. = FALSE)
  }
  if (nrow(answers) > 0 && any(!answers$answer %in% codebook$answer)) {
    stop("attribute_nonresponse_audit: answers$answer must be one of attribute_verification_codebook()'s answers.",
         call. = FALSE)
  }
  if (nrow(contacts) > 0 && any(!contacts$outcome %in% outcome_levels)) {
    stop("attribute_nonresponse_audit: contacts$outcome must be a recognized outcome.", call. = FALSE)
  }

  dated <- function(x) inherits(x, "Date") || inherits(x, "POSIXct")
  if (nrow(answers) > 0) {
    if (!dated(answers$verified_at) || anyNA(answers$verified_at)) {
      stop("attribute_nonresponse_audit: answers$verified_at must have valid dates.", call. = FALSE)
    }
    if (any(as.Date(answers$verified_at) > Sys.Date())) {
      stop("attribute_nonresponse_audit: answers$verified_at cannot be in the future.", call. = FALSE)
    }
    if (anyNA(answers$respondent_role) || any(!nzchar(trimws(answers$respondent_role)))) {
      stop("attribute_nonresponse_audit: answers need a non-blank respondent/channel field.", call. = FALSE)
    }
  }
  if (nrow(contacts) > 0) {
    if (!dated(contacts$contacted_at) || anyNA(contacts$contacted_at)) {
      stop("attribute_nonresponse_audit: contacts$contacted_at must have valid dates.", call. = FALSE)
    }
    if (anyNA(contacts$channel) || any(!nzchar(trimws(contacts$channel)))) {
      stop("attribute_nonresponse_audit: answers need a non-blank respondent/channel field.", call. = FALSE)
    }
  }

  dup_assertion_key <- paste(questions$id, questions$year, questions$source, questions$attribute,
                             questions$value, sep = "\r")
  if (anyDuplicated(dup_assertion_key) > 0) {
    stop("attribute_nonresponse_audit: questions contains a duplicate assertion under two question_ids.",
         call. = FALSE)
  }
  actual_missing <- is.na(questions$value) | !nzchar(trimws(as.character(questions$value)))
  if (any(questions$value_missing != actual_missing)) {
    stop("attribute_nonresponse_audit: questions$value_missing is inconsistent with value.", call. = FALSE)
  }

  samp <- data.frame(
    id = trimws(as.character(sample[[id]])), year = sample[[year]],
    weight = sample$weight, eligibility_status = sample$eligibility_status
  )
  # Checked on the normalized (trimmed) frame, not the raw input -- otherwise
  # whitespace-padded duplicates pass validation here and then silently
  # collide once trimws() above treats them as the same key.
  assert_key(samp, c("id", "year"), "sample")
  qs <- questions
  contacts_norm <- data.frame(
    id = trimws(as.character(contacts[[id]])), year = contacts[[year]],
    contacted_at = contacts$contacted_at, channel = contacts$channel, outcome = contacts$outcome
  )

  samp_key <- paste(samp$id, samp$year, sep = "\r")
  q_key <- paste(qs$id, qs$year, sep = "\r")
  c_key <- paste(contacts_norm$id, contacts_norm$year, sep = "\r")
  if (nrow(answers) > 0 && any(!answers$question_id %in% qs$question_id)) {
    stop("attribute_nonresponse_audit: answers reference a question_id not in questions.", call. = FALSE)
  }
  if (any(!q_key %in% samp_key) || any(!c_key %in% samp_key) || any(!samp_key %in% q_key)) {
    stop("attribute_nonresponse_audit: sample, questions, and contacts keys do not reconcile.", call. = FALSE)
  }

  items <- merge(qs, answers, by = "question_id", all.x = TRUE)
  items <- merge(items, codebook, by = "answer", all.x = TRUE)
  items$answered <- !is.na(items$answer)
  items$definitive <- !items$value_missing & items$accuracy %in% c("correct", "incorrect")
  items$incorrect <- items$definitive & items$accuracy %in% "incorrect"
  items <- items[order(items$id, items$year, items$question_id), ]

  item_key <- paste(items$id, items$year, sep = "\r")
  item_idx <- split(seq_len(nrow(items)), item_key, drop = TRUE)
  provider_items <- dplyr::bind_rows(lapply(item_idx, function(rows) {
    data.frame(
      id = items$id[rows[1]], year = items$year[rows[1]],
      n_questions = length(rows), n_answered = sum(items$answered[rows]),
      n_definitive = sum(items$definitive[rows])
    )
  }))

  if (nrow(contacts_norm) > 0) {
    contact_idx <- split(seq_len(nrow(contacts_norm)), c_key, drop = TRUE)
    attempts <- dplyr::bind_rows(lapply(contact_idx, function(rows) {
      data.frame(
        id = contacts_norm$id[rows[1]], year = contacts_norm$year[rows[1]],
        n_attempts = length(rows),
        first_attempt = min(contacts_norm$contacted_at[rows]),
        last_attempt = max(contacts_norm$contacted_at[rows]),
        ever_refused = any(contacts_norm$outcome[rows] == "refused"),
        invalid_contact_reported = any(contacts_norm$outcome[rows] == "invalid_contact"),
        retired_reported = any(contacts_norm$outcome[rows] == "retired_reported"),
        deceased_reported = any(contacts_norm$outcome[rows] == "deceased_reported")
      )
    }))
  } else {
    attempts <- data.frame(
      id = character(0), year = samp$year[0], n_attempts = integer(0),
      first_attempt = contacts_norm$contacted_at[0], last_attempt = contacts_norm$contacted_at[0],
      ever_refused = logical(0), invalid_contact_reported = logical(0),
      retired_reported = logical(0), deceased_reported = logical(0)
    )
  }

  providers <- merge(samp, provider_items, by = c("id", "year"), all.x = TRUE)
  providers <- merge(providers, attempts, by = c("id", "year"), all.x = TRUE)
  providers$n_attempts[is.na(providers$n_attempts)] <- 0L
  providers$ever_refused[is.na(providers$ever_refused)] <- FALSE
  providers$response_status <- dplyr::case_when(
    providers$n_answered == providers$n_questions ~ "complete_response",
    providers$n_answered > 0L ~ "partial_response",
    providers$ever_refused ~ "refused",
    providers$n_attempts > 0L ~ "contacted_no_response",
    TRUE ~ "not_attempted"
  )
  providers <- providers[order(providers$id, providers$year), ]

  flow_key <- paste(providers$year, providers$eligibility_status, providers$response_status, sep = "\r")
  flow_idx <- split(seq_len(nrow(providers)), flow_key, drop = TRUE)
  flow <- dplyr::bind_rows(lapply(flow_idx, function(rows) {
    data.frame(
      year = providers$year[rows[1]], eligibility_status = providers$eligibility_status[rows[1]],
      response_status = providers$response_status[rows[1]],
      n_providers = length(rows), weighted_total = sum(providers$weight[rows])
    )
  }))
  flow <- flow[order(flow$year, flow$eligibility_status, flow$response_status), ]

  year_idx <- split(seq_len(nrow(providers)), as.character(providers$year), drop = TRUE)
  response_rates <- dplyr::bind_rows(lapply(year_idx, function(rows) {
    g <- providers[rows, ]
    elig <- g$eligibility_status != "confirmed_ineligible"
    w_denominator <- sum(g$weight[elig])
    w_complete <- sum(g$weight[elig & g$response_status == "complete_response"])
    w_any_response <- sum(g$weight[elig & g$n_answered > 0L])
    data.frame(
      year = g$year[1], n_sampled = nrow(g), n_confirmed_ineligible = sum(!elig),
      w_denominator = w_denominator, w_complete = w_complete, w_any_response = w_any_response,
      pct_complete = if (w_denominator > 0) 100 * w_complete / w_denominator else NA_real_,
      pct_any_response = if (w_denominator > 0) 100 * w_any_response / w_denominator else NA_real_
    )
  }))
  response_rates <- response_rates[order(response_rates$year), ]

  message(sprintf(
    "attribute_nonresponse_audit: %d sampled provider-year(s), %d contact attempt(s); no weights changed.",
    nrow(providers), nrow(contacts_norm)
  ))

  list(
    sample = tibble::as_tibble(sample), questions = tibble::as_tibble(questions),
    items = tibble::as_tibble(items), providers = tibble::as_tibble(providers),
    contacts = tibble::as_tibble(contacts), flow = tibble::as_tibble(flow),
    response_rates = tibble::as_tibble(response_rates)
  )
}
