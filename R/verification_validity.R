#' Classify listed fields from independent verification
#'
#' @param audit Reconciled [attribute_nonresponse_audit()] object. Supply a
#'   `verification_method` column on the answers before reconciliation.
#' @param targets Data frame of unique source and attribute pairs. Every
#'   sampled physician-year is evaluated for each pair. Include every pair
#'   represented in the audit, then filter `validity` for analysis.
#' @param independent_methods Character vector of verification methods
#'   accepted as independent evidence by the study protocol.
#' @param id,year Identifier and year column names in the original sample.
#' @return A list with `fields` (all targets, with original sample columns),
#'   `validity` (eligible fields with a listing status), `exclusions`, and
#'   the original `items` for answer-level provenance. Analyze each
#'   source and attribute separately to preserve the sampling unit.
#' @details
#' An independently confirmed error makes the field invalid. A field is
#' valid only when every listed value is independently confirmed correct.
#' Missing, untrusted, or mixed evidence remains unresolved. Fields with
#' no listed value, entirely inapplicable fields, and confirmed ineligible
#' physicians are explicitly excluded. A physician without contact attempts
#' or answers has status `not_attempted` for otherwise eligible fields.
#' @examples
#' sample <- data.frame(
#'   npi = c("1", "2", "3"), year = 2026, weight = c(2, 3, 5), eligibility_status = "eligible"
#' )
#' claims <- data.frame(npi = sample$npi, year = 2026, source = "nppes",
#'                       attribute = "practice_zip5", value = "80204")
#' questions <- attribute_verification_questionnaire(sample, claims)
#' # verification_method is supplied on the answers, not required by
#' # attribute_nonresponse_audit() itself -- it passes through untouched.
#' answers <- data.frame(
#'   question_id = questions$question_id,
#'   answer = c("correct", "never_correct", "unable_to_verify"),
#'   verified_at = as.Date("2026-11-08"), respondent_role = "physician",
#'   verification_method = c("state_board_license_verification",
#'                           "state_board_license_verification", "self_report")
#' )
#' contacts <- data.frame(npi = sample$npi, year = 2026, contacted_at = as.Date("2026-11-08"),
#'                         channel = "telephone", outcome = "reached")
#' audit <- attribute_nonresponse_audit(sample, questions, answers, contacts)
#'
#' targets <- data.frame(source = "nppes", attribute = "practice_zip5")
#' status <- attribute_verification_status(
#'   audit, targets, independent_methods = "state_board_license_verification"
#' )
#' status$validity[, c("id", "listing_status", "unresolved_reasons")]
#' # id 1: confirmed_valid (independently verified correct)
#' # id 2: confirmed_invalid (independently verified incorrect)
#' # id 3: unresolved -- self_report isn't in independent_methods, so an
#' #       otherwise-definitive answer doesn't count as independent evidence
#' @seealso [validity_estimates()], [validity_sensitivity_bounds()].
#' @export
attribute_verification_status <- function(
    audit, targets, independent_methods, id = "npi", year = "year") {
  checkmate::assert_list(audit)
  checkmate::assert_string(id, min.chars = 1)
  checkmate::assert_string(year, min.chars = 1)
  checkmate::assert_character(independent_methods, min.len = 1,
                              any.missing = FALSE)
  independent_methods <- base::trimws(independent_methods)
  checkmate::assert_character(independent_methods, min.chars = 1, unique = TRUE)

  require_columns <- function(rows, columns, label, min_rows = 0L) {
    checkmate::assert_data_frame(rows, min.rows = min_rows)
    missing_columns <- base::setdiff(columns, base::names(rows))
    if (base::length(missing_columns)) {
      base::stop(label, " missing: ",
                 base::paste(missing_columns, collapse = ", "), call. = FALSE)
    }
  }
  person_keys <- base::c("id", "year")
  field_keys <- base::c("source", "attribute")
  target_keys <- base::c(person_keys, field_keys)
  assertion <- base::c(target_keys, "value", "value_missing")
  sample <- audit$sample
  items <- audit$items
  providers <- audit$providers
  require_columns(sample, base::c(id, year, "weight", "eligibility_status"),
                  "audit sample", 1L)
  require_columns(targets, field_keys, "targets", 1L)
  require_columns(providers, base::c(person_keys, "n_attempts"), "providers")
  require_columns(audit$questions, base::c("question_id", assertion),
                  "frozen questions")
  require_columns(items, base::c("question_id", assertion, "answer",
                                 "verified_at", "verification_method"),
                  "audit items")
  checkmate::assert_numeric(sample$weight, any.missing = FALSE, finite = TRUE)
  checkmate::assert_integerish(providers$n_attempts, lower = 0,
                               any.missing = FALSE)
  if (base::any(sample$weight <= 0) ||
      base::any(!sample$eligibility_status %in%
                base::c("eligible", "unknown", "confirmed_ineligible"))) {
    base::stop("Invalid sample weights or eligibility.", call. = FALSE)
  }
  item_question <- .audit_key(items, "question_id", "items")
  frozen_question <- .audit_key(audit$questions, "question_id", "questions")
  frozen_row <- base::match(item_question, frozen_question)
  if (base::anyNA(frozen_row) ||
      base::length(item_question) != base::length(frozen_question) ||
      !base::all(base::vapply(assertion, function(column) {
        base::identical(items[[column]],
                        audit$questions[[column]][frozen_row])
      }, base::logical(1)))) {
    base::stop("Items do not match the frozen questions.", call. = FALSE)
  }
  checkmate::assert_logical(items$value_missing, any.missing = FALSE)
  if (base::any(items$value_missing !=
                (base::is.na(items$value) |
                 !base::nzchar(base::trimws(items$value))))) {
    base::stop("Frozen value_missing flag is inconsistent.", call. = FALSE)
  }

  answered <- !base::is.na(items$answer)
  codebook <- attribute_verification_codebook()
  category <- codebook$accuracy[base::match(items$answer, codebook$answer)]
  if (base::anyNA(category[answered])) {
    base::stop("Items contain an unrecognized answer.", call. = FALSE)
  }
  dates <- items$verified_at[answered]
  if (base::length(dates)) {
    if (!base::inherits(dates, base::c("Date", "POSIXct")) ||
        base::any(!base::is.finite(base::as.numeric(dates)))) {
      base::stop("Answered items must have valid verification dates.",
                 call. = FALSE)
    }
    if (base::any(base::as.Date(dates) > base::Sys.Date())) {
      base::stop("Verification dates cannot be in the future.", call. = FALSE)
    }
  }
  method <- base::trimws(base::as.character(items$verification_method))
  trusted <- answered & method %in% independent_methods

  sample_key <- .audit_key(sample, base::c(id, year), "sample")
  target_key <- .audit_key(targets, field_keys, "targets")
  item_person <- .audit_key(items, person_keys, "items", unique = FALSE)
  item_target <- .audit_key(items, field_keys, "items", unique = FALSE)
  provider_key <- .audit_key(providers, person_keys, "providers")
  if (!base::setequal(sample_key, provider_key) ||
      !base::all(item_person %in% sample_key) ||
      !base::all(item_target %in% target_key)) {
    base::stop("Sample, providers, items and targets do not reconcile.",
               call. = FALSE)
  }

  sample_row <- base::rep(base::seq_len(base::nrow(sample)),
                          each = base::nrow(targets))
  target_row <- base::rep(base::seq_len(base::nrow(targets)),
                          times = base::nrow(sample))
  fields <- tibble::as_tibble(sample[sample_row, , drop = FALSE])
  added <- base::c("source", "attribute", "n_attempts", "listing_status",
                  "exclusion_reason", "question_ids", "verification_methods",
                  "verification_dates", "unresolved_reasons")
  canonical <- base::setdiff(person_keys, base::c(id, year))
  if (base::any(base::c(added, canonical) %in% base::names(sample)) ||
      id == year || id == "year" || year == "id") {
    base::stop("Sample columns collide with verification columns.",
               call. = FALSE)
  }
  fields$id <- base::trimws(base::as.character(sample[[id]][sample_row]))
  fields$year <- .audit_year(sample[[year]][sample_row])
  for (column in field_keys) {
    fields[[column]] <- base::trimws(
      base::as.character(targets[[column]][target_row])
    )
  }
  fields$n_attempts <- providers$n_attempts[
    base::match(sample_key[sample_row], provider_key)
  ]
  attempted <- fields$n_attempts > 0 |
    sample_key[sample_row] %in% item_person[answered]
  grid_key <- .audit_key(fields, target_keys, "verification grid")
  item_key <- .audit_key(items, target_keys, "items", unique = FALSE)
  groups <- base::split(base::seq_len(base::nrow(items)),
                        base::match(item_key, grid_key))
  classify <- function(index) {
    rows <- groups[[base::as.character(index)]]
    listed <- rows[!items$value_missing[rows]]
    eligible <- fields$eligibility_status[index] != "confirmed_ineligible"
    supported <- trusted[listed]
    responses <- category[listed]
    exclusion <- if (!eligible) {
      "confirmed_ineligible"
    } else if (!base::length(rows)) {
      "no_listed_value"
    } else if (!base::length(listed)) {
      "all_missing_values"
    } else if (base::all(supported & responses %in% "not_applicable")) {
      "not_applicable"
    } else {
      NA_character_
    }
    status <- if (!base::is.na(exclusion)) {
      NA_character_
    } else if (base::any(supported & responses %in% "incorrect")) {
      "confirmed_invalid"
    } else if (base::all(supported & responses %in% "correct")) {
      "confirmed_valid"
    } else if (!attempted[index]) {
      "not_attempted"
    } else {
      "unresolved"
    }
    no_method <- base::is.na(method[listed]) |
      !base::nzchar(method[listed])
    reasons <- base::c(
      unanswered = base::any(!answered[listed]),
      missing_verification_method = base::any(answered[listed] & no_method),
      unaccepted_verification_method = base::any(
        answered[listed] & !no_method & !supported
      ),
      unable_to_verify = base::any(responses %in% "unresolved"),
      mixed_applicability = base::any(responses %in% "not_applicable")
    )
    tibble::tibble(
      listing_status = status, exclusion_reason = exclusion,
      question_ids = base::list(items$question_id[rows]),
      verification_methods = base::list(base::unique(method[rows][
        answered[rows] & !base::is.na(method[rows]) &
          base::nzchar(method[rows])
      ])),
      verification_dates = base::list(items$verified_at[rows][answered[rows]]),
      unresolved_reasons = base::list(
        if (base::identical(status, "unresolved")) {
          base::names(reasons)[reasons]
        } else {
          base::character()
        }
      )
    )
  }
  classifications <- dplyr::bind_rows(
    base::lapply(base::seq_len(base::nrow(fields)), classify)
  )
  fields <- dplyr::bind_cols(fields, classifications)
  keep <- base::is.na(fields$exclusion_reason)
  base::message("attribute_verification_status: ",
                base::format(base::nrow(items), big.mark = ","), " items, ",
                base::format(base::sum(keep), big.mark = ","), " eligible, ",
                base::format(base::sum(!keep), big.mark = ","), " excluded.")
  base::list(fields = fields, validity = fields[keep, ],
             exclusions = fields[!keep, ], items = items)
}
