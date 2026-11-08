#' Manuscript-ready prose for an OIG-style directory audit
#'
#' Turns the outputs of this package's OIG-style audit functions --
#' [attribute_record_presence()], [attribute_set_agreement_summary()], and
#' optionally [attribute_enrollment_locations()] and
#' [attribute_nonresponse_audit()] -- into the kind of results paragraph a
#' directory-validity manuscript reports, the same way
#' [format_no_gold_standard_prose()] and [attribute_truth_fusion_prose()] do
#' for the package's other analyses. Every number in the paragraph is read
#' directly from the supplied tables; nothing is invented or estimated here.
#'
#' @details
#' **Presence and completeness denominators.** Source record counts include
#' only records marked `record_present` within the declared study frame.
#' Completeness uses `completeness_eligible` when supplied, checked against
#' presence and field availability. For older tables without that flag,
#' eligibility is derived from `record_present` and `field_status`, with
#' `field_available` checked when supplied. Unavailable extract fields are
#' excluded and described explicitly; a zero eligible denominator is
#' reported as not estimable. Duplicate physician-year-source-attribute
#' rows, inconsistent flags or statuses, and impossible counts are errors.
#' Comparable counts cannot exceed the number of shared physicians with
#' populated eligible fields; agreement proportions must match their counts.
#'
#' **No accuracy section.** The original drafted version of this paragraph
#' had an independently-verified-accuracy section backed by a function this
#' package never adopted, because its weighted bounds duplicated, less
#' rigorously, what [validity_estimates()] and [validity_sensitivity_bounds()]
#' already do with a real survey-design object. If a verified-accuracy
#' finding belongs in the same paragraph, compose it separately from those
#' two functions' output and append it to this function's return value.
#'
#' **No format-validity sentence.** [attribute_field_dictionary()]'s
#' `normalizer` column is deliberately metadata, not executable
#' transformation code, so this package has nothing that checks whether a
#' populated value is well-formed. The paragraph reports only whether a
#' field was populated, not whether the populated value passed a format
#' check.
#'
#' **Every optional section is independently supplied.** `linkage` and
#' `nonresponse` each add one sentence if supplied and are silently omitted
#' if `NULL` -- there is no cross-validation between them and the
#' completeness/agreement sections beyond matching on `year`, since they can
#' legitimately come from different sub-studies of the same audit.
#'
#' @param record_presence [attribute_record_presence()]'s output, covering
#'   at least `year`, `source_a`, and `source_b` for `attribute`.
#' @param set_agreement_summary [attribute_set_agreement_summary()]'s
#'   output, covering the same `year`/`source_a`/`source_b`.
#' @param year The audit year to report on.
#' @param source_a,source_b The two source labels to report on. Order does
#'   not matter -- `set_agreement_summary`'s row can list them in either
#'   order.
#' @param attribute The attribute (as it appears in `record_presence`'s
#'   `attribute` column) to report on.
#' @param attribute_label Optional human-readable label for `attribute` in
#'   the prose (for example `"practice ZIP codes"`). Defaults to `attribute`
#'   itself.
#' @param linkage Optional [attribute_enrollment_locations()]'s `links`
#'   element, filtered to `year` internally. `NULL` omits the linkage
#'   sentence.
#' @param nonresponse Optional [attribute_nonresponse_audit()]'s full
#'   output (the list with `providers` and `response_rates`), filtered to
#'   `year` internally. `NULL` omits the verification-response sentence.
#' @return A single character string: the paragraph, ready to paste into a
#'   manuscript.
#' @examples
#' record_presence <- data.frame(
#'   id = c("1", "2", "1", "2"), year = 2024, source = c("nppes", "nppes", "pecos", "pecos"),
#'   attribute = "practice_zip5", record_present = c(TRUE, TRUE, TRUE, FALSE),
#'   field_status = c("populated", "populated", "populated", "absent_from_declared_extract")
#' )
#' sources <- list(
#'   nppes = data.frame(npi = c("1", "2"), year = 2024, state = c("CO", "CO")),
#'   pecos = data.frame(npi = "1", year = 2024, state = "CO")
#' )
#' set_agreement <- attribute_set_agreement(sources, attribute = "state")
#' set_summary <- attribute_set_agreement_summary(set_agreement)
#' attribute_oig_audit_prose(
#'   record_presence, set_summary, year = 2024, source_a = "nppes", source_b = "pecos",
#'   attribute = "practice_zip5", attribute_label = "practice ZIP codes"
#' )
#' @seealso [attribute_record_presence()], [attribute_set_agreement_summary()],
#'   [attribute_enrollment_locations()], [attribute_nonresponse_audit()].
#' @family no-gold-standard comparison
#' @export
attribute_oig_audit_prose <- function(record_presence, set_agreement_summary, year, source_a, source_b,
                                        attribute, attribute_label = NULL, linkage = NULL, nonresponse = NULL) {
  checkmate::assert_data_frame(record_presence, min.rows = 1)
  checkmate::assert_data_frame(set_agreement_summary, min.rows = 1)
  checkmate::assert_string(source_a, min.chars = 1)
  checkmate::assert_string(source_b, min.chars = 1)
  checkmate::assert_string(attribute, min.chars = 1)
  checkmate::assert_atomic_vector(year, len = 1, any.missing = FALSE)
  if (identical(source_a, source_b)) {
    stop("attribute_oig_audit_prose: source_a and source_b must differ.", call. = FALSE)
  }

  require_cols <- function(df, cols, label) {
    missing_cols <- setdiff(cols, names(df))
    if (length(missing_cols) > 0) {
      stop(sprintf("attribute_oig_audit_prose: %s is missing column(s): %s",
                   label, paste(missing_cols, collapse = ", ")), call. = FALSE)
    }
  }
  require_cols(record_presence, c("id", "year", "source", "attribute", "record_present", "field_status"),
               "record_presence")
  require_cols(set_agreement_summary,
               c("source_a", "source_b", "year", "n_compared", "n_any_shared", "pct_any_shared",
                 "n_same_set", "pct_same_set"), "set_agreement_summary")

  if (is.null(attribute_label)) attribute_label <- attribute
  checkmate::assert_string(attribute_label, min.chars = 1)

  count_text <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
  pct_text <- function(numerator, denominator) {
    if (is.na(denominator) || denominator == 0) return("not estimable")
    sprintf("%.1f%%", 100 * numerator / denominator)
  }

  one_row <- function(df, label) {
    if (nrow(df) != 1L) stop(sprintf("attribute_oig_audit_prose: %s must resolve to exactly one row.", label),
                              call. = FALSE)
    df
  }

  keys <- record_presence[, base::c("id", "year", "source", "attribute"),
                          drop = FALSE]
  blank <- base::vapply(keys, function(value) {
    base::any(base::is.na(value) |
                !base::nzchar(base::trimws(base::as.character(value))))
  }, base::logical(1))
  if (base::any(blank) || base::anyDuplicated(keys) > 0L) {
    base::stop("record_presence has missing/blank or duplicate keys.",
               call. = FALSE)
  }
  checkmate::assert_logical(record_presence$record_present,
                           any.missing = FALSE)
  checkmate::assert_character(record_presence$field_status,
                             any.missing = FALSE)
  present_status <- base::c("populated", "field_row_missing", "field_value_missing",
                      "field_not_in_extract")
  absent_status <- base::c("absent_from_declared_extract",
                     "not_observed_in_incomplete_extract")
  if (base::any(!record_presence$field_status %in%
                  base::c(present_status, absent_status))) {
    base::stop("record_presence has an unknown field_status.", call. = FALSE)
  }
  if (base::any(record_presence$record_present !=
                  (record_presence$field_status %in% present_status))) {
    base::stop("record_presence has inconsistent presence and field_status.",
               call. = FALSE)
  }
  eligible <- record_presence$record_present &
    record_presence$field_status != "field_not_in_extract"
  if ("field_available" %in% base::names(record_presence)) {
    checkmate::assert_logical(record_presence$field_available,
                             any.missing = FALSE)
    if (base::any(eligible != (record_presence$record_present &
                               record_presence$field_available))) {
      base::stop("record_presence has inconsistent field_available flags.",
                 call. = FALSE)
    }
  }
  if ("completeness_eligible" %in% base::names(record_presence)) {
    checkmate::assert_logical(record_presence$completeness_eligible,
                             any.missing = FALSE)
    if (base::any(eligible != record_presence$completeness_eligible)) {
      base::stop("record_presence has inconsistent completeness_eligible.",
                 call. = FALSE)
    }
    eligible <- record_presence$completeness_eligible
  }
  record_presence$completeness_eligible <- eligible
  for (column in base::c("n_field_rows", "n_populated")) {
    if (column %in% base::names(record_presence)) {
      checkmate::assert_numeric(record_presence[[column]], lower = 0,
                               any.missing = FALSE, finite = TRUE)
      if (base::any(record_presence[[column]] !=
                      base::floor(record_presence[[column]]))) {
        base::stop("record_presence counts must be whole numbers.",
                   call. = FALSE)
      }
    }
  }
  if (base::all(base::c("n_field_rows", "n_populated") %in%
                base::names(record_presence))) {
    n_rows <- record_presence$n_field_rows
    n_pop <- record_presence$n_populated
    status <- record_presence$field_status
    inconsistent <- n_pop > n_rows | (!eligible & n_rows > 0) |
      (eligible & status == "field_row_missing" & n_rows != 0) |
      (eligible & status == "field_value_missing" &
         (n_rows == 0 | n_pop != 0)) |
      (status == "populated" & n_pop == 0)
    if (base::any(inconsistent)) {
      base::stop("record_presence has inconsistent counts and field_status.",
                 call. = FALSE)
    }
  }

  rp_side <- function(source) {
    row <- record_presence[record_presence$year == year &
                            record_presence$source == source &
                            record_presence$attribute == attribute, ]
    if (!base::nrow(row)) {
      base::stop(base::sprintf(
        base::paste0("attribute_oig_audit_prose: no record_presence rows for ",
               "source '%s', year %s, attribute '%s'."),
        source, year, attribute
      ), call. = FALSE)
    }
    base::list(n_present = base::sum(row$record_present),
         n_eligible = base::sum(row$completeness_eligible),
         n_unavailable = base::sum(row$record_present &
                                    row$field_status == "field_not_in_extract"),
         n_populated = base::sum(row$field_status == "populated"),
         populated_ids = row$id[row$completeness_eligible &
                                  row$field_status == "populated"])
  }
  a <- rp_side(source_a)
  b <- rp_side(source_b)

  pair_match <- (set_agreement_summary$source_a == source_a & set_agreement_summary$source_b == source_b) |
    (set_agreement_summary$source_a == source_b & set_agreement_summary$source_b == source_a)
  comparison <- one_row(set_agreement_summary[set_agreement_summary$year == year & pair_match, ],
                        "set_agreement_summary selection")

  for (column in base::c("n_compared", "n_any_shared", "n_same_set")) {
    checkmate::assert_number(comparison[[column]], lower = 0, finite = TRUE)
    if (comparison[[column]] != base::floor(comparison[[column]])) {
      base::stop("Agreement counts must be whole numbers.", call. = FALSE)
    }
  }
  if (comparison$n_any_shared > comparison$n_compared ||
      comparison$n_same_set > comparison$n_any_shared) {
    base::stop("Agreement counts are inconsistent.", call. = FALSE)
  }
  for (suffix in base::c("any_shared", "same_set")) {
    proportion <- comparison[[base::paste0("pct_", suffix)]]
    numerator <- comparison[[base::paste0("n_", suffix)]]
    if (comparison$n_compared == 0) {
      if (!base::is.na(proportion)) {
        base::stop("Agreement proportions must be NA with no comparisons.",
                   call. = FALSE)
      }
    } else {
      checkmate::assert_number(proportion, lower = 0, upper = 1,
                               finite = TRUE)
      if (base::abs(proportion -
                    numerator / comparison$n_compared) > 1e-8) {
        base::stop("Agreement counts and proportions are inconsistent.",
                   call. = FALSE)
      }
    }
  }

  if (comparison$n_compared > min(a$n_present, b$n_present)) {
    stop("attribute_oig_audit_prose: comparable count exceeds the lesser source's present-record count.",
         call. = FALSE)
  }

  n_shared <- base::length(base::intersect(a$populated_ids, b$populated_ids))
  if (comparison$n_compared > n_shared) {
    base::stop(
      base::paste0("Comparable count exceeds physicians with populated eligible ",
             "fields in both sources."),
      call. = FALSE
    )
  }

  sentences <- sprintf(
    base::paste0("In %s, %s and %s contained %s and %s physician records, respectively, within the declared study ",
           "frame. Among records present with the field available in each source, %s were populated in %s and %s, respectively."),
    year, toupper(source_a), toupper(source_b), count_text(a$n_present), count_text(b$n_present),
    attribute_label, pct_text(a$n_populated, a$n_eligible), pct_text(b$n_populated, b$n_eligible)
  )

  for (source in base::c(source_a, source_b)) {
    side <- if (source == source_a) a else b
    if (side$n_unavailable > 0) {
      sentences <- base::c(sentences, base::sprintf(
        base::paste0("In %s, %s were unavailable in the extract for %s present ",
               "physician records; these records were excluded from ",
               "the completeness denominator."),
        base::toupper(source), attribute_label, count_text(side$n_unavailable)
      ))
    }
  }

  if (comparison$n_compared > 0) {
    sentences <- base::c(sentences, sprintf(
      base::paste0("Among %s comparable physicians, at least one value matched in %s, whereas the complete reported ",
             "value sets agreed in %s."),
      count_text(comparison$n_compared), pct_text(comparison$n_any_shared, comparison$n_compared),
      pct_text(comparison$n_same_set, comparison$n_compared)
    ))
  } else {
    sentences <- base::c(sentences,
                   "Cross-source agreement was not estimable because no physicians had usable values in both sources.")
  }

  if (!is.null(linkage)) {
    require_cols(linkage, c("id", "year", "enrollment_id", "location_id", "linkage_path", "location_linked"),
                 "linkage")
    links <- linkage[linkage$year == year & linkage$location_linked, ]
    links <- unique(links[, c("id", "year", "enrollment_id", "location_id", "linkage_path")])
    n_direct <- sum(links$linkage_path == "direct_enrollment")
    n_group <- sum(links$linkage_path == "group_reassignment")
    sentences <- c(sentences, sprintf(
      paste0("Enrollment linkage identified %s direct and %s group-reassignment physician-enrollment-location ",
             "associations; these administrative associations did not establish clinical presence."),
      count_text(n_direct), count_text(n_group)
    ))
  }

  if (!is.null(nonresponse)) {
    require_cols(nonresponse$providers, c("id", "year", "response_status"), "nonresponse$providers")
    providers <- nonresponse$providers[nonresponse$providers$year == year, ]
    rates <- one_row(nonresponse$response_rates[nonresponse$response_rates$year == year, ],
                     "nonresponse$response_rates selection")
    sentences <- c(sentences, sprintf(
      paste0("Of %s sampled providers, %s completed verification, %s provided partial responses, and %s refused. ",
             "After excluding %s confirmed-ineligible providers and retaining unknown eligibility, the weighted ",
             "complete-response rate was %s."),
      count_text(nrow(providers)), count_text(sum(providers$response_status == "complete_response")),
      count_text(sum(providers$response_status == "partial_response")),
      count_text(sum(providers$response_status == "refused")), count_text(rates$n_confirmed_ineligible),
      pct_text(rates$w_complete, rates$w_denominator)
    ))
  }

  sentences <- c(sentences, "Cross-source disagreement was not classified as verified inaccuracy.")
  paste(sentences, collapse = " ")
}
