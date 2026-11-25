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
#' @param provider_noun Singular noun used for audited clinicians in the
#'   narrative (for example `"physician"`, `"nurse-midwife"`, or `"clinician"`).
#'   Defaults to `"physician"`.
#' @param provider_plural Optional plural noun corresponding to `provider_noun`
#'   (for example `"physicians"` or `"nurse-midwives"`). When `NULL`, derived
#'   automatically from `provider_noun` (converting a trailing `"midwife"` to
#'   `"midwives"` and otherwise appending `"s"`).
#' @param verification_mode Either `"survey"` (the default, reporting
#'   respondent survey completion, partial response, refusal, and weighted
#'   response rates) or `"administrative_registry"` (reporting deterministic
#'   administrative registry record matching and explicitly noting that
#'   file-based registry linkage is not a respondent survey).
#' @param location_specificity Optional output list from
#'   [attribute_location_specificity()] providing clinician-level direct vs
#'   billing-group location specificity and plain-English summary prose. When
#'   supplied, appends `location_specificity$plain_english_summary` to the
#'   narrative. Default `NULL`.
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
#'   [attribute_enrollment_locations()], [attribute_location_specificity()],
#'   [attribute_nonresponse_audit()].
#' @family no-gold-standard comparison
#' @export
attribute_oig_audit_prose <- function(record_presence, set_agreement_summary, year, source_a, source_b,
                                        attribute, attribute_label = NULL, linkage = NULL, nonresponse = NULL,
                                        provider_noun = "physician", provider_plural = NULL,
                                        verification_mode = c("survey", "administrative_registry"),
                                        location_specificity = NULL) {
  checkmate::assert_data_frame(record_presence, min.rows = 1)
  checkmate::assert_data_frame(set_agreement_summary, min.rows = 1)
  checkmate::assert_string(source_a, min.chars = 1)
  checkmate::assert_string(source_b, min.chars = 1)
  checkmate::assert_string(attribute, min.chars = 1)
  checkmate::assert_atomic_vector(year, len = 1, any.missing = FALSE)
  checkmate::assert_string(provider_noun, min.chars = 1)
  if (base::is.null(provider_plural)) {
    provider_plural <- if (base::grepl("midwife$", provider_noun)) {
      base::sub("midwife$", "midwives", provider_noun)
    } else {
      base::paste0(provider_noun, "s")
    }
  } else {
    checkmate::assert_string(provider_plural, min.chars = 1)
  }
  verification_mode <- base::match.arg(verification_mode)
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
  noncanonical <- base::vapply(keys, function(value) {
    text <- base::as.character(value)
    base::any(text != base::trimws(text), na.rm = TRUE)
  }, base::logical(1))
  if (base::any(noncanonical)) {
    base::stop("record_presence keys have noncanonical whitespace.",
               call. = FALSE)
  }
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
    base::paste0("In %s, %s and %s contained %s and %s %s records, respectively, within the declared study ",
           "frame. Among records present with the field available in each source, %s were populated in %s and %s, respectively."),
    year, toupper(source_a), toupper(source_b), count_text(a$n_present), count_text(b$n_present), provider_noun,
    attribute_label, pct_text(a$n_populated, a$n_eligible), pct_text(b$n_populated, b$n_eligible)
  )

  for (source in base::c(source_a, source_b)) {
    side <- if (source == source_a) a else b
    if (side$n_unavailable > 0) {
      sentences <- base::c(sentences, base::sprintf(
        base::paste0("In %s, %s were unavailable in the extract for %s present ",
               "%s records; these records were excluded from ",
               "the completeness denominator."),
        base::toupper(source), attribute_label, count_text(side$n_unavailable), provider_noun
      ))
    }
  }

  if (comparison$n_compared > 0) {
    sentences <- base::c(sentences, sprintf(
      base::paste0("Among %s comparable %s, at least one value matched in %s, whereas the complete reported ",
             "value sets agreed in %s."),
      count_text(comparison$n_compared), provider_plural, pct_text(comparison$n_any_shared, comparison$n_compared),
      pct_text(comparison$n_same_set, comparison$n_compared)
    ))
  } else {
    sentences <- base::c(sentences, base::sprintf(
      "Cross-source agreement was not estimable because no %s had usable values in both sources.",
      provider_plural
    ))
  }

  if (!is.null(linkage)) {
    require_cols(linkage, c("id", "year", "enrollment_id", "location_id", "linkage_path", "location_linked"),
                 "linkage")
    links <- linkage[linkage$year == year & linkage$location_linked, ]
    links <- unique(links[, c("id", "year", "enrollment_id", "location_id", "linkage_path")])
    n_direct <- sum(links$linkage_path == "direct_enrollment")
    n_group <- sum(links$linkage_path == "group_reassignment")
    sentences <- c(sentences, sprintf(
      paste0("Enrollment linkage identified %s direct and %s group-reassignment %s-enrollment-location ",
             "associations; these administrative associations did not establish clinical presence."),
      count_text(n_direct), count_text(n_group), provider_noun
    ))
  }

  if (!is.null(location_specificity)) {
    checkmate::assert_list(location_specificity)
    if (!"plain_english_summary" %in% names(location_specificity)) {
      stop("attribute_oig_audit_prose: location_specificity must contain 'plain_english_summary'.", call. = FALSE)
    }
    checkmate::assert_string(location_specificity$plain_english_summary, min.chars = 1)
    sentences <- c(sentences, location_specificity$plain_english_summary)
  }

  if (!is.null(nonresponse)) {
    require_cols(nonresponse$providers, c("id", "year", "response_status"), "nonresponse$providers")
    providers <- nonresponse$providers[nonresponse$providers$year == year, ]
    rates <- one_row(nonresponse$response_rates[nonresponse$response_rates$year == year, ],
                     "nonresponse$response_rates selection")
    if (verification_mode == "administrative_registry") {
      sentences <- c(sentences, sprintf(
        paste0("In deterministic administrative registry linkage across %s audited %s, ",
               "%s yielded a complete registry disposition (%s partial, %s unresolved/unmatched; ",
               "weighted registry disposition rate %s after excluding %s confirmed-ineligible records; ",
               "file-based registry matching does not constitute a respondent survey)."),
        count_text(nrow(providers)), provider_plural,
        count_text(sum(providers$response_status == "complete_response")),
        count_text(sum(providers$response_status == "partial_response")),
        count_text(sum(!providers$response_status %in% c("complete_response", "partial_response"))),
        pct_text(rates$w_complete, rates$w_denominator),
        count_text(rates$n_confirmed_ineligible)
      ))
    } else {
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
  }

  sentences <- c(sentences, "Cross-source disagreement was not classified as verified inaccuracy.")
  paste(sentences, collapse = " ")
}

.oig_html_escape <- function(x) {
  x <- base::as.character(x)
  x[base::is.na(x)] <- ""
  x <- base::gsub("&", "&amp;", x, fixed = TRUE)
  x <- base::gsub("<", "&lt;", x, fixed = TRUE)
  x <- base::gsub(">", "&gt;", x, fixed = TRUE)
  x <- base::gsub("\"", "&quot;", x, fixed = TRUE)
  x <- base::gsub("'", "&#39;", x, fixed = TRUE)
  x
}

#' Self-contained HTML report for an OIG-style directory audit
#'
#' Assembles a standalone, publication-styled HTML5 report combining the
#' narrative prose from [attribute_oig_audit_prose()], an inline SVG
#' study-frame and denominator flow diagram, visual horizontal stacked bar
#' charts, and formatted HTML tables for OIG Table 3 (Completeness), OIG
#' Table 4 (Cross-Source Set Agreement & Inconsistency), and optional modules
#' for Appendix A ([attribute_enrollment_locations()]), OIG Tables 1--2
#' ([attribute_nonresponse_audit()], [validity_estimates()], and
#' [validity_sensitivity_bounds()]), and longitudinal update-lag resolution
#' ([attribute_lead_lag_duration()]).
#'
#' @inheritParams attribute_oig_audit_prose
#' @param validity_estimate Optional one-row data frame returned by
#'   [validity_estimates()] reporting the weighted definitive validity
#'   estimate and confidence interval (`estimate`, `se`, `n`, `ci_low`,
#'   `ci_high`).
#' @param validity_bounds Optional data frame returned by
#'   [validity_sensitivity_bounds()] reporting worst-case, point, and
#'   best-case validity bounds (`scenario`, `pct_valid`, `ci_low`, `ci_high`).
#' @param lead_lag Optional list returned by [attribute_lead_lag_duration()]
#'   reporting longitudinal discordance resolution (`base_year`, `disc_base_n`,
#'   `b_adopts`, `a_reverts`, `combined_text`).
#' @param title Optional report title string. Defaults to an OIG-style header
#'   naming `source_a`, `source_b`, and `year`.
#' @param file Optional file path where the self-contained HTML document should
#'   be written. When `NULL` (the default), no file is written and the HTML
#'   string is returned visibly; when a path is supplied, parent directories
#'   are created if needed, the file is written in UTF-8, and the HTML string
#'   is returned invisibly.
#' @return A single character string containing the complete `<!DOCTYPE html>`
#'   document (returned invisibly when `file` is non-`NULL`).
#' @examples
#' record_presence <- data.frame(
#'   id = c("1", "2", "1", "2"), year = 2024,
#'   source = c("nppes", "nppes", "pecos", "pecos"),
#'   attribute = "practice_zip5", record_present = c(TRUE, TRUE, TRUE, FALSE),
#'   field_status = c("populated", "populated", "populated", "absent_from_declared_extract")
#' )
#' sources <- list(
#'   nppes = data.frame(npi = c("1", "2"), year = 2024, state = c("CO", "CO")),
#'   pecos = data.frame(npi = "1", year = 2024, state = "CO")
#' )
#' set_agreement <- attribute_set_agreement(sources, attribute = "state")
#' set_summary <- attribute_set_agreement_summary(set_agreement)
#' html <- attribute_oig_audit_html(
#'   record_presence, set_summary, year = 2024, source_a = "nppes",
#'   source_b = "pecos", attribute = "practice_zip5",
#'   attribute_label = "practice ZIP codes"
#' )
#' substr(html, 1, 15)
#' @seealso [attribute_oig_audit_prose()], [attribute_record_presence()],
#'   [attribute_set_agreement_summary()], [attribute_enrollment_locations()],
#'   [attribute_nonresponse_audit()], [validity_estimates()],
#'   [validity_sensitivity_bounds()], [attribute_lead_lag_duration()].
#' @family no-gold-standard comparison
#' @export
attribute_oig_audit_html <- function(record_presence, set_agreement_summary,
                                     year, source_a, source_b, attribute,
                                     attribute_label = NULL, linkage = NULL,
                                     nonresponse = NULL,
                                     provider_noun = "physician",
                                     provider_plural = NULL,
                                     verification_mode = c("survey", "administrative_registry"),
                                     location_specificity = NULL,
                                     validity_estimate = NULL,
                                     validity_bounds = NULL,
                                     lead_lag = NULL,
                                     title = NULL,
                                     file = NULL) {
  prose <- attribute_oig_audit_prose(
    record_presence = record_presence,
    set_agreement_summary = set_agreement_summary,
    year = year,
    source_a = source_a,
    source_b = source_b,
    attribute = attribute,
    attribute_label = attribute_label,
    linkage = linkage,
    nonresponse = nonresponse,
    provider_noun = provider_noun,
    provider_plural = provider_plural,
    verification_mode = verification_mode,
    location_specificity = location_specificity
  )

  if (base::is.null(attribute_label)) attribute_label <- attribute
  if (base::is.null(title)) {
    title <- base::sprintf(
      "OIG-Style Provider Directory Audit: %s vs. %s (%s)",
      base::toupper(source_a), base::toupper(source_b), base::as.character(year)
    )
  } else {
    checkmate::assert_string(title, min.chars = 1)
  }
  if (!base::is.null(file)) {
    checkmate::assert_string(file, min.chars = 1)
  }

  require_cols <- function(df, cols, label) {
    missing_cols <- base::setdiff(cols, base::names(df))
    if (base::length(missing_cols) > 0L) {
      base::stop(base::sprintf(
        "attribute_oig_audit_html: %s is missing column(s): %s",
        label, base::paste(missing_cols, collapse = ", ")
      ), call. = FALSE)
    }
  }

  if (!base::is.null(validity_estimate)) {
    checkmate::assert_data_frame(validity_estimate, nrows = 1L)
    require_cols(
      validity_estimate,
      base::c("estimate", "se", "n", "ci_low", "ci_high"),
      "validity_estimate"
    )
  }
  if (!base::is.null(validity_bounds)) {
    checkmate::assert_data_frame(validity_bounds, min.rows = 1L)
    require_cols(
      validity_bounds,
      base::c("scenario", "pct_valid"),
      "validity_bounds"
    )
  }
  if (!base::is.null(lead_lag)) {
    checkmate::assert_list(lead_lag)
    if (!("combined_text" %in% base::names(lead_lag))) {
      base::stop(
        "attribute_oig_audit_html: lead_lag must contain 'combined_text'.",
        call. = FALSE
      )
    }
    checkmate::assert_string(lead_lag$combined_text, min.chars = 1)
  }

  count_text <- function(x) {
    base::format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
  }
  pct_num <- function(numerator, denominator) {
    if (base::is.na(denominator) || denominator == 0) return(0)
    100 * numerator / denominator
  }
  pct_text <- function(numerator, denominator) {
    if (base::is.na(denominator) || denominator == 0) return("not estimable")
    base::sprintf("%.1f%%", 100 * numerator / denominator)
  }

  eligible <- record_presence$record_present &
    record_presence$field_status != "field_not_in_extract"
  if ("completeness_eligible" %in% base::names(record_presence)) {
    eligible <- record_presence$completeness_eligible
  }
  record_presence$completeness_eligible <- eligible

  summarize_side <- function(src, attr_name) {
    rows <- record_presence[
      record_presence$year == year &
        record_presence$source == src &
        record_presence$attribute == attr_name, , drop = FALSE
    ]
    n_frame <- base::nrow(rows)
    n_present <- base::sum(rows$record_present)
    n_absent <- n_frame - n_present
    n_eligible <- base::sum(rows$completeness_eligible)
    n_populated <- base::sum(rows$field_status == "populated")
    n_row_missing <- base::sum(rows$field_status == "field_row_missing")
    n_val_missing <- base::sum(rows$field_status == "field_value_missing")
    n_unavail <- base::sum(rows$record_present &
                             rows$field_status == "field_not_in_extract")
    base::list(
      source = src,
      attribute = attr_name,
      n_frame = n_frame,
      n_present = n_present,
      n_absent = n_absent,
      n_eligible = n_eligible,
      n_populated = n_populated,
      n_row_missing = n_row_missing,
      n_val_missing = n_val_missing,
      n_unavail = n_unavail,
      pct_populated_eligible = pct_num(n_populated, n_eligible),
      pct_populated_frame = pct_num(n_populated, n_frame),
      pct_absent_frame = pct_num(n_absent, n_frame),
      pct_missing_field_frame = pct_num(n_row_missing + n_val_missing, n_frame)
    )
  }

  side_a <- summarize_side(source_a, attribute)
  side_b <- summarize_side(source_b, attribute)

  all_attrs <- base::unique(record_presence$attribute[
    record_presence$year == year &
      record_presence$source %in% base::c(source_a, source_b)
  ])
  all_attrs <- base::c(attribute, base::setdiff(all_attrs, attribute))

  completeness_rows_html <- base::character(0)
  completeness_bars_html <- base::character(0)
  for (attr_i in all_attrs) {
    for (src_i in base::c(source_a, source_b)) {
      sub_rows <- record_presence[
        record_presence$year == year &
          record_presence$source == src_i &
          record_presence$attribute == attr_i, , drop = FALSE
      ]
      if (base::nrow(sub_rows) == 0L) next
      s_i <- summarize_side(src_i, attr_i)
      lbl_i <- if (attr_i == attribute) attribute_label else attr_i
      completeness_rows_html <- base::c(
        completeness_rows_html,
        base::sprintf(
          paste0(
            "<tr>",
            "<td><strong>%s</strong></td>",
            "<td>%s</td>",
            "<td class=\"num\">%s</td>",
            "<td class=\"num\">%s</td>",
            "<td class=\"num\">%s (%s)</td>",
            "<td class=\"num\">%s</td>",
            "<td class=\"num\">%s</td>",
            "<td class=\"num\">%s</td>",
            "<td class=\"num\">%s</td>",
            "</tr>"
          ),
          .oig_html_escape(base::toupper(src_i)),
          .oig_html_escape(lbl_i),
          count_text(s_i$n_frame),
          count_text(s_i$n_present),
          count_text(s_i$n_populated),
          pct_text(s_i$n_populated, s_i$n_eligible),
          count_text(s_i$n_absent),
          count_text(s_i$n_row_missing),
          count_text(s_i$n_val_missing),
          count_text(s_i$n_unavail)
        )
      )
      w_pop <- base::sprintf("%.1f", s_i$pct_populated_frame)
      w_miss <- base::sprintf("%.1f", s_i$pct_missing_field_frame)
      w_abs <- base::sprintf("%.1f", s_i$pct_absent_frame)
      completeness_bars_html <- base::c(
        completeness_bars_html,
        base::sprintf(
          paste0(
            "<div class=\"bar-row\">",
            "<div class=\"bar-label\"><strong>%s</strong> &middot; %s</div>",
            "<div class=\"bar-track\">",
            "<div class=\"bar-seg seg-ok\" style=\"width:%s%%\" title=\"Populated: %s\"></div>",
            "<div class=\"bar-seg seg-warn\" style=\"width:%s%%\" title=\"Field Missing: %s\"></div>",
            "<div class=\"bar-seg seg-err\" style=\"width:%s%%\" title=\"Record Absent: %s\"></div>",
            "</div>",
            "<div class=\"bar-val\">%s eligible pop.</div>",
            "</div>"
          ),
          .oig_html_escape(base::toupper(src_i)),
          .oig_html_escape(lbl_i),
          w_pop, count_text(s_i$n_populated),
          w_miss, count_text(s_i$n_row_missing + s_i$n_val_missing),
          w_abs, count_text(s_i$n_absent),
          pct_text(s_i$n_populated, s_i$n_eligible)
        )
      )
    }
  }

  pair_match <- (set_agreement_summary$source_a == source_a &
                   set_agreement_summary$source_b == source_b) |
    (set_agreement_summary$source_a == source_b &
       set_agreement_summary$source_b == source_a)
  comp <- set_agreement_summary[
    set_agreement_summary$year == year & pair_match, , drop = FALSE
  ][1L, , drop = FALSE]

  n_comp <- comp$n_compared
  n_any <- comp$n_any_shared
  n_same <- comp$n_same_set
  n_no_shared <- n_comp - n_any
  n_diff_set <- n_comp - n_same
  pct_any_val <- if (n_comp > 0) 100 * comp$pct_any_shared else 0
  pct_same_val <- if (n_comp > 0) 100 * comp$pct_same_set else 0
  pct_inconsist_any <- if (n_comp > 0) 100 - pct_any_val else 0
  pct_inconsist_same <- if (n_comp > 0) 100 - pct_same_val else 0

  svg_link_line <- if (!base::is.null(linkage)) {
    links_yr <- linkage[linkage$year == year & linkage$location_linked, , drop = FALSE]
    links_yr <- base::unique(links_yr[, base::c("id", "year", "enrollment_id", "location_id", "linkage_path"), drop = FALSE])
    base::sprintf(
      "Direct: %s | Reassigned: %s",
      count_text(base::sum(links_yr$linkage_path == "direct_enrollment")),
      count_text(base::sum(links_yr$linkage_path == "group_reassignment"))
    )
  } else {
    "Linkage table not supplied"
  }

  svg_verif_line <- if (!base::is.null(nonresponse)) {
    prov_yr <- nonresponse$providers[nonresponse$providers$year == year, , drop = FALSE]
    rates_yr <- nonresponse$response_rates[nonresponse$response_rates$year == year, , drop = FALSE][1L, , drop = FALSE]
    base::sprintf(
      "Sampled: %s | Weighted resp: %s",
      count_text(base::nrow(prov_yr)),
      pct_text(rates_yr$w_complete, rates_yr$w_denominator)
    )
  } else if (!base::is.null(validity_estimate)) {
    base::sprintf(
      "Verified n=%s | Valid: %.1f%%",
      count_text(validity_estimate$n),
      100 * validity_estimate$estimate
    )
  } else {
    "Verification audit optional"
  }

  svg_diagram <- base::sprintf(
    paste0(
      "<svg class=\"oig-flow-svg\" viewBox=\"0 0 880 230\" xmlns=\"http://www.w3.org/2000/svg\" role=\"img\" ",
      "aria-label=\"OIG directory audit study frame and denominator flow diagram\">",
      "<defs>",
      "<marker id=\"oig-arrow\" viewBox=\"0 0 10 10\" refX=\"8\" refY=\"5\" markerWidth=\"6\" markerHeight=\"6\" orient=\"auto-start-reverse\">",
      "<path d=\"M 0 1 L 10 5 L 0 9 z\" fill=\"#475569\"/>",
      "</marker>",
      "</defs>",
      "<rect x=\"12\" y=\"20\" width=\"195\" height=\"185\" rx=\"10\" fill=\"#f8fafc\" stroke=\"#cbd5e1\" stroke-width=\"1.5\"/>",
      "<rect x=\"12\" y=\"20\" width=\"195\" height=\"32\" rx=\"10\" fill=\"#1e293b\"/>",
      "<text x=\"109\" y=\"41\" text-anchor=\"middle\" fill=\"#ffffff\" font-size=\"12\" font-weight=\"700\">1. Study Frame (%s)</text>",
      "<text x=\"24\" y=\"76\" fill=\"#0f172a\" font-size=\"12\" font-weight=\"600\">%s Records:</text>",
      "<text x=\"195\" y=\"76\" text-anchor=\"end\" fill=\"#0f172a\" font-size=\"12\">%s / %s</text>",
      "<text x=\"24\" y=\"100\" fill=\"#0f172a\" font-size=\"12\" font-weight=\"600\">%s Records:</text>",
      "<text x=\"195\" y=\"100\" text-anchor=\"end\" fill=\"#0f172a\" font-size=\"12\">%s / %s</text>",
      "<text x=\"24\" y=\"132\" fill=\"#475569\" font-size=\"11\">Focal attribute:</text>",
      "<text x=\"24\" y=\"150\" fill=\"#0f172a\" font-size=\"11\" font-weight=\"600\">%s</text>",
      "<text x=\"24\" y=\"182\" fill=\"#64748b\" font-size=\"10\">attribute_record_presence()</text>",
      "<line x1=\"207\" y1=\"112\" x2=\"233\" y2=\"112\" stroke=\"#475569\" stroke-width=\"2\" marker-end=\"url(#oig-arrow)\"/>",
      "<rect x=\"236\" y=\"20\" width=\"195\" height=\"185\" rx=\"10\" fill=\"#f8fafc\" stroke=\"#cbd5e1\" stroke-width=\"1.5\"/>",
      "<rect x=\"236\" y=\"20\" width=\"195\" height=\"32\" rx=\"10\" fill=\"#0f766e\"/>",
      "<text x=\"333\" y=\"41\" text-anchor=\"middle\" fill=\"#ffffff\" font-size=\"12\" font-weight=\"700\">2. Completeness (Table 3)</text>",
      "<text x=\"248\" y=\"76\" fill=\"#0f172a\" font-size=\"12\" font-weight=\"600\">%s Populated:</text>",
      "<text x=\"419\" y=\"76\" text-anchor=\"end\" fill=\"#0f766e\" font-size=\"12\" font-weight=\"700\">%s</text>",
      "<text x=\"248\" y=\"100\" fill=\"#0f172a\" font-size=\"12\" font-weight=\"600\">%s Populated:</text>",
      "<text x=\"419\" y=\"100\" text-anchor=\"end\" fill=\"#0f766e\" font-size=\"12\" font-weight=\"700\">%s</text>",
      "<text x=\"248\" y=\"132\" fill=\"#475569\" font-size=\"11\">%s Absent / Null: %s / %s</text>",
      "<text x=\"248\" y=\"150\" fill=\"#475569\" font-size=\"11\">%s Absent / Null: %s / %s</text>",
      "<text x=\"248\" y=\"182\" fill=\"#64748b\" font-size=\"10\">Eligible present denominator</text>",
      "<line x1=\"431\" y1=\"112\" x2=\"457\" y2=\"112\" stroke=\"#475569\" stroke-width=\"2\" marker-end=\"url(#oig-arrow)\"/>",
      "<rect x=\"460\" y=\"20\" width=\"195\" height=\"185\" rx=\"10\" fill=\"#f8fafc\" stroke=\"#cbd5e1\" stroke-width=\"1.5\"/>",
      "<rect x=\"460\" y=\"20\" width=\"195\" height=\"32\" rx=\"10\" fill=\"#b45309\"/>",
      "<text x=\"557\" y=\"41\" text-anchor=\"middle\" fill=\"#ffffff\" font-size=\"12\" font-weight=\"700\">3. Consistency (Table 4)</text>",
      "<text x=\"472\" y=\"76\" fill=\"#0f172a\" font-size=\"12\" font-weight=\"600\">Comparable N:</text>",
      "<text x=\"643\" y=\"76\" text-anchor=\"end\" fill=\"#0f172a\" font-size=\"12\" font-weight=\"700\">%s</text>",
      "<text x=\"472\" y=\"100\" fill=\"#0f172a\" font-size=\"12\" font-weight=\"600\">Any Shared:</text>",
      "<text x=\"643\" y=\"100\" text-anchor=\"end\" fill=\"#0f766e\" font-size=\"12\" font-weight=\"700\">%s</text>",
      "<text x=\"472\" y=\"124\" fill=\"#0f172a\" font-size=\"12\" font-weight=\"600\">Exact Set Match:</text>",
      "<text x=\"643\" y=\"124\" text-anchor=\"end\" fill=\"#b45309\" font-size=\"12\" font-weight=\"700\">%s</text>",
      "<text x=\"472\" y=\"152\" fill=\"#be123c\" font-size=\"11\" font-weight=\"600\">Set Inconsistent: %s</text>",
      "<text x=\"472\" y=\"182\" fill=\"#64748b\" font-size=\"10\">attribute_set_agreement_summary()</text>",
      "<line x1=\"655\" y1=\"112\" x2=\"681\" y2=\"112\" stroke=\"#475569\" stroke-width=\"2\" marker-end=\"url(#oig-arrow)\"/>",
      "<rect x=\"684\" y=\"20\" width=\"184\" height=\"185\" rx=\"10\" fill=\"#f8fafc\" stroke=\"#cbd5e1\" stroke-width=\"1.5\"/>",
      "<rect x=\"684\" y=\"20\" width=\"184\" height=\"32\" rx=\"10\" fill=\"#334155\"/>",
      "<text x=\"776\" y=\"41\" text-anchor=\"middle\" fill=\"#ffffff\" font-size=\"12\" font-weight=\"700\">4. Linkage &amp; Accuracy</text>",
      "<text x=\"696\" y=\"76\" fill=\"#0f172a\" font-size=\"11\" font-weight=\"600\">Enrollment Linkage:</text>",
      "<text x=\"696\" y=\"95\" fill=\"#475569\" font-size=\"10.5\">%s</text>",
      "<text x=\"696\" y=\"126\" fill=\"#0f172a\" font-size=\"11\" font-weight=\"600\">Verification Audit:</text>",
      "<text x=\"696\" y=\"145\" fill=\"#475569\" font-size=\"10.5\">%s</text>",
      "<text x=\"696\" y=\"182\" fill=\"#64748b\" font-size=\"10\">Tables 1-2 &amp; Appendix A</text>",
      "</svg>"
    ),
    .oig_html_escape(year),
    .oig_html_escape(base::toupper(source_a)),
    count_text(side_a$n_present), count_text(side_a$n_frame),
    .oig_html_escape(base::toupper(source_b)),
    count_text(side_b$n_present), count_text(side_b$n_frame),
    .oig_html_escape(attribute_label),
    .oig_html_escape(base::toupper(source_a)),
    pct_text(side_a$n_populated, side_a$n_eligible),
    .oig_html_escape(base::toupper(source_b)),
    pct_text(side_b$n_populated, side_b$n_eligible),
    .oig_html_escape(base::toupper(source_a)),
    count_text(side_a$n_absent), count_text(side_a$n_row_missing + side_a$n_val_missing),
    .oig_html_escape(base::toupper(source_b)),
    count_text(side_b$n_absent), count_text(side_b$n_row_missing + side_b$n_val_missing),
    count_text(n_comp),
    pct_text(n_any, n_comp),
    pct_text(n_same, n_comp),
    pct_text(n_diff_set, n_comp),
    .oig_html_escape(svg_link_line),
    .oig_html_escape(svg_verif_line)
  )

  linkage_section_html <- ""
  if (!base::is.null(linkage)) {
    links_all_yr <- linkage[linkage$year == year, , drop = FALSE]
    links_dedup <- base::unique(links_all_yr[
      links_all_yr$location_linked,
      base::c("id", "year", "enrollment_id", "location_id", "linkage_path"),
      drop = FALSE
    ])
    n_dir <- base::sum(links_dedup$linkage_path == "direct_enrollment")
    n_grp <- base::sum(links_dedup$linkage_path == "group_reassignment")
    n_unlinked <- base::sum(!links_all_yr$location_linked)
    n_tot_links <- n_dir + n_grp + n_unlinked
    linkage_section_html <- base::sprintf(
      paste0(
        "<section class=\"oig-section\">",
        "<h2>Appendix A: Enrollment-to-Location Linkage (%s)</h2>",
        "<p class=\"sec-note\">Separates direct individual enrollment records from organizational ",
        "group-reassignment associations via <code>attribute_enrollment_locations()</code>.</p>",
        "<table class=\"oig-table\">",
        "<thead><tr><th>Linkage Path</th><th class=\"num\">Associations</th><th class=\"num\">Share</th></tr></thead>",
        "<tbody>",
        "<tr><td>Direct Enrollment (<code>direct_enrollment</code>)</td><td class=\"num\">%s</td><td class=\"num\">%s</td></tr>",
        "<tr><td>Group Reassignment (<code>group_reassignment</code>)</td><td class=\"num\">%s</td><td class=\"num\">%s</td></tr>",
        "<tr><td>Unlinked Enrollment Rows (<code>!location_linked</code>)</td><td class=\"num\">%s</td><td class=\"num\">%s</td></tr>",
        "</tbody></table>",
        "</section>"
      ),
      .oig_html_escape(year),
      count_text(n_dir), pct_text(n_dir, n_tot_links),
      count_text(n_grp), pct_text(n_grp, n_tot_links),
      count_text(n_unlinked), pct_text(n_unlinked, n_tot_links)
    )
  }

  if (!base::is.null(location_specificity) &&
      base::all(base::c("tiers", "funnel") %in% base::names(location_specificity))) {
    st_tbl <- location_specificity$tiers
    ef_tbl <- location_specificity$funnel
    st_rows <- base::character(0)
    for (i in base::seq_len(base::nrow(st_tbl))) {
      r_i <- st_tbl[i, , drop = FALSE]
      st_rows <- base::c(
        st_rows,
        base::sprintf(
          "<tr><td>%s</td><td class=\"num\">%s</td><td class=\"num\">%.1f%%</td></tr>",
          .oig_html_escape(r_i$specificity_tier),
          count_text(r_i$n_clinicians),
          r_i$pct_of_cohort
        )
      )
    }
    ef_rows <- base::character(0)
    for (i in base::seq_len(base::nrow(ef_tbl))) {
      r_i <- ef_tbl[i, , drop = FALSE]
      ef_rows <- base::c(
        ef_rows,
        base::sprintf(
          "<tr><td>%s</td><td class=\"num\">%s</td><td class=\"num\">%.1f%%</td></tr>",
          .oig_html_escape(r_i$stage_label),
          count_text(r_i$n_clinicians),
          r_i$pct_of_cohort
        )
      )
    }
    linkage_section_html <- base::paste0(
      linkage_section_html,
      "<section class=\"oig-section\">",
      "<h2>Appendix A2: Clinician-Level Location Specificity &amp; Evidence Funnel (<code>attribute_location_specificity()</code>)</h2>",
      "<div class=\"oig-prose-box\">", .oig_html_escape(location_specificity$plain_english_summary), "</div>",
      "<h3>4-Stage Practice-Location Evidence Funnel</h3>",
      "<table class=\"oig-table\"><thead><tr><th>Evidence Stage</th><th class=\"num\">Clinicians</th><th class=\"num\">% of Cohort</th></tr></thead><tbody>",
      base::paste(ef_rows, collapse = ""),
      "</tbody></table>",
      "<h3>6-Tier Mutually Exclusive Location Specificity</h3>",
      "<table class=\"oig-table\"><thead><tr><th>Specificity Tier</th><th class=\"num\">Clinicians</th><th class=\"num\">% of Cohort</th></tr></thead><tbody>",
      base::paste(st_rows, collapse = ""),
      "</tbody></table></section>"
    )
  }

  verif_section_html <- ""
  if (!base::is.null(nonresponse) || !base::is.null(validity_estimate) || !base::is.null(validity_bounds)) {
    nr_html <- ""
    if (!base::is.null(nonresponse)) {
      prov_yr <- nonresponse$providers[nonresponse$providers$year == year, , drop = FALSE]
      rates_yr <- nonresponse$response_rates[nonresponse$response_rates$year == year, , drop = FALSE][1L, , drop = FALSE]
      nr_html <- base::sprintf(
        paste0(
          "<h3>Verification Sample Disposition (<code>attribute_nonresponse_audit()</code>)</h3>",
          "<table class=\"oig-table\">",
          "<thead><tr><th>Sampled Providers</th><th class=\"num\">Complete</th><th class=\"num\">Partial</th>",
          "<th class=\"num\">Refused</th><th class=\"num\">Confirmed Ineligible</th>",
          "<th class=\"num\">Weighted Complete Response Rate</th></tr></thead>",
          "<tbody><tr>",
          "<td class=\"num\">%s</td><td class=\"num\">%s</td><td class=\"num\">%s</td>",
          "<td class=\"num\">%s</td><td class=\"num\">%s</td><td class=\"num\"><strong>%s</strong></td>",
          "</tr></tbody></table>"
        ),
        count_text(base::nrow(prov_yr)),
        count_text(base::sum(prov_yr$response_status == "complete_response")),
        count_text(base::sum(prov_yr$response_status == "partial_response")),
        count_text(base::sum(prov_yr$response_status == "refused")),
        count_text(rates_yr$n_confirmed_ineligible),
        pct_text(rates_yr$w_complete, rates_yr$w_denominator)
      )
    }
    est_html <- ""
    if (!base::is.null(validity_estimate)) {
      est_html <- base::sprintf(
        paste0(
          "<h3>Weighted Definitive Listing Validity (<code>validity_estimates()</code>)</h3>",
          "<table class=\"oig-table\">",
          "<thead><tr><th class=\"num\">Definitive N</th><th class=\"num\">Valid Estimate</th>",
          "<th class=\"num\">Inaccurate (1 - Estimate)</th><th class=\"num\">SE</th>",
          "<th class=\"num\">95%% Confidence Interval</th></tr></thead>",
          "<tbody><tr>",
          "<td class=\"num\">%s</td><td class=\"num\"><strong>%.1f%%</strong></td>",
          "<td class=\"num\">%.1f%%</td><td class=\"num\">%.3f</td>",
          "<td class=\"num\">[%.1f%%, %.1f%%]</td>",
          "</tr></tbody></table>"
        ),
        count_text(validity_estimate$n),
        100 * validity_estimate$estimate,
        100 * (1 - validity_estimate$estimate),
        validity_estimate$se,
        100 * validity_estimate$ci_low,
        100 * validity_estimate$ci_high
      )
    }
    bounds_html <- ""
    if (!base::is.null(validity_bounds)) {
      b_rows <- base::character(0)
      has_denom <- base::all(base::c("n_valid", "denominator") %in% base::names(validity_bounds))
      for (i in base::seq_len(base::nrow(validity_bounds))) {
        r_i <- validity_bounds[i, , drop = FALSE]
        denom_txt <- if (has_denom) {
          base::sprintf("%s / %s", count_text(r_i$n_valid), count_text(r_i$denominator))
        } else {
          "—"
        }
        b_rows <- base::c(
          b_rows,
          base::sprintf(
            "<tr><td>%s</td><td class=\"num\">%s</td><td class=\"num\"><strong>%.1f%%</strong></td></tr>",
            .oig_html_escape(r_i$scenario),
            denom_txt,
            r_i$pct_valid
          )
        )
      }
      bounds_html <- base::paste0(
        "<h3>Unresolved-Outcome Sensitivity Bounds (<code>validity_sensitivity_bounds()</code>)</h3>",
        "<table class=\"oig-table\">",
        "<thead><tr><th>Scenario</th><th class=\"num\">Weighted Valid / Denominator</th><th class=\"num\">Percent Valid</th></tr></thead>",
        "<tbody>", base::paste(b_rows, collapse = ""), "</tbody></table>"
      )
    }
    verif_section_html <- base::paste0(
      "<section class=\"oig-section\">",
      "<h2>OIG Tables 1 &amp; 2: Independent Verification &amp; Listing Accuracy</h2>",
      nr_html, est_html, bounds_html,
      "</section>"
    )
  }

  lead_lag_section_html <- ""
  if (!base::is.null(lead_lag)) {
    lead_lag_section_html <- base::sprintf(
      paste0(
        "<section class=\"oig-section\">",
        "<h2>Longitudinal Update-Lag Resolution (<code>attribute_lead_lag_duration()</code>)</h2>",
        "<div class=\"oig-prose-box\">%s</div>",
        "</section>"
      ),
      .oig_html_escape(lead_lag$combined_text)
    )
  }

  html_doc <- base::paste0(
    "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n<meta charset=\"utf-8\">\n",
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n",
    "<title>", .oig_html_escape(title), "</title>\n",
    "<style>\n",
    "body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif; ",
    "color: #0f172a; background: #f8fafc; margin: 0; padding: 24px; line-height: 1.55; }\n",
    ".oig-container { max-width: 1020px; margin: 0 auto; background: #ffffff; border: 1px solid #e2e8f0; ",
    "border-radius: 12px; padding: 28px 32px; box-shadow: 0 4px 16px rgba(15, 23, 42, 0.05); }\n",
    ".oig-header { border-bottom: 2px solid #0f172a; padding-bottom: 14px; margin-bottom: 22px; }\n",
    ".oig-badge { display: inline-block; background: #1e293b; color: #ffffff; font-size: 11px; ",
    "font-weight: 700; letter-spacing: 0.06em; text-transform: uppercase; padding: 3px 9px; border-radius: 4px; }\n",
    "h1 { font-size: 22px; margin: 8px 0 4px 0; color: #0f172a; }\n",
    "h2 { font-size: 16px; margin: 24px 0 8px 0; color: #1e293b; border-bottom: 1px solid #e2e8f0; padding-bottom: 6px; }\n",
    "h3 { font-size: 14px; margin: 16px 0 6px 0; color: #334155; }\n",
    ".sec-note { font-size: 13px; color: #475569; margin: 0 0 12px 0; }\n",
    ".kpi-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 12px; margin-bottom: 22px; }\n",
    ".kpi-card { background: #f8fafc; border: 1px solid #cbd5e1; border-radius: 8px; padding: 12px 14px; }\n",
    ".kpi-label { font-size: 11px; text-transform: uppercase; letter-spacing: 0.04em; color: #64748b; font-weight: 600; }\n",
    ".kpi-val { font-size: 22px; font-weight: 700; color: #0f172a; margin: 4px 0 2px 0; }\n",
    ".kpi-sub { font-size: 12px; color: #475569; }\n",
    ".oig-flow-svg { width: 100%; height: auto; display: block; margin: 12px 0 18px 0; }\n",
    ".oig-prose-box { background: #f0fdf4; border-left: 4px solid #0f766e; padding: 14px 18px; ",
    "border-radius: 6px; font-size: 14px; color: #0f172a; margin: 12px 0; }\n",
    ".oig-table { width: 100%; border-collapse: collapse; font-size: 13px; margin: 10px 0 16px 0; }\n",
    ".oig-table th, .oig-table td { border-bottom: 1px solid #e2e8f0; padding: 8px 10px; text-align: left; }\n",
    ".oig-table th { background: #f1f5f9; font-weight: 600; color: #334155; }\n",
    ".oig-table th.num, .oig-table td.num { text-align: right; font-variant-numeric: tabular-nums; }\n",
    ".bar-row { display: flex; align-items: center; gap: 10px; margin: 7px 0; font-size: 12px; }\n",
    ".bar-label { width: 220px; flex-shrink: 0; color: #1e293b; }\n",
    ".bar-track { flex: 1; height: 16px; background: #e2e8f0; border-radius: 4px; overflow: hidden; display: flex; }\n",
    ".bar-seg { height: 100%; }\n",
    ".seg-ok { background: #0f766e; }\n",
    ".seg-warn { background: #d97706; }\n",
    ".seg-err { background: #be123c; }\n",
    ".bar-val { width: 120px; text-align: right; font-variant-numeric: tabular-nums; color: #334155; font-weight: 600; }\n",
    ".legend-row { display: flex; gap: 16px; font-size: 12px; color: #475569; margin-bottom: 8px; }\n",
    ".legend-swatch { display: inline-block; width: 10px; height: 10px; border-radius: 2px; margin-right: 5px; vertical-align: middle; }\n",
    "</style>\n</head>\n<body>\n<div class=\"oig-container\">\n",
    "<header class=\"oig-header\">",
    "<span class=\"oig-badge\">OIG OE-07-09-00440 Audit Framework &middot; mysteryroster</span>",
    "<h1>", .oig_html_escape(title), "</h1>",
    "<div class=\"sec-note\">Focal attribute: <strong>", .oig_html_escape(attribute_label),
    "</strong> (<code>", .oig_html_escape(attribute), "</code>) &middot; Audit Year: <strong>",
    .oig_html_escape(year), "</strong></div>",
    "</header>\n",
    "<div class=\"kpi-grid\">",
    base::sprintf(
      "<div class=\"kpi-card\"><div class=\"kpi-label\">%s Completeness</div><div class=\"kpi-val\">%s</div><div class=\"kpi-sub\">%s of %s present eligible (%s frame)</div></div>",
      .oig_html_escape(base::toupper(source_a)),
      pct_text(side_a$n_populated, side_a$n_eligible),
      count_text(side_a$n_populated), count_text(side_a$n_eligible), count_text(side_a$n_frame)
    ),
    base::sprintf(
      "<div class=\"kpi-card\"><div class=\"kpi-label\">%s Completeness</div><div class=\"kpi-val\">%s</div><div class=\"kpi-sub\">%s of %s present eligible (%s frame)</div></div>",
      .oig_html_escape(base::toupper(source_b)),
      pct_text(side_b$n_populated, side_b$n_eligible),
      count_text(side_b$n_populated), count_text(side_b$n_eligible), count_text(side_b$n_frame)
    ),
    base::sprintf(
      "<div class=\"kpi-card\"><div class=\"kpi-label\">Any Shared Value Match</div><div class=\"kpi-val\">%s</div><div class=\"kpi-sub\">%s of %s comparable records (%s disjoint)</div></div>",
      pct_text(n_any, n_comp), count_text(n_any), count_text(n_comp), pct_text(n_no_shared, n_comp)
    ),
    base::sprintf(
      "<div class=\"kpi-card\"><div class=\"kpi-label\">Exact Value-Set Match</div><div class=\"kpi-val\">%s</div><div class=\"kpi-sub\">%s of %s comparable records (%s inconsistent)</div></div>",
      pct_text(n_same, n_comp), count_text(n_same), count_text(n_comp), pct_text(n_diff_set, n_comp)
    ),
    "</div>\n",
    "<section class=\"oig-section\">",
    "<h2>Study Architecture &amp; Denominator Flow</h2>",
    svg_diagram,
    "</section>\n",
    "<section class=\"oig-section\">",
    "<h2>Manuscript-Ready Audit Paragraph (<code>attribute_oig_audit_prose()</code>)</h2>",
    "<div class=\"oig-prose-box\">", .oig_html_escape(prose), "</div>",
    "</section>\n",
    "<section class=\"oig-section\">",
    "<h2>OIG Table 3: Record Presence &amp; Field Completeness (", .oig_html_escape(year), ")</h2>",
    "<div class=\"legend-row\">",
    "<span><span class=\"legend-swatch seg-ok\"></span>Populated in Frame</span>",
    "<span><span class=\"legend-swatch seg-warn\"></span>Present but Field Missing (Row/Value Null)</span>",
    "<span><span class=\"legend-swatch seg-err\"></span>Record Absent from Source Extract</span>",
    "</div>",
    base::paste(completeness_bars_html, collapse = "\n"),
    "<table class=\"oig-table\">",
    "<thead><tr><th>Source</th><th>Attribute</th><th class=\"num\">Frame N</th><th class=\"num\">Present N</th>",
    "<th class=\"num\">Populated (% Eligible)</th><th class=\"num\">Absent Record</th>",
    "<th class=\"num\">Row Missing</th><th class=\"num\">Value Null</th><th class=\"num\">Not in Extract</th></tr></thead>",
    "<tbody>", base::paste(completeness_rows_html, collapse = "\n"), "</tbody></table>",
    "</section>\n",
    "<section class=\"oig-section\">",
    "<h2>OIG Table 4: Cross-Source Consistency &amp; Set Agreement (",
    .oig_html_escape(base::toupper(source_a)), " vs. ", .oig_html_escape(base::toupper(source_b)), ")</h2>",
    base::sprintf(
      paste0(
        "<div class=\"bar-row\">",
        "<div class=\"bar-label\"><strong>Any Shared Value</strong></div>",
        "<div class=\"bar-track\">",
        "<div class=\"bar-seg seg-ok\" style=\"width:%.1f%%\"></div>",
        "<div class=\"bar-seg seg-err\" style=\"width:%.1f%%\"></div>",
        "</div>",
        "<div class=\"bar-val\">%s match</div></div>",
        "<div class=\"bar-row\">",
        "<div class=\"bar-label\"><strong>Exact Value Set</strong></div>",
        "<div class=\"bar-track\">",
        "<div class=\"bar-seg seg-ok\" style=\"width:%.1f%%\"></div>",
        "<div class=\"bar-seg seg-err\" style=\"width:%.1f%%\"></div>",
        "</div>",
        "<div class=\"bar-val\">%s match</div></div>"
      ),
      pct_any_val, pct_inconsist_any, pct_text(n_any, n_comp),
      pct_same_val, pct_inconsist_same, pct_text(n_same, n_comp)
    ),
    "<table class=\"oig-table\">",
    "<thead><tr><th>Source Pair</th><th class=\"num\">Year</th><th class=\"num\">Comparable N</th>",
    "<th class=\"num\">Any Shared Match</th><th class=\"num\">Disjoint Sets (Inconsistent)</th>",
    "<th class=\"num\">Exact Set Match</th><th class=\"num\">Set Difference (Inconsistent)</th></tr></thead>",
    "<tbody>",
    base::sprintf(
      "<tr><td><strong>%s vs. %s</strong></td><td class=\"num\">%s</td><td class=\"num\">%s</td><td class=\"num\">%s (%s)</td><td class=\"num\">%s (%s)</td><td class=\"num\">%s (%s)</td><td class=\"num\">%s (%s)</td></tr>",
      .oig_html_escape(base::toupper(source_a)),
      .oig_html_escape(base::toupper(source_b)),
      .oig_html_escape(year),
      count_text(n_comp),
      count_text(n_any), pct_text(n_any, n_comp),
      count_text(n_no_shared), pct_text(n_no_shared, n_comp),
      count_text(n_same), pct_text(n_same, n_comp),
      count_text(n_diff_set), pct_text(n_diff_set, n_comp)
    ),
    "</tbody></table>",
    "</section>\n",
    linkage_section_html,
    verif_section_html,
    lead_lag_section_html,
    "</div>\n</body>\n</html>"
  )

  if (!base::is.null(file)) {
    parent_dir <- base::dirname(file)
    if (!base::dir.exists(parent_dir)) {
      base::dir.create(parent_dir, recursive = TRUE, showWarnings = FALSE)
    }
    base::writeLines(html_doc, con = file, useBytes = TRUE)
    return(base::invisible(html_doc))
  }

  html_doc
}
