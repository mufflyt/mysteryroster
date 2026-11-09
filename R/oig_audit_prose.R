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
#' @param entity_label,entity_label_plural Singular and plural nouns for the
#'   audited entity (for example `"nurse-midwife"` and `"nurse-midwives"`).
#'   Default `"physician"`; the plural defaults to the singular plus `"s"`,
#'   so irregular plurals must be given explicitly.
#' @param linkage Optional [attribute_enrollment_locations()]'s `links`
#'   element, filtered to `year` internally. `NULL` omits the linkage
#'   sentence.
#' @param nonresponse Optional [attribute_nonresponse_audit()]'s full
#'   output (the list with `providers` and `response_rates`), filtered to
#'   `year` internally. `NULL` omits the verification-response sentence.
#' @return A single character string: the paragraph, ready to paste into a
#'   manuscript. Its first sentence gives the size of the declared study frame
#'   (rows of `record_presence` for each source) and, separately, how many of
#'   those entities each source holds a record for.
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
                                        attribute, attribute_label = NULL, linkage = NULL, nonresponse = NULL,
                                        entity_label = "physician",
                                        entity_label_plural = paste0(entity_label, "s")) {
  checkmate::assert_data_frame(record_presence, min.rows = 1)
  checkmate::assert_data_frame(set_agreement_summary, min.rows = 1)
  checkmate::assert_string(source_a, min.chars = 1)
  checkmate::assert_string(source_b, min.chars = 1)
  checkmate::assert_string(attribute, min.chars = 1)
  checkmate::assert_string(entity_label, min.chars = 1)
  checkmate::assert_string(entity_label_plural, min.chars = 1)
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

  rp_side <- function(source) {
    row <- record_presence[record_presence$year == year & record_presence$source == source &
                              record_presence$attribute == attribute, ]
    if (!nrow(row)) {
      stop(sprintf("attribute_oig_audit_prose: no record_presence rows for source '%s', year %s, attribute '%s'.",
                   source, year, attribute), call. = FALSE)
    }
    list(n_frame = nrow(row), n_present = sum(row$record_present),
         n_populated = sum(row$field_status == "populated"))
  }
  a <- rp_side(source_a)
  b <- rp_side(source_b)

  pair_match <- (set_agreement_summary$source_a == source_a & set_agreement_summary$source_b == source_b) |
    (set_agreement_summary$source_a == source_b & set_agreement_summary$source_b == source_a)
  comparison <- one_row(set_agreement_summary[set_agreement_summary$year == year & pair_match, ],
                        "set_agreement_summary selection")

  if (comparison$n_compared > min(a$n_present, b$n_present)) {
    stop("attribute_oig_audit_prose: comparable count exceeds the lesser source's present-record count.",
         call. = FALSE)
  }

  coverage_sentence <- if (a$n_frame == b$n_frame) {
    sprintf("In %s, the declared study frame contained %s %s; %s and %s held records for %s and %s of them, respectively.",
            year, count_text(a$n_frame), entity_label_plural, toupper(source_a), toupper(source_b),
            count_text(a$n_present), count_text(b$n_present))
  } else {
    sprintf("In %s, %s and %s held records for %s of %s and %s of %s %s in their declared study frames, respectively.",
            year, toupper(source_a), toupper(source_b), count_text(a$n_present), count_text(a$n_frame),
            count_text(b$n_present), count_text(b$n_frame), entity_label_plural)
  }
  sentences <- c(coverage_sentence, sprintf(
    "Among records present in each source, %s were populated in %s and %s, respectively.",
    attribute_label, pct_text(a$n_populated, a$n_present), pct_text(b$n_populated, b$n_present)
  ))

  if (comparison$n_compared > 0) {
    sentences <- c(sentences, sprintf(
      paste0("Among %s comparable %s, at least one value matched in %s, whereas the complete reported ",
             "value sets agreed in %s."),
      count_text(comparison$n_compared), entity_label_plural, pct_text(comparison$n_any_shared, comparison$n_compared),
      pct_text(comparison$n_same_set, comparison$n_compared)
    ))
  } else {
    sentences <- c(sentences, sprintf(
      "Cross-source agreement was not estimable because no %s had usable values in both sources.",
      entity_label_plural
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
      count_text(n_direct), count_text(n_group), entity_label
    ))
  }

  if (!is.null(nonresponse)) {
    require_cols(nonresponse$providers, c("id", "year", "response_status"), "nonresponse$providers")
    providers <- nonresponse$providers[nonresponse$providers$year == year, ]
    rates <- one_row(nonresponse$response_rates[nonresponse$response_rates$year == year, ],
                     "nonresponse$response_rates selection")
    sentences <- c(sentences, sprintf(
      paste0("Of %s sampled %s, %s completed verification, %s provided partial responses, and %s refused. ",
             "After excluding %s confirmed-ineligible %s and retaining unknown eligibility, the weighted ",
             "complete-response rate was %s."),
      count_text(nrow(providers)), entity_label_plural, count_text(sum(providers$response_status == "complete_response")),
      count_text(sum(providers$response_status == "partial_response")),
      count_text(sum(providers$response_status == "refused")), count_text(rates$n_confirmed_ineligible),
      entity_label_plural, pct_text(rates$w_complete, rates$w_denominator)
    ))
  }

  sentences <- c(sentences, "Cross-source disagreement was not classified as verified inaccuracy.")
  paste(sentences, collapse = " ")
}
