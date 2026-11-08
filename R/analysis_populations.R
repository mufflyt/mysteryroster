#' An editable field-definition dictionary, cross-joined over sources
#'
#' An OIG-style directory audit needs a single place that records, for every
#' attribute a study compares, what it means, whether it is an identity,
#' mailing, or practice-location field, how its raw value should eventually
#' be normalized, which sources are required to report it under a cited
#' requirement, and which sources share lineage (so two sources that both
#' trace back to one upstream feed, like a group's PECOS revalidation and its
#' DAC extract, aren't later double-counted as two independent agreements).
#' This function builds that table's shape from a caller-supplied attribute
#' catalog -- it never ships a hardcoded field list for any particular
#' registry, since nothing else in this package assumes a specific source
#' (the same functions run the ENT registry-validity study and the OB/GYN
#' subspecialty study with entirely different registries).
#'
#' @details
#' **Requirement flags start unreviewed, deliberately.** This function never
#' invents a `requirement` status from the attribute catalog -- every row is
#' `"unreviewed"`, with `requirement_reference` and `specification_date` left
#' `NA`, until an analyst checks a dated source specification and edits the
#' table. A hardcoded guess here would quietly assert current reporting law
#' the package has no way to verify.
#'
#' **Normalizer labels are metadata, not executable code.** `normalizer` is a
#' label for a human-readable spec ("zip5", "phone_review") the analyst
#' writes separately -- this function does not transform any value.
#'
#' **Lineage groups reuse [attribute_claims_collapse_lineage()]'s shape.** A
#' source named in no `lineage_groups` element gets its own name as its
#' `lineage` value (it is the sole member of its own family); sources in the
#' same declared group all get that group's name. Which sources share
#' lineage is a provenance fact about how the data was built, not something
#' this function can detect -- it must be supplied explicitly.
#'
#' @param fields A data frame, one row per attribute: `attribute`,
#'   `definition`, `location_role` (typically `"identity"`, `"mailing"`, or
#'   `"practice"`, but any label is accepted), `normalizer`. `attribute`
#'   values must be unique.
#' @param sources Character vector of source labels, unique and non-missing.
#' @param lineage_groups A named list, same shape as
#'   [attribute_claims_collapse_lineage()]'s argument: `list(dac_pecos =
#'   c("dac", "pecos"))`. Every element needs 2+ members; a source must not
#'   appear in more than one group. Default `list()` -- every source is then
#'   its own lineage family.
#' @return A tibble, one row per `(source, attribute)`: `source`, `attribute`,
#'   `definition`, `location_role`, `normalizer`, `comparison` (always
#'   `"any_shared_value"` -- see [attribute_set_agreement()]), `requirement`
#'   (always `"unreviewed"`), `requirement_reference` (`NA`),
#'   `specification_date` (`NA` `Date`), `lineage`.
#' @examples
#' fields <- data.frame(
#'   attribute = c("practice_zip5", "mailing_zip5", "credential"),
#'   definition = c("Practice location ZIP", "Mailing address ZIP; not practice",
#'                   "Credential text; not a specialty code"),
#'   location_role = c("practice", "mailing", "identity"),
#'   normalizer = c("zip5", "zip5", "credential_review")
#' )
#' dictionary <- attribute_field_dictionary(
#'   fields, sources = c("nppes", "pecos", "dac"),
#'   lineage_groups = list(dac_pecos = c("dac", "pecos"))
#' )
#' dictionary[dictionary$attribute == "practice_zip5", c("source", "lineage", "requirement")]
#' @seealso [attribute_set_agreement()], whose `any_shared`/`same_set`
#'   distinction this dictionary's `comparison` column points at; and
#'   [attribute_claims_collapse_lineage()] for the `lineage_groups` shape.
#' @family no-gold-standard comparison
#' @export
attribute_field_dictionary <- function(fields, sources, lineage_groups = list()) {
  checkmate::assert_data_frame(fields, min.rows = 1)
  need_fields <- c("attribute", "definition", "location_role", "normalizer")
  missing_cols <- setdiff(need_fields, names(fields))
  if (length(missing_cols) > 0) {
    stop(sprintf("attribute_field_dictionary: fields is missing column(s): %s",
                 paste(missing_cols, collapse = ", ")), call. = FALSE)
  }
  attribute_vals <- trimws(as.character(fields$attribute))
  if (anyNA(attribute_vals) || any(!nzchar(attribute_vals)) || anyDuplicated(attribute_vals)) {
    stop("attribute_field_dictionary: fields$attribute must be unique, non-missing, non-blank.",
         call. = FALSE)
  }

  checkmate::assert_character(sources, min.len = 1, any.missing = FALSE, unique = TRUE)
  if (any(!nzchar(trimws(sources)))) {
    stop("attribute_field_dictionary: sources must be non-blank.", call. = FALSE)
  }
  checkmate::assert_list(lineage_groups, names = "unique")

  all_members <- unlist(lineage_groups, use.names = FALSE)
  if (length(all_members) != length(unique(all_members))) {
    dup <- unique(all_members[duplicated(all_members)])
    stop(sprintf("attribute_field_dictionary: source(s) in more than one lineage group: %s",
                 paste(dup, collapse = ", ")), call. = FALSE)
  }
  too_small <- names(lineage_groups)[lengths(lineage_groups) < 2L]
  if (length(too_small) > 0) {
    stop(sprintf("attribute_field_dictionary: lineage group(s) with fewer than 2 members: %s",
                 paste(too_small, collapse = ", ")), call. = FALSE)
  }
  unknown_members <- setdiff(all_members, sources)
  if (length(unknown_members) > 0) {
    stop(sprintf("attribute_field_dictionary: lineage group member(s) not in sources: %s",
                 paste(unknown_members, collapse = ", ")), call. = FALSE)
  }

  fields_clean <- tibble::tibble(
    attribute = attribute_vals,
    definition = as.character(fields$definition),
    location_role = as.character(fields$location_role),
    normalizer = as.character(fields$normalizer)
  )

  dictionary <- merge(tibble::tibble(source = sources), fields_clean, by = NULL)
  lineage_of <- function(s) {
    for (family in names(lineage_groups)) {
      if (s %in% lineage_groups[[family]]) return(family)
    }
    s
  }
  dictionary$lineage <- vapply(dictionary$source, lineage_of, character(1))
  dictionary$comparison <- "any_shared_value"
  dictionary$requirement <- "unreviewed"
  dictionary$requirement_reference <- NA_character_
  dictionary$specification_date <- as.Date(NA_character_)

  dictionary <- dictionary[order(dictionary$source, dictionary$attribute), ]
  tibble::as_tibble(dictionary[, c("source", "attribute", "definition", "location_role",
                                    "normalizer", "comparison", "requirement",
                                    "requirement_reference", "specification_date", "lineage")])
}
