.audit_year <- function(year) {
  if (base::is.character(year) || base::is.factor(year)) {
    base::trimws(base::as.character(year))
  } else {
    year
  }
}

.audit_key <- function(rows, columns, label, unique = TRUE) {
  keys <- base::lapply(columns, function(column) {
    base::trimws(base::as.character(rows[[column]]))
  })
  if (base::any(base::vapply(keys, function(value) {
    base::anyNA(value) || base::any(!base::nzchar(value)) ||
      base::any(base::grepl("\\r", value))
  }, base::logical(1)))) {
    base::stop(label, " has missing, blank or invalid keys.", call. = FALSE)
  }
  key <- if (base::nrow(rows)) {
    base::do.call(base::paste, base::c(base::unname(keys), sep = "\r"))
  } else {
    base::character()
  }
  if (unique && base::anyDuplicated(key)) {
    base::stop(label, " has duplicate keys after normalization.", call. = FALSE)
  }
  key
}

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
  sources <- base::trimws(sources)
  if (base::any(!base::nzchar(sources)) || base::anyDuplicated(sources)) {
    base::stop("sources must be non-blank and unique after trimming.",
               call. = FALSE)
  }
  lineage_groups <- base::lapply(lineage_groups, base::trimws)
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

#' Separate coverage, completeness, consistency, and accuracy-eligible populations
#'
#' An OIG-style audit asks several questions that are easy to conflate into
#' one denominator but describe genuinely different populations: is an
#' identifier-year's record present at all in a source's extract
#' (coverage/completeness), does it have a present record in *two* sources
#' at once so a field value comparison is even possible (consistency), and
#' which of those pairwise-overlapping identifier-years were actually
#' sampled for independent verification (accuracy eligibility and the
#' sample drawn from it)? This function builds all four from the same three
#' inputs so a later comparison always states which population its
#' denominator came from.
#'
#' @details
#' **An incomplete extract's absence is not evidence.** When a source's
#' snapshot for a year is declared `extract_complete = FALSE` (a scraped
#' roster or partial dump, say), an identifier missing from `record_index`
#' for that source-year gets `record_status =
#' "not_observed_in_incomplete_extract"`, not
#' `"absent_from_declared_extract"` -- the extract's own incompleteness, not
#' the identifier's absence from the underlying population, explains why no
#' record showed up.
#'
#' **Consistency is pairwise, not general.** Only `pair`'s two named sources
#' are checked for co-presence; an identifier-year present in a third source
#' but absent from one of `pair`'s two sources is not in `consistency`. Call
#' this function again with a different `pair` for a different comparison.
#'
#' **This does not draw a sample.** `accuracy_eligible` is every
#' identifier-year both of `pair`'s sources cover -- the sampling frame, not
#' a sample. `accuracy_sample` is empty unless the caller supplies one
#' (drawn elsewhere, keeping its own sampling weights), and every row
#' supplied must already be in `accuracy_eligible` -- a sample can't reach
#' outside the frame it was drawn from.
#'
#' @param population Data frame of the target population: one row per
#'   `(id, year)`, unique and non-missing.
#' @param record_index Data frame of which sources actually have a record for
#'   which identifier-year: one row per `(id, year, source)`, unique and
#'   non-missing, built from each source's own roster -- not from which
#'   claims happen to be non-missing.
#' @param manifest Data frame of snapshot metadata: one row per `(year,
#'   source)`, unique and non-missing, plus a logical `extract_complete`
#'   column (no `NA`s). Every `(year, source)` appearing in `record_index`
#'   must appear here; every year appearing in `population` must have at
#'   least one row here.
#' @param pair Character vector of exactly two distinct source names (must
#'   appear in `manifest$source`) to check pairwise consistency and accuracy
#'   eligibility for.
#' @param id,year Names of the identifier and year columns, shared across
#'   `population`, `record_index`, and `accuracy_sample` (`manifest` has no
#'   identifier column). Defaults `"npi"`, `"year"`.
#' @param accuracy_sample Optional data frame of `(id, year)` rows sampled
#'   for independent accuracy verification, unique and non-missing. Every
#'   row must already be in the identifier-years both of `pair`'s sources
#'   cover. Default `NULL` -- `accuracy_sample` in the result is then empty.
#' @return A list: `coverage` (one row per `id`/`year`/`source` triple implied
#'   by `population` x that year's manifest sources: `id`, `year`, `source`,
#'   `extract_complete`, `record_present`, `record_status` -- one of
#'   `"present"`, `"absent_from_declared_extract"`,
#'   `"not_observed_in_incomplete_extract"`); `completeness` (`coverage`
#'   filtered to `record_present`); `consistency` (`id`, `year` -- present in
#'   both of `pair`'s sources); `accuracy_eligible` (identical shape to
#'   `consistency` -- the accuracy sampling frame); `accuracy_sample` (`id`,
#'   `year` -- the supplied sample, or empty).
#' @examples
#' population <- data.frame(npi = c("1", "2"), year = 2024)
#' record_index <- data.frame(
#'   npi = c("1", "2", "1"), year = 2024, source = c("nppes", "nppes", "pecos")
#' )
#' manifest <- data.frame(year = 2024, source = c("nppes", "pecos"), extract_complete = TRUE)
#' populations <- attribute_analysis_populations(
#'   population, record_index, manifest, pair = c("nppes", "pecos")
#' )
#' populations$coverage[, c("id", "source", "record_status")]
#' populations$consistency # only npi 1 has a present record in both sources
#' @seealso [attribute_field_dictionary()] and [attribute_set_agreement()] for
#'   the comparisons these populations are built to feed.
#' @family no-gold-standard comparison
#' @export
attribute_analysis_populations <- function(population, record_index, manifest, pair,
                                             id = "npi", year = "year", accuracy_sample = NULL) {
  checkmate::assert_data_frame(population, min.rows = 1)
  checkmate::assert_data_frame(record_index, min.rows = 1)
  checkmate::assert_data_frame(manifest, min.rows = 1)
  checkmate::assert_character(pair, len = 2, any.missing = FALSE, unique = TRUE)
  checkmate::assert_string(id, min.chars = 1)
  checkmate::assert_string(year, min.chars = 1)

  require_cols <- function(df, cols, label) {
    missing_cols <- setdiff(cols, names(df))
    if (length(missing_cols) > 0) {
      stop(sprintf("attribute_analysis_populations: %s is missing column(s): %s",
                   label, paste(missing_cols, collapse = ", ")), call. = FALSE)
    }
  }
  require_cols(population, c(id, year), "population")
  require_cols(record_index, c(id, year, "source"), "record_index")
  require_cols(manifest, c(year, "source", "extract_complete"), "manifest")
  checkmate::assert_logical(manifest$extract_complete, any.missing = FALSE)

  .audit_key(population, c(id, year), "population")
  .audit_key(record_index, c(id, year, "source"), "record_index")
  .audit_key(manifest, c(year, "source"), "manifest")

  pop <- data.frame(id = trimws(as.character(population[[id]])), year = .audit_year(population[[year]]))
  ridx <- data.frame(
    id = trimws(as.character(record_index[[id]])), year = .audit_year(record_index[[year]]),
    source = trimws(as.character(record_index$source)), record_present = TRUE
  )
  man <- data.frame(
    year = .audit_year(manifest[[year]]), source = trimws(as.character(manifest$source)),
    extract_complete = manifest$extract_complete
  )

  pair <- base::trimws(pair)
  checkmate::assert_character(pair, min.chars = 1, unique = TRUE)
  unknown_sources <- setdiff(pair, man$source)
  if (length(unknown_sources) > 0) {
    stop(sprintf("attribute_analysis_populations: pair source(s) not in manifest: %s",
                 paste(unknown_sources, collapse = ", ")), call. = FALSE)
  }
  ridx_key <- paste(ridx$year, ridx$source, sep = "\r")
  man_key <- paste(man$year, man$source, sep = "\r")
  if (any(!ridx_key %in% man_key)) {
    stop("attribute_analysis_populations: record_index includes a (year, source) absent from manifest.",
         call. = FALSE)
  }
  missing_years <- setdiff(pop$year, man$year)
  if (length(missing_years) > 0) {
    stop("attribute_analysis_populations: every population year needs snapshot metadata in manifest.",
         call. = FALSE)
  }

  coverage <- merge(pop, man, by = "year")
  coverage <- merge(coverage, ridx, by = c("id", "year", "source"), all.x = TRUE)
  coverage$record_present[is.na(coverage$record_present)] <- FALSE
  coverage$record_status <- ifelse(
    coverage$record_present, "present",
    ifelse(coverage$extract_complete, "absent_from_declared_extract",
           "not_observed_in_incomplete_extract")
  )
  coverage <- coverage[order(coverage$id, coverage$year, coverage$source), ]
  coverage <- coverage[, c("id", "year", "source", "extract_complete", "record_present", "record_status")]

  completeness <- coverage[coverage$record_present, ]

  pair_grid <- coverage[coverage$source %in% pair, ]
  key <- paste(pair_grid$id, pair_grid$year, sep = "\r")
  idx <- split(seq_len(nrow(pair_grid)), key, drop = TRUE)
  consistency_rows <- lapply(idx, function(rows) {
    if (sum(pair_grid$record_present[rows]) == 2L) {
      data.frame(id = pair_grid$id[rows[1]], year = pair_grid$year[rows[1]])
    }
  })
  consistency <- dplyr::bind_rows(consistency_rows)
  if (!nrow(consistency)) consistency <- data.frame(id = character(0), year = pop$year[0])
  consistency <- consistency[order(consistency$id, consistency$year), ]
  accuracy_eligible <- consistency

  if (is.null(accuracy_sample)) {
    accuracy_sample <- accuracy_eligible[0, ]
  } else {
    checkmate::assert_data_frame(accuracy_sample, min.rows = 1)
    require_cols(accuracy_sample, c(id, year), "accuracy_sample")
    as_df <- data.frame(id = trimws(as.character(accuracy_sample[[id]])), year = .audit_year(accuracy_sample[[year]]))
    .audit_key(as_df, c("id", "year"), "accuracy_sample")
    key_elig <- paste(accuracy_eligible$id, accuracy_eligible$year, sep = "\r")
    key_samp <- paste(as_df$id, as_df$year, sep = "\r")
    if (any(!key_samp %in% key_elig)) {
      stop("attribute_analysis_populations: accuracy_sample includes identifier-year(s) outside the pair's overlap.",
           call. = FALSE)
    }
    accuracy_sample <- as_df
  }

  message(sprintf(
    paste0("attribute_analysis_populations: %d coverage row(s) (%d present), ",
           "%d consistency pair(s), %d accuracy-sample row(s)"),
    nrow(coverage), sum(coverage$record_present), nrow(consistency), nrow(accuracy_sample)
  ))

  list(
    coverage = tibble::as_tibble(coverage),
    completeness = tibble::as_tibble(completeness),
    consistency = tibble::as_tibble(consistency),
    accuracy_eligible = tibble::as_tibble(accuracy_eligible),
    accuracy_sample = tibble::as_tibble(accuracy_sample)
  )
}

#' Distinguish an absent record, an unavailable field, and an empty field value
#'
#' [attribute_analysis_populations()]'s `coverage` only says whether a
#' *record* is present. That one bit hides three different reasons a field's
#' value could end up missing from a completeness count: the whole record
#' isn't there, the record is there but this field was never part of that
#' source's extract at all, or the field was part of the extract but every
#' row of it happened to be blank. Conflating these means a source penalized
#' for genuinely never asking a question looks identical to one that asked
#' and recorded nothing -- this function keeps all three apart, and a fourth,
#' distinguishable `"populated"` outcome, at the identifier/year/source/
#' attribute grain.
#'
#' @details
#' **Field availability must be stated, never inferred.** A `(year, source,
#' attribute)` combination with no row in `source_fields` is an error, not a
#' silent `FALSE` -- declaring "this field wasn't in the extract" is a fact
#' about the extract that must be supplied, the same way
#' [attribute_analysis_populations()] requires `extract_complete` rather than
#' guessing it from absence.
#'
#' **A claim cannot contradict the record/field status that governs it.**
#' If `claims` has a row for an `(id, year, source, attribute)` where
#' `coverage` says the record isn't present, or `source_fields` says the
#' field isn't available, that is a contradiction in the inputs -- this
#' function errors rather than silently preferring one input over the other.
#'
#' **Raw format validity is a separate question.** `n_populated` counts
#' non-blank values; it says nothing about whether those values are
#' well-formed (a ZIP of `"ZIP IS UNKNOWN"` still counts as populated here).
#' Check format separately before treating a field as usable.
#'
#' @param coverage [attribute_analysis_populations()]'s `coverage` output (or
#'   anything with the same `id`, `year`, `source`, `record_present`,
#'   `record_status` columns).
#' @param dictionary [attribute_field_dictionary()]'s output (or anything
#'   with at least `source`, `attribute` columns).
#' @param source_fields Data frame of explicit field availability: one row
#'   per `(year, source, attribute)`, unique and non-missing, plus a logical
#'   `field_available` column with no `NA`s. Every `(year, source,
#'   attribute)` combination implied by `coverage` x `dictionary` must appear
#'   here.
#' @param claims Data frame of raw assertions: `id`, `year`, `source`,
#'   `attribute`, `value` (character; multiple rows per `(id, year, source,
#'   attribute)` are allowed and all counted). Only rows matching a `(year,
#'   source, attribute)` the dictionary defines for that source are used;
#'   others are silently out of scope (they describe an attribute this
#'   dictionary doesn't track, not a missing value).
#' @param id,year Names of the identifier and year columns on `claims` only
#'   -- `coverage` always carries `id`/`year` as named by
#'   [attribute_analysis_populations()]. Defaults `"npi"`, `"year"`.
#' @return A tibble, one row per `id`/`year`/`source`/`attribute` implied by
#'   `coverage` x `dictionary`: those plus `record_present`, `record_status`,
#'   `field_available`, `n_field_rows`, `n_populated`, `field_status` (one of
#'   `coverage`'s own `record_status` values when the record isn't present,
#'   else `"field_not_in_extract"`, `"field_row_missing"`,
#'   `"field_value_missing"`, or `"populated"`), and `completeness_eligible`
#'   (`record_present & field_available` -- the denominator a completeness
#'   rate should use).
#' @examples
#' coverage <- data.frame(
#'   id = c("1", "1"), year = 2024, source = c("nppes", "pecos"),
#'   record_present = c(TRUE, TRUE), record_status = "present"
#' )
#' dictionary <- data.frame(source = c("nppes", "pecos"), attribute = "practice_zip5")
#' source_fields <- data.frame(
#'   year = 2024, source = c("nppes", "pecos"), attribute = "practice_zip5",
#'   field_available = c(TRUE, FALSE) # pecos never carried this field
#' )
#' claims <- data.frame(
#'   npi = "1", year = 2024, source = "nppes", attribute = "practice_zip5", value = "80204"
#' )
#' indicators <- attribute_record_presence(coverage, dictionary, source_fields, claims)
#' indicators[, c("id", "source", "field_status", "completeness_eligible")]
#' @seealso [attribute_analysis_populations()] for `coverage`,
#'   [attribute_field_dictionary()] for `dictionary`.
#' @family no-gold-standard comparison
#' @export
attribute_record_presence <- function(coverage, dictionary, source_fields, claims,
                                        id = "npi", year = "year") {
  checkmate::assert_data_frame(coverage, min.rows = 1)
  checkmate::assert_data_frame(dictionary, min.rows = 1)
  checkmate::assert_data_frame(source_fields, min.rows = 1)
  checkmate::assert_data_frame(claims)
  checkmate::assert_string(id, min.chars = 1)
  checkmate::assert_string(year, min.chars = 1)

  require_cols <- function(df, cols, label) {
    missing_cols <- setdiff(cols, names(df))
    if (length(missing_cols) > 0) {
      stop(sprintf("attribute_record_presence: %s is missing column(s): %s",
                   label, paste(missing_cols, collapse = ", ")), call. = FALSE)
    }
  }
  require_cols(coverage, c("id", "year", "source", "record_present", "record_status"), "coverage")
  require_cols(dictionary, c("source", "attribute"), "dictionary")
  require_cols(source_fields, c("year", "source", "attribute", "field_available"), "source_fields")
  require_cols(claims, c(id, year, "source", "attribute", "value"), "claims")
  checkmate::assert_logical(source_fields$field_available, any.missing = FALSE)
  checkmate::assert_logical(coverage$record_present, any.missing = FALSE)
  if (base::any(!coverage$record_status %in% base::c(
    "present", "absent_from_declared_extract",
    "not_observed_in_incomplete_extract"
  )) || base::any(coverage$record_present !=
                  (coverage$record_status == "present"))) {
    base::stop("coverage record_status disagrees with record_present.",
               call. = FALSE)
  }
  checkmate::assert_character(claims$value)

  .audit_key(coverage, c("id", "year", "source"), "coverage")
  .audit_key(dictionary, c("source", "attribute"), "dictionary")
  .audit_key(source_fields, c("year", "source", "attribute"), "source_fields")

  cov <- data.frame(
    id = trimws(as.character(coverage$id)), year = .audit_year(coverage$year),
    source = trimws(as.character(coverage$source)),
    record_present = coverage$record_present, record_status = as.character(coverage$record_status)
  )
  dict <- data.frame(
    source = trimws(as.character(dictionary$source)), attribute = trimws(as.character(dictionary$attribute))
  )
  missing_dictionary <- setdiff(unique(cov$source), unique(dict$source))
  if (length(missing_dictionary) > 0) {
    stop(sprintf("attribute_record_presence: dictionary is missing coverage source(s): %s",
                 paste(missing_dictionary, collapse = ", ")), call. = FALSE)
  }

  indicators <- merge(cov, dict, by = "source")

  fields <- data.frame(
    year = .audit_year(source_fields$year), source = trimws(as.character(source_fields$source)),
    attribute = trimws(as.character(source_fields$attribute)), field_available = source_fields$field_available
  )
  indicators <- merge(indicators, fields, by = c("year", "source", "attribute"), all.x = TRUE)
  if (anyNA(indicators$field_available)) {
    stop("attribute_record_presence: source_fields must state field_available for every (year, source, attribute) coverage x dictionary implies.",
         call. = FALSE)
  }

  claims_norm <- data.frame(
    id = trimws(as.character(claims[[id]])), year = .audit_year(claims[[year]]),
    source = trimws(as.character(claims$source)), attribute = trimws(as.character(claims$attribute)),
    value = claims$value
  )
  ikey <- paste(indicators$id, indicators$year, indicators$source, indicators$attribute, sep = "\r")
  field_keys <- base::c("year", "source", "attribute")
  tracked_fields <- base::unique(indicators[, field_keys, drop = FALSE])
  claims_norm <- base::merge(claims_norm, tracked_fields, by = field_keys,
                             sort = FALSE)
  ckey <- .audit_key(claims_norm, base::c("id", field_keys), "claims",
                     unique = FALSE)
  claims_scoped <- claims_norm[ckey %in% ikey, ]
  claims_scoped$populated <- !is.na(claims_scoped$value) & nzchar(trimws(claims_scoped$value))

  if (nrow(claims_scoped) > 0) {
    key2 <- paste(claims_scoped$id, claims_scoped$year, claims_scoped$source, claims_scoped$attribute, sep = "\r")
    idx2 <- split(seq_len(nrow(claims_scoped)), key2, drop = TRUE)
    counted <- dplyr::bind_rows(lapply(idx2, function(rows) {
      data.frame(
        id = claims_scoped$id[rows[1]], year = claims_scoped$year[rows[1]],
        source = claims_scoped$source[rows[1]], attribute = claims_scoped$attribute[rows[1]],
        n_field_rows = length(rows), n_populated = sum(claims_scoped$populated[rows])
      )
    }))
  } else {
    counted <- data.frame(id = character(0), year = claims_norm$year[0], source = character(0),
                           attribute = character(0), n_field_rows = integer(0), n_populated = integer(0))
  }
  indicators <- merge(indicators, counted, by = c("id", "year", "source", "attribute"), all.x = TRUE)
  indicators$n_field_rows[is.na(indicators$n_field_rows)] <- 0L
  indicators$n_populated[is.na(indicators$n_populated)] <- 0L

  contradiction <- indicators[indicators$n_field_rows > 0L &
                                 (!indicators$record_present | !indicators$field_available), ]
  if (nrow(contradiction) > 0) {
    stop("attribute_record_presence: claims contradict record presence or field availability.", call. = FALSE)
  }

  indicators$field_status <- dplyr::case_when(
    !indicators$record_present ~ indicators$record_status,
    !indicators$field_available ~ "field_not_in_extract",
    indicators$n_field_rows == 0L ~ "field_row_missing",
    indicators$n_populated == 0L ~ "field_value_missing",
    TRUE ~ "populated"
  )
  indicators$completeness_eligible <- indicators$record_present & indicators$field_available

  indicators <- indicators[order(indicators$id, indicators$year, indicators$source, indicators$attribute), ]
  message(sprintf("attribute_record_presence: %d indicator row(s) across %d field_status categor%s",
                   nrow(indicators), length(unique(indicators$field_status)),
                   if (length(unique(indicators$field_status)) == 1L) "y" else "ies"))

  tibble::as_tibble(indicators[, c("id", "year", "source", "attribute", "record_present", "record_status",
                                    "field_available", "n_field_rows", "n_populated", "field_status",
                                    "completeness_eligible")])
}

#' Audit longitudinal source coverage for extraction-vintage discontinuities and missing sub-feeds
#'
#' When an administrative source family is assembled across multiple panel years
#' from more than one release format or sub-feed (for example, combining historic
#' Medicare `PECOS` enrollment files with `Doctors and Clinicians` (`DAC`)
#' Care Compare releases), annual coverage rates can alternate sharply if one
#' sub-feed was omitted or extracted from a partial snapshot in specific years
#' (e.g., `22.7%` in 2018 vs `49.2%` in 2019 vs `24.8%` in 2020 vs `56.7%` in
#' 2021). Interpreting such sawtooth jumps as real clinician entry/exit without
#' first auditing the sub-feed composition of each panel year leads to severe
#' longitudinal bias.
#'
#' This function audits annual cohort coverage across every individual source or
#' sub-feed and across optional composite source families, computes year-over-year
#' percentage-point jumps (`yoy_pp_change`), and flags panel years with abrupt
#' coverage discontinuities (`abs(yoy_pp_change) >= jump_threshold_pct`) or
#' missing sub-feeds (`subfeed_missing_in_year`).
#'
#' @param population Data frame defining the denominator cohort per year (`id`,
#'   `year`), or a single cross-sectional cohort (`id` only, expanded across all
#'   years present in `sources`).
#' @param sources Named list of data frames, one per raw source or sub-feed
#'   (e.g. `list(pecos_raw = pecos_df, dac_raw = dac_df, nppes = nppes_df, tmsis = tmsis_df)`),
#'   each containing `id` and `year`.
#' @param source_families Optional named list mapping composite source family
#'   names to character vectors of component names in `sources` (for example,
#'   `list(medicare_combined = c("pecos_raw", "dac_raw"))`).
#' @param id,year Names of identifier and year columns. Defaults `"npi"`,
#'   `"year"`.
#' @param jump_threshold_pct Absolute year-over-year percentage-point change
#'   that triggers a `discontinuity_flag`. Default `15.0` (15 percentage points).
#' @return A list with three tibbles:
#'   \describe{
#'     \item{`annual_coverage`}{One row per `(source, year)` across all
#'       individual `sources` and composite `source_families`, reporting
#'       `n_denominator`, `n_present`, `pct_present`, `yoy_pp_change`,
#'       `discontinuity_flag`, and `active_subfeeds`.}
#'     \item{`family_decomposition`}{For each composite family in
#'       `source_families` and each year, breaks down `n_combined`,
#'       per-sub-feed counts, `n_missing_subfeeds`, `missing_subfeeds`, and a
#'       diagnostic `vintage_status` (`"complete_subfeeds"`,
#'       `"partial_subfeed_omission"`, or `"no_records_in_year"`).}
#'     \item{`flagged_discontinuities`}{Subset of `annual_coverage` rows where
#'       `discontinuity_flag` is `TRUE` or where a composite family is missing
#'       one or more component sub-feeds.}
#'   }
#' @examples
#' pop <- expand.grid(npi = as.character(1:10), year = 2018:2021, stringsAsFactors = FALSE)
#' pecos <- data.frame(
#'   npi = c(as.character(1:5), as.character(1:6), as.character(1:6)),
#'   year = c(rep(2018, 5), rep(2019, 6), rep(2021, 6)) # missing in 2020!
#' )
#' dac <- data.frame(
#'   npi = rep(as.character(1:3), 4),
#'   year = rep(2018:2021, each = 3)
#' )
#' audit <- attribute_source_vintage_audit(
#'   pop,
#'   sources = list(pecos = pecos, dac = dac),
#'   source_families = list(medicare = c("pecos", "dac"))
#' )
#' audit$family_decomposition
#' audit$flagged_discontinuities
#' @seealso [attribute_analysis_populations()], [attribute_record_presence()].
#' @family no-gold-standard comparison
#' @export
attribute_source_vintage_audit <- function(population,
                                           sources,
                                           source_families = NULL,
                                           id = "npi",
                                           year = "year",
                                           jump_threshold_pct = 15.0) {
  checkmate::assert_data_frame(population, min.rows = 1)
  checkmate::assert_list(sources, min.len = 1, names = "unique")
  checkmate::assert_string(id, min.chars = 1)
  checkmate::assert_string(year, min.chars = 1)
  checkmate::assert_number(jump_threshold_pct, lower = 0, finite = TRUE)

  if (!id %in% names(population)) {
    stop(sprintf("attribute_source_vintage_audit: population is missing column '%s'.", id), call. = FALSE)
  }

  clean_src <- lapply(names(sources), function(s_name) {
    df <- sources[[s_name]]
    checkmate::assert_data_frame(df)
    for (col in c(id, year)) {
      if (!col %in% names(df)) {
        stop(sprintf("attribute_source_vintage_audit: sources$%s is missing column '%s'.", s_name, col), call. = FALSE)
      }
    }
    s_id <- trimws(as.character(df[[id]]))
    s_yr <- .audit_year(df[[year]])
    keep <- !is.na(s_id) & nzchar(s_id) & !is.na(s_yr)
    unique(data.frame(id = s_id[keep], year = s_yr[keep], stringsAsFactors = FALSE))
  })
  names(clean_src) <- names(sources)

  all_src_years <- sort(unique(unlist(lapply(clean_src, function(d) d$year))))
  if (year %in% names(population)) {
    pop_df <- unique(data.frame(
      id = trimws(as.character(population[[id]])),
      year = .audit_year(population[[year]]),
      stringsAsFactors = FALSE
    ))
  } else {
    u_ids <- unique(trimws(as.character(population[[id]])))
    u_ids <- u_ids[!is.na(u_ids) & nzchar(u_ids)]
    pop_df <- expand.grid(id = u_ids, year = all_src_years, stringsAsFactors = FALSE)
  }
  pop_df <- pop_df[!is.na(pop_df$id) & nzchar(pop_df$id) & !is.na(pop_df$year), , drop = FALSE]
  all_years <- sort(unique(pop_df$year))

  eval_series <- function(s_label, s_type, comp_names) {
    rows <- lapply(all_years, function(yr) {
      denom_ids <- pop_df$id[pop_df$year == yr]
      n_denom <- length(denom_ids)
      active_sf <- character(0)
      present_ids <- character(0)
      for (cn in comp_names) {
        c_df <- clean_src[[cn]]
        c_yr_ids <- intersect(c_df$id[c_df$year == yr], denom_ids)
        if (length(c_yr_ids) > 0L) {
          active_sf <- c(active_sf, cn)
          present_ids <- union(present_ids, c_yr_ids)
        }
      }
      n_pres <- length(present_ids)
      pct_pres <- if (n_denom > 0L) round(100 * n_pres / n_denom, 2) else NA_real_
      tibble::tibble(
        source = s_label,
        source_type = s_type,
        year = as.integer(yr),
        n_denominator = as.integer(n_denom),
        n_present = as.integer(n_pres),
        pct_present = pct_pres,
        n_expected_subfeeds = as.integer(length(comp_names)),
        n_active_subfeeds = as.integer(length(active_sf)),
        active_subfeeds = if (length(active_sf) > 0L) paste(active_sf, collapse = "+") else "none",
        missing_subfeeds = if (length(setdiff(comp_names, active_sf)) > 0L) paste(setdiff(comp_names, active_sf), collapse = "+") else "none"
      )
    })
    ser <- dplyr::bind_rows(rows)
    ser$yoy_pp_change <- c(NA_real_, round(diff(ser$pct_present), 2))
    ser$discontinuity_flag <- !is.na(ser$yoy_pp_change) & (abs(ser$yoy_pp_change) >= jump_threshold_pct)
    ser$subfeed_missing_in_year <- ser$n_active_subfeeds < ser$n_expected_subfeeds
    ser
  }

  indiv_list <- lapply(names(clean_src), function(sn) eval_series(sn, "individual_source", sn))
  fam_list <- list()
  fam_decomp_list <- list()

  if (!is.null(source_families)) {
    checkmate::assert_list(source_families, min.len = 1, names = "unique")
    for (f_name in names(source_families)) {
      comps <- source_families[[f_name]]
      checkmate::assert_character(comps, min.len = 1, any.missing = FALSE)
      unknown_c <- setdiff(comps, names(clean_src))
      if (length(unknown_c) > 0L) {
        stop(sprintf("attribute_source_vintage_audit: source_families$%s references unknown source(s): %s.",
                     f_name, paste(unknown_c, collapse = ", ")), call. = FALSE)
      }
      f_ser <- eval_series(f_name, "composite_family", comps)
      fam_list[[f_name]] <- f_ser

      f_rows <- lapply(seq_len(nrow(f_ser)), function(i) {
        yr <- f_ser$year[i]
        denom_ids <- pop_df$id[pop_df$year == yr]
        sub_counts <- vapply(comps, function(cn) {
          c_df <- clean_src[[cn]]
          length(intersect(c_df$id[c_df$year == yr], denom_ids))
        }, integer(1))
        sub_summary <- paste(sprintf("%s=%d", comps, sub_counts), collapse = "; ")
        v_status <- dplyr::case_when(
          f_ser$n_active_subfeeds[i] == 0L ~ "no_records_in_year",
          f_ser$n_active_subfeeds[i] < f_ser$n_expected_subfeeds[i] ~ "partial_subfeed_omission",
          TRUE ~ "complete_subfeeds"
        )
        tibble::tibble(
          family = f_name,
          year = yr,
          n_denominator = f_ser$n_denominator[i],
          n_combined = f_ser$n_present[i],
          pct_combined = f_ser$pct_present[i],
          yoy_pp_change = f_ser$yoy_pp_change[i],
          subfeed_counts = sub_summary,
          active_subfeeds = f_ser$active_subfeeds[i],
          missing_subfeeds = f_ser$missing_subfeeds[i],
          vintage_status = v_status
        )
      })
      fam_decomp_list[[f_name]] <- dplyr::bind_rows(f_rows)
    }
  }

  # fam_list is named by family name; dplyr::bind_rows() treats a named
  # list's names as a nested-column key rather than flattening its rows, so
  # unname() first -- otherwise every family row lands in a spurious
  # <family_name>-named packed column instead of being row-bound in.
  annual_coverage <- dplyr::bind_rows(indiv_list, unname(fam_list))
  family_decomposition <- if (length(fam_decomp_list) > 0L) {
    dplyr::bind_rows(fam_decomp_list)
  } else {
    tibble::tibble(
      family = character(0), year = integer(0), n_denominator = integer(0),
      n_combined = integer(0), pct_combined = numeric(0), yoy_pp_change = numeric(0),
      subfeed_counts = character(0), active_subfeeds = character(0),
      missing_subfeeds = character(0), vintage_status = character(0)
    )
  }
  flagged <- annual_coverage[
    annual_coverage$discontinuity_flag |
      (annual_coverage$source_type == "composite_family" & annual_coverage$subfeed_missing_in_year), ,
    drop = FALSE
  ]

  list(
    annual_coverage = tibble::as_tibble(annual_coverage),
    family_decomposition = tibble::as_tibble(family_decomposition),
    flagged_discontinuities = tibble::as_tibble(flagged)
  )
}

#' Multi-attribute OIG Table 4 inconsistency summary with composite "any variable inconsistent" row
#'
#' In the May 2013 HHS OIG audit (`OEI-07-09-00440`, Table 4), cross-source
#' inconsistency between NPPES and PECOS is reported attribute-by-attribute
#' (practice address/ZIP, credential, phone, etc.) followed by an overall
#' composite row: the percentage of comparable clinicians with **at least one**
#' inconsistent attribute across all evaluated variables. Because [attribute_set_agreement()]
#' evaluates one attribute at a time, computing the composite "any variable
#' inconsistent" denominator and numerator requires joining the per-attribute
#' set-agreement indicators at the `(id, year)` grain.
#'
#' This function runs [attribute_set_agreement()] across a vector of
#' `attributes` for two sources and returns both the per-attribute inconsistency
#' rows and the composite `"any_variable_inconsistent"` row (evaluated on
#' clinicians comparable on at least one attribute, as well as on clinicians
#' comparable across *all* attributes simultaneously), with Wilson confidence
#' intervals.
#'
#' @param source_a,source_b Data frames for the two sources to compare, each
#'   containing `id`, optional `year`, and all columns named in `attributes`.
#' @param attributes Character vector of attribute column names to evaluate
#'   (e.g. `c("practice_zip5", "state", "credential", "phone")`).
#' @param attribute_labels Optional named character vector or character vector of
#'   the same length as `attributes` providing human-readable labels for the
#'   output table.
#' @param id,year Names of identifier and year columns. Defaults `"npi"`,
#'   `"year"` (`year = NULL` for a single cross-section).
#' @param source_a_label,source_b_label Labels for the two sources. Defaults
#'   `"source_a"`, `"source_b"`.
#' @param conf_level Confidence level for Wilson intervals via [wilson_ci()].
#'   Default `0.95`.
#' @return A list with two tibbles:
#'   \describe{
#'     \item{`table4_summary`}{One row per `(year, attribute)` plus two composite
#'       rows per year (`"any_variable_inconsistent_union"` across clinicians
#'       comparable on `>= 1` attribute, and
#'       `"any_variable_inconsistent_complete_case"` across clinicians comparable
#'       on all `attributes`), reporting `n_compared`, `n_any_shared_match`,
#'       `n_disjoint_inconsistent`, `pct_disjoint_inconsistent`, `disjoint_ci_low`,
#'       `disjoint_ci_high`, `n_exact_set_match`, `n_set_diff_inconsistent`,
#'       `pct_set_diff_inconsistent`, `set_diff_ci_low`, `set_diff_ci_high`.}
#'     \item{`clinician_level`}{One row per `(id, year)` comparable on at least
#'       one attribute, reporting `n_attributes_compared`,
#'       `n_disjoint_inconsistent`, `any_disjoint_inconsistent`,
#'       `n_set_diff_inconsistent`, and `any_set_diff_inconsistent`.}
#'   }
#' @examples
#' a <- data.frame(
#'   npi = c("1", "2", "3"), year = 2024,
#'   zip5 = c("80204", "80204", "80204"),
#'   state = c("CO", "CO", "CO")
#' )
#' b <- data.frame(
#'   npi = c("1", "2", "3"), year = 2024,
#'   zip5 = c("80204", "80301", "80204"),
#'   state = c("CO", "CO", "NM")
#' )
#' t4 <- attribute_inconsistency_table(a, b, attributes = c("zip5", "state"))
#' t4$table4_summary[, c("attribute", "n_compared", "n_disjoint_inconsistent", "pct_disjoint_inconsistent")]
#' @seealso [attribute_set_agreement()], [attribute_set_agreement_summary()].
#' @family no-gold-standard comparison
#' @export
attribute_inconsistency_table <- function(source_a,
                                          source_b,
                                          attributes,
                                          attribute_labels = NULL,
                                          id = "npi",
                                          year = "year",
                                          source_a_label = "source_a",
                                          source_b_label = "source_b",
                                          conf_level = 0.95) {
  checkmate::assert_data_frame(source_a, min.rows = 1)
  checkmate::assert_data_frame(source_b, min.rows = 1)
  checkmate::assert_character(attributes, min.len = 1, any.missing = FALSE, unique = TRUE)
  checkmate::assert_string(id, min.chars = 1)
  checkmate::assert_string(source_a_label, min.chars = 1)
  checkmate::assert_string(source_b_label, min.chars = 1)
  checkmate::assert_number(conf_level, lower = 0, upper = 1)
  if (conf_level <= 0 || conf_level >= 1) {
    stop("attribute_inconsistency_table: conf_level must be strictly between 0 and 1.", call. = FALSE)
  }

  for (col in c(id, attributes)) {
    if (!col %in% names(source_a)) {
      stop(sprintf("attribute_inconsistency_table: source_a is missing column '%s'.", col), call. = FALSE)
    }
    if (!col %in% names(source_b)) {
      stop(sprintf("attribute_inconsistency_table: source_b is missing column '%s'.", col), call. = FALSE)
    }
  }

  if (is.null(attribute_labels)) {
    attr_lbl_map <- stats::setNames(attributes, attributes)
  } else if (!is.null(names(attribute_labels))) {
    attr_lbl_map <- stats::setNames(attributes, attributes)
    overlap <- intersect(names(attribute_labels), attributes)
    attr_lbl_map[overlap] <- unname(attribute_labels[overlap])
  } else {
    if (length(attribute_labels) != length(attributes)) {
      stop("attribute_inconsistency_table: attribute_labels must have the same length as attributes.", call. = FALSE)
    }
    attr_lbl_map <- stats::setNames(as.character(attribute_labels), attributes)
  }

  src_list <- stats::setNames(list(source_a, source_b), c(source_a_label, source_b_label))

  per_attr <- lapply(attributes, function(attr_i) {
    sa <- attribute_set_agreement(src_list, attribute = attr_i, id = id, year = year)
    sa$attribute <- attr_i
    sa
  })
  all_sa <- dplyr::bind_rows(per_attr)
  if (nrow(all_sa) == 0L) {
    stop("attribute_inconsistency_table: no comparable records found across any of the specified attributes.", call. = FALSE)
  }

  make_summary_row <- function(yr_val, attr_code, attr_label, n_comp, n_disjoint, n_set_diff) {
    ci_dis <- if (n_comp > 0L) wilson_ci(n_disjoint, n_comp, conf_level = conf_level, as_percent = TRUE) else list(ci_low = NA_real_, ci_high = NA_real_)
    ci_set <- if (n_comp > 0L) wilson_ci(n_set_diff, n_comp, conf_level = conf_level, as_percent = TRUE) else list(ci_low = NA_real_, ci_high = NA_real_)
    tibble::tibble(
      source_a = source_a_label,
      source_b = source_b_label,
      year = yr_val,
      attribute = attr_code,
      attribute_label = attr_label,
      n_compared = as.integer(n_comp),
      n_any_shared_match = as.integer(n_comp - n_disjoint),
      n_disjoint_inconsistent = as.integer(n_disjoint),
      pct_disjoint_inconsistent = if (n_comp > 0L) round(100 * n_disjoint / n_comp, 2) else NA_real_,
      disjoint_ci_low = round(ci_dis$ci_low, 2),
      disjoint_ci_high = round(ci_dis$ci_high, 2),
      n_exact_set_match = as.integer(n_comp - n_set_diff),
      n_set_diff_inconsistent = as.integer(n_set_diff),
      pct_set_diff_inconsistent = if (n_comp > 0L) round(100 * n_set_diff / n_comp, 2) else NA_real_,
      set_diff_ci_low = round(ci_set$ci_low, 2),
      set_diff_ci_high = round(ci_set$ci_high, 2)
    )
  }

  yr_vals <- unique(all_sa$year)
  yr_vals <- yr_vals[order(yr_vals, na.last = TRUE)]

  summary_rows <- list()
  clin_rows <- list()
  n_attrs_total <- length(attributes)

  for (yv in yr_vals) {
    sub_y <- if (is.na(yv)) all_sa[is.na(all_sa$year), , drop = FALSE] else all_sa[!is.na(all_sa$year) & all_sa$year == yv, , drop = FALSE]
    for (attr_i in attributes) {
      sub_a <- sub_y[sub_y$attribute == attr_i, , drop = FALSE]
      n_c <- nrow(sub_a)
      n_dis <- sum(!sub_a$any_shared)
      n_sd <- sum(!sub_a$same_set)
      summary_rows[[length(summary_rows) + 1L]] <- make_summary_row(
        yv, attr_i, unname(attr_lbl_map[attr_i]), n_c, n_dis, n_sd
      )
    }

    by_id <- split(sub_y, sub_y$id, drop = TRUE)
    c_df <- dplyr::bind_rows(lapply(by_id, function(g) {
      n_dis_g <- sum(!g$any_shared)
      n_sd_g <- sum(!g$same_set)
      tibble::tibble(
        id = g$id[1],
        year = yv,
        n_attributes_compared = as.integer(nrow(g)),
        n_disjoint_inconsistent = as.integer(n_dis_g),
        any_disjoint_inconsistent = n_dis_g > 0L,
        n_set_diff_inconsistent = as.integer(n_sd_g),
        any_set_diff_inconsistent = n_sd_g > 0L
      )
    }))
    c_df <- c_df[order(c_df$id), , drop = FALSE]
    clin_rows[[length(clin_rows) + 1L]] <- c_df

    # Composite union row (comparable on >= 1 attribute)
    summary_rows[[length(summary_rows) + 1L]] <- make_summary_row(
      yv,
      "any_variable_inconsistent_union",
      "At Least One Variable Inconsistent (Union Denominator)",
      nrow(c_df),
      sum(c_df$any_disjoint_inconsistent),
      sum(c_df$any_set_diff_inconsistent)
    )

    # Composite complete-case row (comparable on all evaluated attributes)
    cc_df <- c_df[c_df$n_attributes_compared == n_attrs_total, , drop = FALSE]
    summary_rows[[length(summary_rows) + 1L]] <- make_summary_row(
      yv,
      "any_variable_inconsistent_complete_case",
      "At Least One Variable Inconsistent (Complete-Case Denominator)",
      nrow(cc_df),
      sum(cc_df$any_disjoint_inconsistent),
      sum(cc_df$any_set_diff_inconsistent)
    )
  }

  list(
    table4_summary = tibble::as_tibble(dplyr::bind_rows(summary_rows)),
    clinician_level = tibble::as_tibble(dplyr::bind_rows(clin_rows))
  )
}

#' Execute canonical normalizers and format-validity guards for provider directory attributes
#'
#' [attribute_field_dictionary()] records a `normalizer` name for each tracked
#' directory attribute (`"zip5"`, `"phone10"`, `"state_postal"`,
#' `"credential_token"`, `"person_name"`, etc.), while [attribute_record_presence()]
#' checks only whether a raw value is non-blank (`"populated"`), leaving format
#' validation to a separate step so that malformed strings such as
#' `"ZIP IS UNKNOWN"` or `"000-000-0000"` are not silently dropped or trusted.
#'
#' This function provides the executable normalizers and format-validity guards
#' corresponding to [attribute_field_dictionary()], including leading-zero
#' restoration for 3- and 4-digit Northeast/Puerto Rico ZIP codes stripped by
#' spreadsheet/CSV readers (`"7030"` -> `"07030"`, `"901"` -> `"00901"`),
#' 10-digit NANP phone normalization, USPS state/territory validation,
#' credential token canonicalization, and 10-digit NPI Luhn checksum validation.
#'
#' @param x Vector of raw attribute values to normalize and validate.
#' @param normalizer Single string selecting the normalizer rule:
#'   \itemize{
#'     \item `"zip5"` -- Extracts digits, pads 3- or 4-digit numeric strings
#'       with leading zeros to 5 digits (`00501`--`99950`), truncates 9-digit
#'       `ZIP+4` strings to their 5-digit prefix, and rejects all-zero
#'       (`"00000"`) or non-numeric strings.
#'     \item `"scf3"` -- Normalizes via `"zip5"` and takes the 3-digit
#'       Sectional Center Facility prefix.
#'     \item `"phone10"` -- Strips non-digits, removes a leading US country code
#'       `"1"` from 11-digit strings, and validates 10-digit North American
#'       Numbering Plan format (area code and central-office exchange cannot
#'       begin with `0` or `1`, and cannot be all-zero or repeating placeholder
#'       digits such as `"9999999999"`).
#'     \item `"state_postal"` -- Upper-cases, trims, and validates against the
#'       50 US states, DC, and USPS territories (`PR`, `VI`, `GU`, `AS`, `MP`,
#'       `UM`, `FM`, `MH`, `PW`, `AA`, `AE`, `AP`).
#'     \item `"credential_token"` -- Upper-cases, strips punctuation (`"."`,
#'       `","`), and extracts canonical midwifery/advanced-practice/physician
#'       tokens (`CNM`, `CM`, `APRN`, `NP`, `WHNP`, `FNP`, `MSN`, `DNP`, `RN`,
#'       `MD`, `DO`, `PA`) in sorted semicolon-delimited canonical order.
#'     \item `"person_name"` -- Upper-cases, removes honorifics/suffixes (`DR`,
#'       `MD`, `CNM`, `JR`, `SR`, `II`, `III`), collapses whitespace, and
#'       validates that at least two alphabetic characters remain.
#'     \item `"npi10"` -- Validates 10-digit numeric NPI format (`1` or `2`
#'       leading digit) and verifies the ISO/CMS Luhn check digit (with `80840`
#'       health-industry prefix).
#'   }
#' @return A tibble with one row per element of `x`:
#'   \describe{
#'     \item{`raw_value`}{Original input coerced to character.}
#'     \item{`populated`}{Logical: `TRUE` when `raw_value` is non-`NA` and
#'       non-blank after trimming.}
#'     \item{`normalized_value`}{Canonical normalized string (`NA_character_`
#'       when `!format_valid`).}
#'     \item{`format_valid`}{Logical: `TRUE` when `populated` and the value
#'       passes the normalizer's format/checksum guard.}
#'     \item{`validation_status`}{One of `"valid"`, `"missing_or_blank"`, or a
#'       specific rejection reason (e.g. `"invalid_zip5"`, `"invalid_phone10"`,
#'       `"invalid_state_postal"`, `"invalid_npi_luhn"`).}
#'   }
#' @examples
#' normalize_directory_attribute(
#'   c("80204-1234", "7030", "901", "00000", "ZIP UNKNOWN", NA),
#'   normalizer = "zip5"
#' )
#' normalize_directory_attribute(
#'   c("(303) 555-0199", "1-303-555-0199", "000-000-0000", ""),
#'   normalizer = "phone10"
#' )
#' @seealso [attribute_field_dictionary()], [attribute_record_presence()].
#' @family no-gold-standard comparison
#' @export
normalize_directory_attribute <- function(x,
                                          normalizer = c("zip5", "scf3", "phone10",
                                                         "state_postal", "credential_token",
                                                         "person_name", "npi10")) {
  normalizer <- match.arg(normalizer)
  raw_chr <- as.character(x)
  trimmed <- trimws(raw_chr)
  populated <- !is.na(trimmed) & nzchar(trimmed)
  n <- length(raw_chr)

  norm_val <- rep(NA_character_, n)
  valid <- rep(FALSE, n)
  status <- ifelse(populated, "invalid", "missing_or_blank")

  if (normalizer %in% c("zip5", "scf3")) {
    digits <- gsub("[^0-9]", "", trimmed)
    short <- populated & nchar(digits) >= 3L & nchar(digits) <= 4L
    digits[short] <- sprintf("%05d", suppressWarnings(as.integer(digits[short])))
    z5 <- ifelse(populated & nchar(digits) >= 5L, substr(digits, 1L, 5L), NA_character_)
    z_int <- suppressWarnings(as.integer(z5))
    ok <- populated & !is.na(z_int) & z_int >= 501L & z_int <= 99950L
    valid <- ok
    norm_val[ok] <- if (normalizer == "zip5") z5[ok] else substr(z5[ok], 1L, 3L)
    status[populated & ok] <- "valid"
    status[populated & !ok] <- paste0("invalid_", normalizer)
  } else if (normalizer == "phone10") {
    digits <- gsub("[^0-9]", "", trimmed)
    has_cc <- populated & nchar(digits) == 11L & substr(digits, 1L, 1L) == "1"
    digits[has_cc] <- substr(digits[has_cc], 2L, 11L)
    ok_len <- populated & nchar(digits) == 10L
    npa_ok <- ok_len & !substr(digits, 1L, 1L) %in% c("0", "1")
    nxx_ok <- ok_len & !substr(digits, 4L, 4L) %in% c("0", "1")
    not_rep <- ok_len & !grepl("^([0-9])\\1{9}$", digits)
    ok <- npa_ok & nxx_ok & not_rep
    valid <- ok
    norm_val[ok] <- digits[ok]
    status[populated & ok] <- "valid"
    status[populated & !ok] <- "invalid_phone10"
  } else if (normalizer == "state_postal") {
    valid_states <- c(
      datasets::state.abb, "DC", "PR", "VI", "GU", "AS", "MP",
      "UM", "FM", "MH", "PW", "AA", "AE", "AP"
    )
    up <- toupper(trimmed)
    ok <- populated & (up %in% valid_states)
    valid <- ok
    norm_val[ok] <- up[ok]
    status[populated & ok] <- "valid"
    status[populated & !ok] <- "invalid_state_postal"
  } else if (normalizer == "credential_token") {
    up <- toupper(gsub("[^A-Za-z0-9 ]", " ", trimmed))
    known <- c("CNM", "CM", "APRN", "NP", "WHNP", "FNP", "ANP", "MSN", "DNP", "RN", "MD", "DO", "PA", "FACNM")
    for (i in which(populated)) {
      toks <- unique(strsplit(trimws(up[i]), "\\s+")[[1]])
      matched <- sort(intersect(toks, known))
      if (length(matched) > 0L) {
        valid[i] <- TRUE
        norm_val[i] <- paste(matched, collapse = ";")
        status[i] <- "valid"
      } else if (nchar(gsub("[^A-Z]", "", up[i])) >= 2L) {
        valid[i] <- TRUE
        norm_val[i] <- paste(sort(toks[nzchar(toks)]), collapse = ";")
        status[i] <- "valid"
      } else {
        status[i] <- "invalid_credential_token"
      }
    }
  } else if (normalizer == "person_name") {
    up <- toupper(gsub("[^A-Za-z '\\-]", " ", trimmed))
    up <- gsub("\\b(DR|MD|DO|CNM|APRN|RN|NP|MSN|DNP|JR|SR|II|III|IV)\\b", " ", up)
    up <- trimws(gsub("\\s+", " ", up))
    ok <- populated & nchar(gsub("[^A-Z]", "", up)) >= 2L
    valid <- ok
    norm_val[ok] <- up[ok]
    status[populated & ok] <- "valid"
    status[populated & !ok] <- "invalid_person_name"
  } else if (normalizer == "npi10") {
    luhn_ok <- function(s) {
      if (is.na(s) || !grepl("^[12][0-9]{9}$", s)) return(FALSE)
      d9 <- as.integer(strsplit(substr(s, 1L, 9L), "")[[1]])
      check_digit <- as.integer(substr(s, 10L, 10L))
      # 80840 prefix check sum = 24
      weights <- c(2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L)
      prod <- d9 * weights
      prod[prod > 9L] <- prod[prod > 9L] - 9L
      total <- 24L + sum(prod)
      expected <- (10L - (total %% 10L)) %% 10L
      expected == check_digit
    }
    digits <- gsub("[^0-9]", "", trimmed)
    for (i in which(populated)) {
      if (luhn_ok(digits[i])) {
        valid[i] <- TRUE
        norm_val[i] <- digits[i]
        status[i] <- "valid"
      } else {
        status[i] <- "invalid_npi_luhn"
      }
    }
  }

  tibble::tibble(
    raw_value = raw_chr,
    populated = populated,
    normalized_value = norm_val,
    format_valid = valid,
    validation_status = status
  )
}
