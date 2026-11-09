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

  assert_key <- function(df, cols, label) {
    keys <- df[, cols, drop = FALSE]
    blank <- vapply(keys, function(v) any(is.na(v) | !nzchar(trimws(as.character(v)))), logical(1))
    if (any(blank) || anyDuplicated(keys) > 0) {
      stop(sprintf("attribute_analysis_populations: %s has missing/blank or duplicate keys.",
                   label), call. = FALSE)
    }
  }
  pop <- data.frame(id = trimws(as.character(population[[id]])), year = population[[year]])
  ridx <- data.frame(
    id = trimws(as.character(record_index[[id]])), year = record_index[[year]],
    source = trimws(as.character(record_index$source)), record_present = TRUE
  )
  man <- data.frame(
    year = manifest[[year]], source = trimws(as.character(manifest$source)),
    extract_complete = manifest$extract_complete
  )
  # Checked on the normalized (trimmed) frames, not the raw input -- otherwise
  # whitespace-padded duplicates (" 1 " vs "1") pass validation here and then
  # silently collide once trimws() below treats them as the same key.
  assert_key(pop, c("id", "year"), "population")
  assert_key(ridx, c("id", "year", "source"), "record_index")
  assert_key(man, c("year", "source"), "manifest")

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
    as_df <- data.frame(id = trimws(as.character(accuracy_sample[[id]])), year = accuracy_sample[[year]])
    assert_key(as_df, c("id", "year"), "accuracy_sample")
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
  checkmate::assert_character(claims$value)

  assert_key <- function(df, cols, label) {
    keys <- df[, cols, drop = FALSE]
    blank <- vapply(keys, function(v) any(is.na(v) | !nzchar(trimws(as.character(v)))), logical(1))
    if (any(blank) || anyDuplicated(keys) > 0) {
      stop(sprintf("attribute_record_presence: %s has missing/blank or duplicate keys.",
                   label), call. = FALSE)
    }
  }
  cov <- data.frame(
    id = trimws(as.character(coverage$id)), year = coverage$year,
    source = trimws(as.character(coverage$source)),
    record_present = coverage$record_present, record_status = as.character(coverage$record_status)
  )
  dict <- data.frame(
    source = trimws(as.character(dictionary$source)), attribute = trimws(as.character(dictionary$attribute))
  )
  # Checked on the normalized (trimmed) frames, not the raw input -- otherwise
  # whitespace-padded duplicates pass validation here and then silently
  # collide once trimws() above treats them as the same key.
  assert_key(cov, c("id", "year", "source"), "coverage")
  assert_key(dict, c("source", "attribute"), "dictionary")

  missing_dictionary <- setdiff(unique(cov$source), unique(dict$source))
  if (length(missing_dictionary) > 0) {
    stop(sprintf("attribute_record_presence: dictionary is missing coverage source(s): %s",
                 paste(missing_dictionary, collapse = ", ")), call. = FALSE)
  }

  indicators <- merge(cov, dict, by = "source")

  fields <- data.frame(
    year = source_fields$year, source = trimws(as.character(source_fields$source)),
    attribute = trimws(as.character(source_fields$attribute)), field_available = source_fields$field_available
  )
  assert_key(fields, c("year", "source", "attribute"), "source_fields")
  indicators <- merge(indicators, fields, by = c("year", "source", "attribute"), all.x = TRUE)
  if (anyNA(indicators$field_available)) {
    stop("attribute_record_presence: source_fields must state field_available for every (year, source, attribute) coverage x dictionary implies.",
         call. = FALSE)
  }

  claims_norm <- data.frame(
    id = trimws(as.character(claims[[id]])), year = claims[[year]],
    source = trimws(as.character(claims$source)), attribute = trimws(as.character(claims$attribute)),
    value = claims$value
  )
  ikey <- paste(indicators$id, indicators$year, indicators$source, indicators$attribute, sep = "\r")
  ckey <- paste(claims_norm$id, claims_norm$year, claims_norm$source, claims_norm$attribute, sep = "\r")
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
