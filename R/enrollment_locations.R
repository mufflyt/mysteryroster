#' Link individual and group-reassigned enrollment edges to practice locations
#'
#' A physician's practice location can come from an administrative
#' enrollment record two different ways: the physician's own individual
#' enrollment, or a group practice's enrollment that reassigns its billing
#' (and, administratively, its locations) to the individual. Both paths are
#' routine and legitimate, but they are not the same kind of evidence -- a
#' physician linked only through a group's 40-location reassignment has no
#' stronger a claim to seeing patients at any one of those 40 addresses than
#' the group record alone establishes. This function builds the traceable
#' edge-to-location linkage for both paths without ever asserting the
#' stronger claim itself.
#'
#' @details
#' **Group linkage is not clinical-presence evidence.** Every linked row
#' gets `clinical_presence_verified = NA`, always, with no code path that
#' ever sets it otherwise -- this function establishes an administrative
#' association, not confirmation that the physician was ever actually seen
#' at that location. Determining clinical presence is a separate question
#' this function deliberately does not answer.
#'
#' **Resolve active intervals to one snapshot before calling.** Linkage is
#' keyed on `(year, enrollment_id)` alone; if an enrollment or a location
#' carries its own effective-date range, collapse it to the single snapshot
#' year being analyzed first -- this function does not interpret date
#' ranges.
#'
#' **A shared group enrollment fans out, correctly.** If two physicians both
#' reassign to the same group enrollment, and that enrollment lists several
#' locations, every physician links to every one of that group's locations
#' -- each physician-location pair is its own traceable row, not collapsed
#' or deduplicated, because each is independently a legitimate (if purely
#' administrative) association to confirm or rule out.
#'
#' @param individual_enrollments Data frame of direct enrollment edges: `id`,
#'   `year`, `enrollment_id`, one row per edge, unique and non-missing.
#' @param reassignments Data frame of group-reassignment edges, same shape as
#'   `individual_enrollments` -- `enrollment_id` here is the *receiving
#'   group's* enrollment, established by an explicit source link, not an
#'   inference this function draws.
#' @param locations Data frame of practice locations: `year`,
#'   `enrollment_id`, `location_id`, one row per location, unique and
#'   non-missing, plus any address columns to carry through (for example
#'   `practice_zip5`).
#' @param id,year Names of the identifier and year columns on
#'   `individual_enrollments`/`reassignments`. `locations` always uses
#'   `year` under this same name but has no identifier column of its own.
#'   Defaults `"npi"`, `"year"`.
#' @return A list: `links` (one row per enrollment edge x matching location,
#'   left-joined so an edge with no matching location still appears once
#'   with `location_id = NA`: `id`, `year`, `enrollment_id`, `linkage_path`
#'   (`"direct_enrollment"` or `"group_reassignment"`), `location_id`, any
#'   carried-through location columns, `location_linked`,
#'   `clinical_presence_verified` (always `NA`)); `unmatched_enrollments`
#'   (`links` filtered to `!location_linked`); `unlinked_locations` (rows of
#'   `locations` whose `(year, enrollment_id)` matches no edge at all).
#' @examples
#' individual <- data.frame(npi = "1", year = 2024, enrollment_id = "individual_a")
#' reassignments <- data.frame(npi = "1", year = 2024, enrollment_id = "group_a")
#' locations <- data.frame(
#'   year = 2024, enrollment_id = "group_a",
#'   location_id = c("site_a", "site_b"), practice_zip5 = c("80204", "80045")
#' )
#' linkage <- attribute_enrollment_locations(individual, reassignments, locations)
#' linkage$links[, c("enrollment_id", "linkage_path", "location_id", "location_linked")]
#' linkage$unmatched_enrollments # the individual enrollment has no matching location
#' @family no-gold-standard comparison
#' @export
attribute_enrollment_locations <- function(individual_enrollments, reassignments, locations,
                                             id = "npi", year = "year") {
  checkmate::assert_data_frame(individual_enrollments, min.rows = 1)
  checkmate::assert_data_frame(reassignments, min.rows = 1)
  checkmate::assert_data_frame(locations, min.rows = 1)
  checkmate::assert_string(id, min.chars = 1)
  checkmate::assert_string(year, min.chars = 1)

  require_cols <- function(df, cols, label) {
    missing_cols <- setdiff(cols, names(df))
    if (length(missing_cols) > 0) {
      stop(sprintf("attribute_enrollment_locations: %s is missing column(s): %s",
                   label, paste(missing_cols, collapse = ", ")), call. = FALSE)
    }
  }
  edge_cols <- c(id, year, "enrollment_id")
  require_cols(individual_enrollments, edge_cols, "individual_enrollments")
  require_cols(reassignments, edge_cols, "reassignments")
  require_cols(locations, c(year, "enrollment_id", "location_id"), "locations")

  assert_key <- function(df, cols, label) {
    keys <- df[, cols, drop = FALSE]
    blank <- vapply(keys, function(v) any(is.na(v) | !nzchar(trimws(as.character(v)))), logical(1))
    if (any(blank) || anyDuplicated(keys) > 0) {
      stop(sprintf("attribute_enrollment_locations: %s has missing/blank or duplicate keys.",
                   label), call. = FALSE)
    }
  }
  assert_key(individual_enrollments, edge_cols, "individual_enrollments")
  assert_key(reassignments, edge_cols, "reassignments")
  assert_key(locations, c(year, "enrollment_id", "location_id"), "locations")

  edge_frame <- function(df, path) {
    data.frame(
      id = trimws(as.character(df[[id]])), year = df[[year]],
      enrollment_id = trimws(as.character(df$enrollment_id)), linkage_path = path
    )
  }
  edges <- rbind(
    edge_frame(individual_enrollments, "direct_enrollment"),
    edge_frame(reassignments, "group_reassignment")
  )

  extra_cols <- setdiff(names(locations), c(year, "enrollment_id", "location_id"))
  loc <- data.frame(
    year = locations[[year]], enrollment_id = trimws(as.character(locations$enrollment_id)),
    location_id = trimws(as.character(locations$location_id))
  )
  for (col in extra_cols) loc[[col]] <- locations[[col]]

  linked <- merge(edges, loc, by = c("year", "enrollment_id"), all.x = TRUE)
  linked$location_linked <- !is.na(linked$location_id)
  linked$clinical_presence_verified <- NA
  linked <- linked[order(linked$id, linked$year, linked$enrollment_id, linked$location_id), ]
  linked <- tibble::as_tibble(linked[, c("id", "year", "enrollment_id", "linkage_path", "location_id",
                                          extra_cols, "location_linked", "clinical_presence_verified")])

  unmatched <- linked[!linked$location_linked, ]

  edge_key <- paste(edges$year, edges$enrollment_id, sep = "\r")
  loc_key <- paste(loc$year, loc$enrollment_id, sep = "\r")
  unlinked_locations <- tibble::as_tibble(locations[!loc_key %in% edge_key, , drop = FALSE])

  message(sprintf(
    "attribute_enrollment_locations: %d edge(s), %d linked row(s), %d unmatched edge(s), %d unlinked location(s)",
    nrow(edges), sum(linked$location_linked), nrow(unmatched), nrow(unlinked_locations)
  ))

  list(links = linked, unmatched_enrollments = unmatched, unlinked_locations = unlinked_locations)
}
