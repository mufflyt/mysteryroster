#' Deduplicate a roster with an NPI-aware key and an audit trail
#'
#' Registry extracts often contain the same person twice, but collapsing by
#' name and geography alone can merge two different same-name clinicians. This
#' helper builds a deterministic key from caller-supplied columns, optionally
#' prepends an NPI column when one is available, keeps the highest-quality row
#' in each duplicate group, and returns the removed rows and conflict groups
#' explicitly instead of letting them disappear into a count.
#'
#' The key is conservative: a present NPI and a missing NPI do **not** collapse
#' into one row, because absence of an identifier is not evidence that two
#' records are the same person. When `npi_col` is absent or blank throughout,
#' the key falls back to `key_cols` alone.
#'
#' @param roster Data frame to deduplicate.
#' @param key_cols Character vector of column names forming the roster identity
#'   key (for example `c("physician_name", "city", "state")`). Values are
#'   trimmed, lower-cased, and missing values are treated as blank for keying
#'   only; source columns are not changed.
#' @param npi_col Optional character scalar naming an NPI column. When supplied,
#'   its trimmed value is prepended to the key; `NA`, blank, and the literal
#'   string `"NA"` are blank for keying only.
#' @param score Optional quality score used to choose the row kept within each
#'   duplicate group. Supply either a numeric vector the length of `roster` or
#'   the name of a numeric column in `roster`. Higher scores win; ties keep the
#'   earliest input row. Scores must be finite and non-missing.
#' @param conflict_col Optional character scalar naming a column whose differing
#'   non-blank values within a duplicate group indicate that the group may not
#'   be one entity (for example conflicting subspecialty assignments). Every
#'   row in each conflicted group is returned in `conflicts`.
#' @param resolution_col Character scalar naming the audit column added to
#'   `data` and `removed`. Use `NULL` to omit it.
#' @return A list with:
#'   \describe{
#'     \item{`data`}{Deduplicated rows in input order, with `resolution_col`
#'       marking `unique_record` or `kept_best_record`.}
#'     \item{`removed`}{Rows removed as duplicates, with `resolution_col`
#'       `removed_duplicate` and a `dedup_group` integer.}
#'     \item{`conflicts`}{All rows from duplicate groups whose `conflict_col`
#'       has more than one distinct non-blank value, or an empty tibble when
#'       `conflict_col` is not supplied or no conflicts exist.}
#'     \item{`counts`}{Input, output, removed, duplicate-group, duplicate-row,
#'       and conflict-group counts.}
#'   }
#' @examples
#' x <- data.frame(
#'   npi = c("1", "1", "2"),
#'   physician_name = c("Smith Jane", "SMITH JANE", "Smith Jane"),
#'   city = "Denver", state = "CO",
#'   quality = c(1, 10, 1)
#' )
#' out <- deduplicate_roster(x, key_cols = c("physician_name", "city", "state"),
#'                           npi_col = "npi", score = "quality")
#' out$data
#' out$removed
#' @export
deduplicate_roster <- function(roster, key_cols, npi_col = NULL, score = NULL,
                               conflict_col = NULL,
                               resolution_col = "duplicate_resolution") {
  checkmate::assert_data_frame(roster)
  checkmate::assert_character(key_cols, min.len = 1L, any.missing = FALSE)
  missing_key <- setdiff(key_cols, names(roster))
  if (length(missing_key)) {
    stop("deduplicate_roster: missing key column(s): ",
         paste(missing_key, collapse = ", "), call. = FALSE)
  }
  checkmate::assert_string(npi_col, null.ok = TRUE)
  if (!is.null(npi_col) && !npi_col %in% names(roster)) {
    stop("deduplicate_roster: missing npi column: ", npi_col, call. = FALSE)
  }
  checkmate::assert_string(conflict_col, null.ok = TRUE)
  if (!is.null(conflict_col) && !conflict_col %in% names(roster)) {
    stop("deduplicate_roster: missing conflict column: ", conflict_col, call. = FALSE)
  }
  checkmate::assert_string(resolution_col, null.ok = TRUE)

  n <- nrow(roster)
  empty <- tibble::as_tibble(roster)
  if (!n) {
    return(list(
      data = empty,
      removed = empty,
      conflicts = empty,
      counts = list(input_rows = 0L, output_rows = 0L, removed_rows = 0L,
                    duplicate_groups = 0L, duplicate_rows = 0L,
                    conflict_groups = 0L)
    ))
  }

  key_values <- lapply(roster[key_cols], function(x) {
    x <- trimws(tolower(as.character(x)))
    x[is.na(x)] <- ""
    x
  })
  if (!is.null(npi_col)) {
    npi_key <- trimws(as.character(roster[[npi_col]]))
    npi_key[is.na(npi_key) | !nzchar(npi_key) | npi_key == "NA"] <- ""
    key_values <- c(list(npi_key), key_values)
  }
  key <- do.call(paste, c(key_values, sep = "\x1f"))

  quality <- if (is.null(score)) {
    rep(0, n)
  } else if (is.character(score) && length(score) == 1L) {
    if (!score %in% names(roster)) {
      stop("deduplicate_roster: missing score column: ", score, call. = FALSE)
    }
    roster[[score]]
  } else {
    score
  }
  if (!is.numeric(quality) || length(quality) != n || anyNA(quality) ||
      any(!is.finite(quality))) {
    stop("deduplicate_roster: score must be a finite numeric vector or numeric column",
         call. = FALSE)
  }

  row_id <- seq_len(n)
  dup_counts <- table(key)
  duplicate_keys <- names(dup_counts)[dup_counts > 1L]
  in_duplicate <- key %in% duplicate_keys
  group_id <- match(key, unique(key))

  ord <- order(key, -quality, row_id)
  best_idx <- ord[!duplicated(key[ord])]
  best_idx <- sort(best_idx)
  removed_idx <- sort(setdiff(row_id, best_idx))

  resolution <- rep("unique_record", n)
  resolution[best_idx[in_duplicate[best_idx]]] <- "kept_best_record"
  resolution[removed_idx] <- "removed_duplicate"

  add_resolution <- function(x, rows) {
    out <- tibble::as_tibble(x[rows, , drop = FALSE])
    if (!is.null(resolution_col)) out[[resolution_col]] <- resolution[rows]
    out
  }

  data <- add_resolution(roster, best_idx)
  removed <- add_resolution(roster, removed_idx)
  if (nrow(removed)) removed$dedup_group <- group_id[removed_idx]

  conflicts <- tibble::as_tibble(roster[0, , drop = FALSE])
  conflict_groups <- integer(0)
  if (!is.null(conflict_col)) {
    conflict_rows <- logical(n)
    conflict_group_values <- split(seq_len(n)[in_duplicate], key[in_duplicate])
    for (rows in conflict_group_values) {
      vals <- trimws(as.character(roster[[conflict_col]][rows]))
      vals <- vals[!is.na(vals) & nzchar(vals) & vals != "NA"]
      if (length(unique(vals)) > 1L) {
        conflict_rows[rows] <- TRUE
        conflict_groups <- c(conflict_groups, group_id[rows[1L]])
      }
    }
    conflicts <- tibble::as_tibble(roster[conflict_rows, , drop = FALSE])
    if (nrow(conflicts)) {
      conflicts$dedup_group <- group_id[conflict_rows]
      if (!is.null(resolution_col)) conflicts[[resolution_col]] <- resolution[conflict_rows]
    }
  }

  if (anyDuplicated(key[best_idx])) {
    stop("deduplicate_roster: internal validation failure; duplicate keys remain",
         call. = FALSE)
  }

  list(
    data = data,
    removed = removed,
    conflicts = conflicts,
    counts = list(
      input_rows = n,
      output_rows = nrow(data),
      removed_rows = nrow(removed),
      duplicate_groups = length(duplicate_keys),
      duplicate_rows = sum(in_duplicate),
      conflict_groups = length(conflict_groups)
    )
  )
}
