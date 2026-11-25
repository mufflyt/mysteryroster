#' Disposition codebook used by the ENT registry-validity study
#'
#' A disposition map is a named list of character vectors. The four names are
#' fixed; the values are the exact disposition strings a study's call log uses.
#' `at_location`: staff affirmed the physician is affiliated with this practice
#' (including "full" and "on leave"). `relocated`: staff said the physician
#' changed practices (invalid here, active elsewhere). `retired`: retired,
#' deceased or left medicine. `not_specified`: a "no" with no reason given.
#' Categories must be disjoint.
#'
#' @return A disposition map for the `taking_new_patients` REDCap field of the
#'   ENT study. Replace the strings for another study's codebook.
#' @examples
#' ent_disposition_map()
#' @export
ent_disposition_map <- function() {
  list(
    at_location = c("Yes",
                    "No-practice is full but physician does still see established patients",
                    "No-on leave of absence (like parental leave)"),
    relocated = "No-changed practices",
    retired = "No-retired or no longer practicing medicine",
    not_specified = "No-not specified why"
  )
}

#' Draft disposition codebook for the OB/GYN subspecialty registry-validity study
#'
#' Written from the protocol's outcome definitions (COMIRB draft, 2026-10-09,
#' Section 7) before the call scripts and REDCap codebook exist, so the strings
#' are placeholders to be replaced one-for-one once the disposition field is
#' finalised. The four categories are the same as [ent_disposition_map()]:
#' `at_location` is "practice-confirmed" (sees patients in the indexed
#' subspecialty at this site, whether or not accepting new patients);
#' `relocated` is "workforce-confirmed elsewhere"; `retired` covers
#' retirement, death, leaving medicine and no longer providing the indexed
#' care; `not_specified` is a "no" with no reason. New-patient acceptance is a
#' separate measure and is not a disposition category.
#'
#' @return A disposition map (named list of character vectors).
#' @examples
#' obgyn_disposition_map()
#' @export
obgyn_disposition_map <- function() {
  list(
    at_location = c("Sees patients here-accepting new patients",
                    "Sees patients here-not accepting new patients",
                    "Sees patients here-on leave"),
    relocated = "Moved to another practice location",
    retired = c("Retired or no longer practicing",
                "No longer provides this subspecialty care"),
    not_specified = "No-reason not given"
  )
}

#' Coerce common yes/no encodings to logical
#'
#' REDCap exports and hand-entered call logs rarely store flags as logical:
#' the same column arrives as `"yes"`/`"no"`, `"Y"`/`"N"`, `"true"`/`"false"`,
#' `1`/`0` or some mixture, and [classify_listing_status()] needs a logical
#' `reached` column. This helper normalises those encodings. Comparison is
#' case-insensitive after trimming whitespace; empty strings count as missing.
#' Logical input passes through unchanged. A value outside the recognised
#' encodings stops the call by default so a codebook drift cannot silently
#' become a missing value; set `unrecognised = "na"` only when the drift is
#' understood.
#'
#' @param x A logical, numeric, character or factor vector.
#' @param unrecognised `"error"` (default) or `"na"`.
#' @return A logical vector the length of `x`. `yes`/`y`/`true`/`t`/`1` map to
#'   `TRUE`; `no`/`n`/`false`/`f`/`0` map to `FALSE`; missing values and empty
#'   strings stay `NA`.
#' @examples
#' coerce_logical_flag(c("Yes", "no", "TRUE", 1, 0, NA, ""))
#' @export
coerce_logical_flag <- function(x, unrecognised = c("error", "na")) {
  unrecognised <- match.arg(unrecognised)
  if (is.logical(x)) return(x)
  if (!is.atomic(x)) {
    stop("coerce_logical_flag: x must be logical, numeric, character or factor",
         call. = FALSE)
  }
  true_tokens <- c("yes", "y", "true", "t", "1")
  false_tokens <- c("no", "n", "false", "f", "0")
  token <- tolower(trimws(as.character(x)))
  token[!nzchar(token)] <- NA_character_
  recognised <- token %in% c(true_tokens, false_tokens)
  bad <- !is.na(token) & !recognised
  if (any(bad)) {
    values <- unique(as.character(x)[bad])
    if (unrecognised == "error") {
      stop("coerce_logical_flag: unrecognised value(s): ",
           paste(values, collapse = "; "), call. = FALSE)
    }
  }
  out <- rep_len(NA, length(x))
  out[token %in% true_tokens] <- TRUE
  out[token %in% false_tokens] <- FALSE
  out
}

#' Turn call dispositions into four exhaustive, mutually exclusive listing statuses
#'
#' Physician presence is never inferred from a non-missing new-patient answer
#' or from a reached office; it comes only from the disposition categories in
#' `map`. A non-missing disposition absent from every category is a codebook
#' drift and stops the call unless `unmapped = "unresolved"`. Leading and
#' trailing whitespace is never meaningful in a disposition codebook, so both
#' the data and the map are trimmed before matching: formatting noise is
#' ignored, while any remaining mismatch still fails closed. Empty strings
#' count as missing export cells, not as dispositions.
#'
#' @param calls Data frame, one row per listing (or per call if you aggregate
#'   first).
#' @param disposition Column with the disposition string.
#' @param reached Logical column: `TRUE` office answered, `FALSE` attempted but
#'   not reached, `NA` not attempted. [coerce_logical_flag()] normalises the
#'   yes/no/1/0 encodings REDCap exports arrive in.
#' @param phone_valid Optional logical column: `FALSE` when the listed number
#'   was invalid or disconnected (a confirmed-invalid listing). An `NA` here
#'   means the validity of the number is unknown -- usually the field has not
#'   been transcribed yet -- and is treated as a valid number, because an
#'   untranscribed field is not evidence against the listing. `phone_valid_na`
#'   controls whether that happens silently, with a warning, or not at all.
#' @param phone_valid_na What to do when `phone_valid` is `NA`: `"warn"`
#'   (default) treats the missing values as valid numbers and warns once with
#'   the count, so the assumption is never invisible; `"valid"` treats them as
#'   valid numbers silently (the pre-0.4.0 behaviour); `"error"` stops, forcing
#'   the analyst to resolve or explicitly excuse every missing value.
#' @param map A disposition map; see [ent_disposition_map()].
#' @param unmapped `"error"` (default) or `"unresolved"`.
#' @return `calls` with `physician_at_location` and `physician_active`
#'   (`confirmed_yes` / `confirmed_no` / `unknown`), `invalid_reason`
#'   (`wrong_disconnected_number`, `relocated`, `retired_inactive`,
#'   `not_reached_valid_phone`, `not_specified`; `NA` unless the status is
#'   `confirmed_invalid`) and `listing_status`,
#'   a factor with levels `confirmed_valid`, `confirmed_invalid`, `unresolved`,
#'   `not_attempted`.
#' @examples
#' calls <- data.frame(
#'   disposition = c("Yes", "No-changed practices", "No-retired or no longer practicing medicine", NA),
#'   reached = c(TRUE, TRUE, TRUE, NA))
#' classify_listing_status(calls)[, c("disposition", "listing_status", "invalid_reason")]
#' @export
classify_listing_status <- function(calls, disposition = "disposition", reached = "reached",
                                    phone_valid = NULL, map = ent_disposition_map(),
                                    unmapped = c("error", "unresolved"),
                                    phone_valid_na = c("warn", "valid", "error")) {
  checkmate::assert_data_frame(calls)
  unmapped <- match.arg(unmapped)
  phone_valid_na <- match.arg(phone_valid_na)
  for (col in c(disposition, reached, phone_valid)) {
    if (!col %in% names(calls)) stop(sprintf("classify_listing_status: no column '%s'", col), call. = FALSE)
  }
  checkmate::assert_list(map, names = "unique")
  need <- c("at_location", "relocated", "retired", "not_specified")
  if (!setequal(names(map), need)) {
    stop("classify_listing_status: map must have exactly the categories ",
         paste(need, collapse = ", "), call. = FALSE)
  }
  # Whitespace is never meaningful in a disposition codebook: REDCap exports
  # routinely carry trailing spaces from manual edits, and erroring on them
  # would force every study to re-discover the same trivial cleaning step.
  # Both the map and the data are trimmed before matching, so drift beyond
  # whitespace still fails closed while formatting noise does not. The overlap
  # check runs on the trimmed codes, so two categories that differ only by
  # whitespace are caught as the overlap they are.
  map <- lapply(map, trimws)
  all_codes <- unlist(map, use.names = FALSE)
  if (anyDuplicated(all_codes)) {
    stop("classify_listing_status: disposition map categories overlap: ",
         paste(unique(all_codes[duplicated(all_codes)]), collapse = ", "), call. = FALSE)
  }
  d <- trimws(as.character(calls[[disposition]]))
  # An empty string is a missing export cell, not a disposition: treat it as
  # NA rather than as codebook drift, matching coerce_logical_flag().
  d[!is.na(d) & !nzchar(d)] <- NA_character_
  r <- calls[[reached]]
  checkmate::assert_logical(r)
  pv <- if (is.null(phone_valid)) rep(TRUE, nrow(calls)) else calls[[phone_valid]]
  checkmate::assert_logical(pv)
  if (anyNA(pv)) {
    n_missing <- sum(is.na(pv))
    msg <- sprintf(paste0(
      "classify_listing_status: %d phone_valid value(s) are NA and are treated",
      " as valid numbers; an untranscribed field is not evidence against the",
      " listing (phone_valid_na = \"%s\")"), n_missing, phone_valid_na)
    if (phone_valid_na == "error") stop(msg, call. = FALSE)
    if (phone_valid_na == "warn") warning(msg, call. = FALSE)
    pv[is.na(pv)] <- TRUE
  }
  known <- is.na(d) | d %in% all_codes
  if (any(!known)) {
    msg <- sprintf("classify_listing_status: %d disposition value(s) not in the map: %s",
                   sum(!known), paste(unique(d[!known]), collapse = "; "))
    if (unmapped == "error") stop(msg, call. = FALSE)
    message(msg, " -> unresolved")
  }
  at_loc <- ifelse(d %in% map$at_location, "confirmed_yes",
            ifelse(d %in% map$relocated, "confirmed_no", "unknown"))
  active <- ifelse(d %in% c(map$at_location, map$relocated), "confirmed_yes",
            ifelse(d %in% map$retired, "confirmed_no", "unknown"))
  reason <- dplyr::case_when(
    !pv                                   ~ "wrong_disconnected_number",
    d %in% map$relocated                  ~ "relocated",
    d %in% map$retired                    ~ "retired_inactive",
    !is.na(r) & !r & pv                   ~ "not_reached_valid_phone",
    d %in% map$not_specified              ~ "not_specified",
    TRUE                                  ~ NA_character_
  )
  status <- dplyr::case_when(
    at_loc == "confirmed_yes"             ~ "confirmed_valid",
    at_loc == "confirmed_no"              ~ "confirmed_invalid",
    active == "confirmed_no"              ~ "confirmed_invalid",
    !pv                                   ~ "confirmed_invalid",
    is.na(r) & is.na(d)                   ~ "not_attempted",
    TRUE                                  ~ "unresolved"
  )
  reason[status != "confirmed_invalid"] <- NA_character_   # a reason exists only for an invalid listing
  calls$physician_at_location <- at_loc
  calls$physician_active <- active
  calls$invalid_reason <- reason
  calls$listing_status <- factor(status, levels = listing_status_levels())
  calls
}

#' The four listing statuses, in reporting order
#' @return Character vector of length four.
#' @examples
#' listing_status_levels()
#' @export
listing_status_levels <- function() {
  c("confirmed_valid", "confirmed_invalid", "unresolved", "not_attempted")
}
