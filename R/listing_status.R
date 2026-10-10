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

#' Turn call dispositions into four exhaustive, mutually exclusive listing statuses
#'
#' Physician presence is never inferred from a non-missing new-patient answer
#' or from a reached office; it comes only from the disposition categories in
#' `map`. A non-missing disposition absent from every category is a codebook
#' drift and stops the call unless `unmapped = "unresolved"`.
#'
#' @param calls Data frame, one row per listing (or per call if you aggregate
#'   first).
#' @param disposition Column with the disposition string.
#' @param reached Logical column: `TRUE` office answered, `FALSE` attempted but
#'   not reached, `NA` not attempted.
#' @param phone_valid Optional logical column: `FALSE` when the listed number
#'   was invalid or disconnected (a confirmed-invalid listing).
#' @param map A disposition map; see [ent_disposition_map()].
#' @param unmapped `"error"` (default) or `"unresolved"`.
#' @return `calls` with `physician_at_location` and `physician_active`
#'   (`confirmed_yes` / `confirmed_no` / `unknown`), `invalid_reason`
#'   (`wrong_disconnected_number`, `relocated`, `retired_inactive`,
#'   `not_reached_valid_phone`, `not_specified`; `NA` unless the status is
#'   `confirmed_invalid`) and `listing_status`,
#'   a factor with levels `confirmed_valid`, `confirmed_invalid`, `unresolved`,
#'   `not_attempted`.
#' @export
classify_listing_status <- function(calls, disposition = "disposition", reached = "reached",
                                    phone_valid = NULL, map = ent_disposition_map(),
                                    unmapped = c("error", "unresolved")) {
  checkmate::assert_data_frame(calls)
  unmapped <- match.arg(unmapped)
  for (col in c(disposition, reached, phone_valid)) {
    if (!col %in% names(calls)) stop(sprintf("classify_listing_status: no column '%s'", col), call. = FALSE)
  }
  checkmate::assert_list(map, names = "unique")
  need <- c("at_location", "relocated", "retired", "not_specified")
  if (!setequal(names(map), need)) {
    stop("classify_listing_status: map must have exactly the categories ",
         paste(need, collapse = ", "), call. = FALSE)
  }
  all_codes <- unlist(map, use.names = FALSE)
  if (anyDuplicated(all_codes)) {
    stop("classify_listing_status: disposition map categories overlap: ",
         paste(unique(all_codes[duplicated(all_codes)]), collapse = ", "), call. = FALSE)
  }
  d <- as.character(calls[[disposition]])
  r <- calls[[reached]]
  checkmate::assert_logical(r)
  pv <- if (is.null(phone_valid)) rep(TRUE, nrow(calls)) else calls[[phone_valid]]
  checkmate::assert_logical(pv)
  pv[is.na(pv)] <- TRUE
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
#' @export
listing_status_levels <- function() {
  c("confirmed_valid", "confirmed_invalid", "unresolved", "not_attempted")
}
