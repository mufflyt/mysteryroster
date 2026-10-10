#' Cross-link registries by identifier into mutually exclusive overlap zones
#'
#' Each registry is a data frame holding an identifier column or a character
#' vector of identifiers. Identifiers are trimmed, coerced to character and
#' deduplicated within each registry; missing or empty identifiers are dropped
#' and counted. The result has one row per identifier present in at least one
#' registry, one logical membership column per registry, and an `overlap_zone`
#' label built from the registry names in the order given (for three
#' registries this yields the seven Euler zones).
#'
#' @param sources Named list. Each element is a data frame with an `id` column
#'   or a character vector of identifiers. Names are the registry labels and
#'   must be unique, non-empty and free of `+`.
#' @param id Name of the identifier column in data-frame sources. Default `"npi"`.
#' @return A tibble with columns `id`, `in_<registry>` (logical, one per
#'   registry) and `overlap_zone` (registry names joined by `+`).
#'   Attribute `registries` holds the registry names in order and attribute
#'   `dropped` the count of missing or empty identifiers dropped per registry.
#' @examples
#' ov <- registry_overlap(list(
#'   nppes = c("1", "2", "3", "4"),
#'   board = c("2", "3", "5"),
#'   society = c("3", "4")))
#' overlap_zone_counts(ov)
#' @export
registry_overlap <- function(sources, id = "npi") {
  checkmate::assert_list(sources, min.len = 1, names = "unique")
  checkmate::assert_string(id, min.chars = 1)
  nm <- names(sources)
  if (any(!nzchar(nm)) || any(grepl("+", nm, fixed = TRUE))) {
    stop("registry_overlap: every source needs a non-empty name without '+'", call. = FALSE)
  }
  dropped <- integer(length(nm)); names(dropped) <- nm
  members <- vector("list", length(nm)); names(members) <- nm
  for (s in nm) {
    x <- sources[[s]]
    if (is.data.frame(x)) {
      if (!id %in% names(x)) {
        stop(sprintf("registry_overlap: source '%s' has no column '%s'", s, id), call. = FALSE)
      }
      x <- x[[id]]
    }
    x <- trimws(as.character(x))
    bad <- is.na(x) | !nzchar(x)
    dropped[[s]] <- sum(bad)
    members[[s]] <- unique(x[!bad])
  }
  all_ids <- unique(unlist(members, use.names = FALSE))
  if (!length(all_ids)) stop("registry_overlap: no identifiers in any source", call. = FALSE)
  out <- tibble::tibble(id = all_ids)
  for (s in nm) out[[paste0("in_", s)]] <- out$id %in% members[[s]]
  flags <- as.matrix(out[, paste0("in_", nm), drop = FALSE])
  out$overlap_zone <- apply(flags, 1, function(r) paste(nm[r], collapse = "+"))
  attr(out, "registries") <- nm
  attr(out, "dropped") <- dropped
  out
}

#' Count identifiers per overlap zone, with closure checks
#'
#' @param overlap Output of [registry_overlap()].
#' @return A tibble `zone`, `n`, `n_registries`, ordered by `n_registries`
#'   then `zone`. Stops if the zone counts do not sum to the number of
#'   identifiers or if any registry's total does not equal the sum of the
#'   zones containing it (the "does not close" guard).
#' @export
overlap_zone_counts <- function(overlap) {
  nm <- attr(overlap, "registries")
  checkmate::assert_character(nm, min.len = 1)
  tab <- table(overlap$overlap_zone)
  counts <- tibble::tibble(zone = names(tab), n = as.integer(tab))
  counts$n_registries <- lengths(strsplit(counts$zone, "+", fixed = TRUE))
  counts <- counts[order(counts$n_registries, counts$zone), ]
  if (sum(counts$n) != nrow(overlap)) {
    stop("overlap_zone_counts: zone counts do not close on the identifier total", call. = FALSE)
  }
  for (s in nm) {
    in_zone <- vapply(strsplit(counts$zone, "+", fixed = TRUE), function(z) s %in% z, logical(1))
    if (sum(counts$n[in_zone]) != sum(overlap[[paste0("in_", s)]])) {
      stop(sprintf("overlap_zone_counts: registry '%s' total does not close on its zones", s), call. = FALSE)
    }
  }
  tibble::as_tibble(counts)
}

#' Restrict an overlap table to the frame registry and label protocol strata
#'
#' The sampling frame of a listing-validity study is one registry (usually
#' NPPES); the strata are that registry's overlap with the others. Rows not in
#' the frame registry are dropped, and `stratum` is the overlap zone.
#'
#' @param overlap Output of [registry_overlap()].
#' @param frame Name of the frame registry. Default: the first registry.
#' @return The frame rows with a `stratum` column (a factor, levels in the
#'   order of [overlap_zone_counts()]).
#' @export
frame_strata <- function(overlap, frame = attr(overlap, "registries")[1]) {
  nm <- attr(overlap, "registries")
  checkmate::assert_choice(frame, nm)
  out <- overlap[overlap[[paste0("in_", frame)]], , drop = FALSE]
  if (!nrow(out)) stop(sprintf("frame_strata: no identifiers in frame registry '%s'", frame), call. = FALSE)
  lv <- overlap_zone_counts(out)$zone
  out$stratum <- factor(out$overlap_zone, levels = lv)
  attr(out, "registries") <- nm
  out
}

#' Area-proportional Euler diagram of registry overlap
#'
#' Requires the `eulerr` package. Zone counts come from
#' [overlap_zone_counts()], so the figure cannot disagree with the table.
#'
#' @param overlap Output of [registry_overlap()].
#' @param ... Passed to `eulerr::euler()` (for example `shape = "ellipse"`).
#' @return The `eulerr` fit object, invisibly; call `plot()` on it.
#' @export
plot_registry_overlap <- function(overlap, ...) {
  if (!requireNamespace("eulerr", quietly = TRUE)) {
    stop("plot_registry_overlap: install the 'eulerr' package", call. = FALSE)
  }
  counts <- overlap_zone_counts(overlap)
  combos <- gsub("+", "&", counts$zone, fixed = TRUE)
  fit <- eulerr::euler(stats::setNames(counts$n, combos), ...)
  invisible(fit)
}
