#' Classify an identifier-year by how many sources agree, with no gold standard
#'
#' [attribute_agreement()] only ever compares two sources at a time. With
#' three or more sources sharing an identifier-year, a stronger relative
#' claim is available: how many of them agree with each other, as a group?
#' This never picks a "correct" value -- the value the largest group shares
#' is never materialized in the output, only its size -- so a three-way
#' split where no two sources agree is reported honestly as `"split"`
#' rather than silently resolved by picking whichever value happened to
#' come first.
#'
#' Ported and generalized from a one-off cross-source consensus script
#' (`build_cross_source_comparison.R`, midwifery workforce audit,
#' 2026-11-04): that script hardcoded its tier rule to an exactly-4-source
#' cohort (`== 4` sources agreeing is `"Unanimous"`, `== 3` is `"Majority"`,
#' else `"Split"`). The rule here -- unanimous when every reporting source
#' agrees, majority when the largest group is a strict majority
#' (`> n_reporting / 2`), split otherwise -- reduces to exactly that
#' 4-source rule and also works for any `n_reporting >= min_sources`.
#'
#' @details
#' **Sources and reporting.** Only sources with a non-missing value for an
#' identifier-year count toward `n_reporting`; a source that doesn't cover
#' that identifier-year at all is simply absent, not counted as
#' disagreeing. Identifier-years with fewer than `min_sources` reporting
#' are dropped entirely (not retained with `NA` tier), since "consensus"
#' is not a meaningful claim about a single source.
#'
#' **Normalization.** Identifiers are trimmed; attribute values are
#' trimmed and upper-cased, matching [attribute_agreement()].
#'
#' **Ties.** With an even `n_reporting` and no value holding a strict
#' majority (for example 2-2 across four sources), the tier is `"split"`,
#' not `"majority"` -- a plurality is not a majority.
#'
#' @param sources Named list, same shape as [attribute_agreement()]'s
#'   argument: each element is a data frame with an identifier column, the
#'   attribute column, and (optionally) a year column.
#' @param attribute Name of the attribute column to compare.
#' @param id Name of the identifier column. Default `"npi"`.
#' @param year Name of the year column, or `NULL` for a single cross-section.
#' @param min_sources Minimum number of sources that must report a
#'   non-missing value before an identifier-year is scored. Default `2`.
#' @return A tibble, one row per identifier-year with at least
#'   `min_sources` sources reporting: `id`, `year`, `n_reporting`,
#'   `n_sources_listed` (source names, sorted, joined by `"|"`),
#'   `n_distinct_values`, `n_agree_with_largest_group`, `concordance_pct`
#'   (a percentage, `0`-`100` -- unlike [attribute_agreement_summary()]'s
#'   `pct_agree`, which is a proportion), and `tier` (a factor:
#'   `"unanimous"`, `"majority"`, `"split"`).
#' @examples
#' s <- list(
#'   nppes = data.frame(npi = c("1", "2", "3"), year = 2024, state = c("CO", "CO", "TX")),
#'   dac   = data.frame(npi = c("1", "2", "3"), year = 2024, state = c("CO", "NM", "TX")),
#'   op    = data.frame(npi = c("1", "2", "3"), year = 2024, state = c("CO", "NM", "OK")),
#'   pecos = data.frame(npi = c("1", "2", "3"), year = 2024, state = c("CO", "CO", "TX"))
#' )
#' attribute_consensus_tier(s, attribute = "state")
#' @seealso [attribute_agreement()] for pairwise-only comparison;
#'   [attribute_consensus_tier_flow()] for whether a tier persists over
#'   time; `vignette("consensus-and-corroboration")`.
#' @family no-gold-standard comparison
#' @export
attribute_consensus_tier <- function(sources, attribute, id = "npi", year = "year", min_sources = 2) {
  checkmate::assert_list(sources, min.len = 2, names = "unique")
  checkmate::assert_string(attribute, min.chars = 1)
  checkmate::assert_string(id, min.chars = 1)
  checkmate::assert_int(min_sources, lower = 2)
  nm <- names(sources)

  long <- vector("list", length(nm))
  names(long) <- nm
  for (s in nm) {
    x <- sources[[s]]
    checkmate::assert_data_frame(x)
    for (col in c(id, attribute)) {
      if (!col %in% names(x)) {
        stop(sprintf("attribute_consensus_tier: source '%s' has no column '%s'", s, col), call. = FALSE)
      }
    }
    row <- tibble::tibble(
      id = trimws(as.character(x[[id]])),
      year = if (!is.null(year) && year %in% names(x)) x[[year]] else NA,
      value = toupper(trimws(as.character(x[[attribute]])))
    )
    row <- row[!is.na(row$value) & nzchar(row$value) & !is.na(row$id) & nzchar(row$id), ]
    row$source <- s
    long[[s]] <- row
  }
  long <- dplyr::bind_rows(long)
  if (!nrow(long)) stop("attribute_consensus_tier: no non-missing attribute values in any source", call. = FALSE)

  # interaction()+split() silently drops every row when `year` is NA for all
  # of them (the year = NULL / no-year-column path) -- factor() excludes NA
  # as a level by default, so the resulting grouping key has zero levels.
  # A pasted string key has no such special-casing of NA.
  groups <- split(long, paste(long$id, long$year, sep = ""))
  rows <- lapply(groups, function(g) {
    n_reporting <- nrow(g)
    if (n_reporting < min_sources) return(NULL)
    tab <- table(g$value)
    largest <- as.integer(max(tab))
    tier <- if (length(tab) == 1L) "unanimous" else if (largest > n_reporting / 2) "majority" else "split"
    tibble::tibble(
      id = g$id[1], year = g$year[1], n_reporting = n_reporting,
      n_sources_listed = paste(sort(g$source), collapse = "|"),
      n_distinct_values = length(tab),
      n_agree_with_largest_group = largest,
      concordance_pct = round(100 * largest / n_reporting, 1),
      tier = factor(tier, levels = c("unanimous", "majority", "split"))
    )
  })
  out <- dplyr::bind_rows(rows)
  if (!nrow(out)) {
    stop(sprintf("attribute_consensus_tier: no identifier-year has >= %d sources reporting", min_sources), call. = FALSE)
  }
  tibble::as_tibble(out[order(out$year, out$id), ])
}

#' Does a consensus tier persist across years, or churn?
#'
#' [attribute_consensus_tier()] answers one year at a time. Given its output
#' across multiple years, this counts year-over-year transitions between
#' tiers -- a claim about *stability*, distinct from the lead-lag functions'
#' claim about *which source moved first*. An identifier that is
#' `"unanimous"` in every year is informative in a different way than one
#' that flickers between `"unanimous"` and `"split"`.
#'
#' Ported from the same `build_cross_source_comparison.R` script's
#' longitudinal consensus-tier flow, generalized beyond its hardcoded
#' 4-source, 2021-2025 cohort to any tier table and any set of years.
#'
#' @details
#' **Which years.** Transitions are counted between each pair of
#' consecutive *distinct years present in `tiers`*, not consecutive
#' calendar years -- if `tiers` has 2021, 2022, and 2024 but no 2023, the
#' 2022 -> 2024 transition is still reported, labeled with both years, not
#' silently treated as a 1-year gap.
#'
#' **Cohort.** `require_all_years = TRUE` (the default, matching the
#' ported script) restricts to identifiers present in every year in
#' `tiers`, so the flow tracks a fixed cohort across the whole period
#' rather than a shifting population whose apparent "churn" could just be
#' entry and exit. Set it `FALSE` to instead count every consecutive-year
#' pair an identifier happens to have, dropping only that restriction.
#'
#' @param tiers A data frame with one row per identifier-year, such as
#'   [attribute_consensus_tier()]'s output.
#' @param id,year,tier Column names in `tiers`. Defaults `"id"`, `"year"`,
#'   `"tier"`.
#' @param require_all_years If `TRUE` (default), restrict to identifiers
#'   present in every distinct year found in `tiers`.
#' @return A tibble: `year_from`, `year_to`, `tier_from`, `tier_to`, `n`.
#'   Only observed transitions are included (a tier pair with zero
#'   identifiers contributes no row).
#' @examples
#' tiers <- data.frame(
#'   id = rep(c("1", "2", "3"), 2), year = rep(c(2023, 2024), each = 3),
#'   tier = c("unanimous", "split", "majority", "unanimous", "unanimous", "split")
#' )
#' attribute_consensus_tier_flow(tiers)
#' @seealso [attribute_consensus_tier()]; `vignette("consensus-and-corroboration")`.
#' @family no-gold-standard comparison
#' @export
attribute_consensus_tier_flow <- function(tiers, id = "id", year = "year", tier = "tier",
                                           require_all_years = TRUE) {
  checkmate::assert_data_frame(tiers, min.rows = 1)
  checkmate::assert_flag(require_all_years)
  for (col in c(id, year, tier)) {
    if (!col %in% names(tiers)) {
      stop(sprintf("attribute_consensus_tier_flow: tiers has no column '%s'", col), call. = FALSE)
    }
  }
  df <- tibble::tibble(
    id = trimws(as.character(tiers[[id]])),
    year = tiers[[year]],
    tier = as.character(tiers[[tier]])
  )
  if (anyDuplicated(df[, c("id", "year")])) {
    stop("attribute_consensus_tier_flow: tiers has duplicate (id, year) rows", call. = FALSE)
  }
  years <- sort(unique(df$year))
  if (length(years) < 2L) {
    stop("attribute_consensus_tier_flow: tiers has fewer than 2 distinct years", call. = FALSE)
  }

  if (require_all_years) {
    counts <- table(df$id)
    complete_ids <- names(counts)[counts == length(years)]
    df <- df[df$id %in% complete_ids, , drop = FALSE]
    if (!nrow(df)) {
      stop("attribute_consensus_tier_flow: no identifier is present in every year (require_all_years = TRUE)",
           call. = FALSE)
    }
  }

  flow <- vector("list", length(years) - 1L)
  for (k in seq_len(length(years) - 1L)) {
    y1 <- years[k]; y2 <- years[k + 1L]
    t1 <- df[df$year == y1, c("id", "tier")]; names(t1)[2] <- "tier_from"
    t2 <- df[df$year == y2, c("id", "tier")]; names(t2)[2] <- "tier_to"
    j <- merge(t1, t2, by = "id")
    if (!nrow(j)) next
    tab <- as.data.frame(table(tier_from = j$tier_from, tier_to = j$tier_to), stringsAsFactors = FALSE)
    names(tab)[names(tab) == "Freq"] <- "n"
    tab <- tab[tab$n > 0, , drop = FALSE]
    tab$year_from <- y1
    tab$year_to <- y2
    flow[[k]] <- tab[, c("year_from", "year_to", "tier_from", "tier_to", "n")]
  }
  out <- dplyr::bind_rows(flow)
  if (!nrow(out)) {
    stop("attribute_consensus_tier_flow: no identifier appears in two consecutive years", call. = FALSE)
  }
  tibble::as_tibble(out[order(out$year_from, out$tier_from, out$tier_to), ])
}

#' Which source does the ecosystem converge toward?
#'
#' [attribute_lead_lag_matrix()] answers "who moved first" for one pair of
#' sources. With more than two sources, a further relative claim is
#' available by aggregating across every pair: for each source, how often
#' did *other* sources eventually adopt its value after a disagreement,
#' versus how often did it get corrected toward *another* source's value?
#' A high `lead_rate` says other sources tend to catch up to this one --
#' not that this one is correct, only that it is the one the rest of the
#' comparison set converges toward.
#'
#' @details
#' **Pairs.** Every unordered pair of `sources` is run through
#' [attribute_lead_lag_matrix()] independently. A pair that errors (for
#' example, zero shared consecutive-year transitions) is skipped with a
#' message rather than failing the whole call; a pair with zero
#' base-year discordance for a given attribute contributes nothing for
#' that attribute.
#'
#' **What counts as "led" and "corrected".** For a pair A/B and attribute
#' `x`, if A and B disagree in `base_year` and B later adopts A's value
#' (`attribute_lead_lag_matrix()`'s `b_adopts_a_multi_n`), that is one
#' instance of A leading (and of B being corrected toward A); if A instead
#' reverts to B's value (`a_reverts_b_multi_n`), that is B leading. These
#' are tallied separately per source and summed across every pair it
#' appears in.
#'
#' **`base_year` consistency.** If `base_year = NULL` (the default), each
#' pair resolves its own base year independently (see
#' [attribute_lead_lag_matrix()]), which can differ across pairs with
#' different year coverage. Pass an explicit `base_year` for a ranking
#' that compares every source against the same baseline.
#'
#' **This is not a correctness score.** A source with `lead_rate` near 1
#' is the one other sources tend to adopt -- it could be the most
#' carefully maintained source, or simply the one every vendor's CRM
#' copies from. `opportunities_n` (how many disagreements this source was
#' even party to) and `n_pairs` (how many pairs contributed) are reported
#' alongside so a high rate on a handful of opportunities is never
#' confused with one backed by many.
#'
#' @param sources Named list of longitudinal data frames, same shape as
#'   [attribute_lead_lag_matrix()]'s `source_a`/`source_b`: each needs
#'   `id`, `year`, and the `attributes` columns.
#' @param attributes Character vector of attribute column names.
#' @param id,year Column names. Defaults `"npi"`, `"year"`.
#' @param base_year Passed to [attribute_lead_lag_matrix()] for every
#'   pair; see Details.
#' @return A tibble, one row per `(source, attribute)`: `led_n`,
#'   `corrected_n`, `opportunities_n` (total base-year discordances this
#'   source was party to, across all pairs), `n_pairs` (pairs that
#'   contributed at least one opportunity), and `lead_rate`
#'   (`led_n / (led_n + corrected_n)`, `NA` if that sum is `0`). Ordered by
#'   `attribute`, then descending `lead_rate`.
#' @examples
#' # b already has the eventual value; a and c both move to match it.
#' a <- data.frame(npi = c("1", "1"), year = c(2022, 2023), state = c("CO", "NM"))
#' b <- data.frame(npi = c("1", "1"), year = c(2022, 2023), state = c("NM", "NM"))
#' c <- data.frame(npi = c("1", "1"), year = c(2022, 2023), state = c("CO", "NM"))
#' attribute_source_corroboration(list(a = a, b = b, c = c), attributes = "state")
#' @seealso [attribute_lead_lag_matrix()] for the single-pair version;
#'   `vignette("consensus-and-corroboration")`.
#' @family no-gold-standard comparison
#' @export
attribute_source_corroboration <- function(sources, attributes, id = "npi", year = "year", base_year = NULL) {
  checkmate::assert_list(sources, min.len = 2, names = "unique")
  checkmate::assert_character(attributes, min.len = 1, any.missing = FALSE, unique = TRUE)
  nm <- names(sources)
  pairs <- utils::combn(nm, 2, simplify = FALSE)

  results <- vector("list", 0)
  for (p in pairs) {
    a_name <- p[1]; b_name <- p[2]
    ll <- tryCatch(
      attribute_lead_lag_matrix(sources[[a_name]], sources[[b_name]], attributes = attributes,
                                 id = id, year = year, base_year = base_year),
      error = function(e) {
        message(sprintf("attribute_source_corroboration: skipping pair '%s'/'%s': %s",
                         a_name, b_name, conditionMessage(e)))
        NULL
      }
    )
    if (is.null(ll)) next
    for (i in seq_len(nrow(ll))) {
      disc_n <- ll$disc_base_n[i]
      if (is.na(disc_n) || disc_n == 0L) next
      at <- ll$attribute[i]
      results[[length(results) + 1L]] <- tibble::tibble(
        source = a_name, attribute = at,
        led_n = ll$b_adopts_a_multi_n[i], corrected_n = ll$a_reverts_b_multi_n[i],
        opportunities_n = disc_n
      )
      results[[length(results) + 1L]] <- tibble::tibble(
        source = b_name, attribute = at,
        led_n = ll$a_reverts_b_multi_n[i], corrected_n = ll$b_adopts_a_multi_n[i],
        opportunities_n = disc_n
      )
    }
  }
  if (!length(results)) {
    stop("attribute_source_corroboration: no pair of sources produced a usable base-year disagreement", call. = FALSE)
  }
  long <- dplyr::bind_rows(results)

  groups <- split(long, interaction(long$source, long$attribute, drop = TRUE))
  rows <- lapply(groups, function(g) {
    led <- as.integer(sum(g$led_n)); corrected <- as.integer(sum(g$corrected_n))
    tibble::tibble(
      source = g$source[1], attribute = g$attribute[1],
      led_n = led, corrected_n = corrected,
      opportunities_n = as.integer(sum(g$opportunities_n)),
      n_pairs = nrow(g),
      lead_rate = if (led + corrected > 0L) round(led / (led + corrected), 3) else NA_real_
    )
  })
  out <- dplyr::bind_rows(rows)
  tibble::as_tibble(out[order(out$attribute, -out$lead_rate), ])
}
