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

#' Is exactly one of 3+ sources the odd one out?
#'
#' A general disagreement among 3+ sources ("no two agree") and a lone
#' dissenter ("all but one agree") are different claims --
#' [attribute_consensus_tier()] reports both as `"split"` once no strict
#' majority exists. This detects the second, structurally narrower case
#' specifically: exactly one source's value differs from a value every
#' other source shares. It never says the lone source is wrong -- the
#' majority's value is never reported, only its size and which source sits
#' outside it -- so tallying this with [attribute_odd_one_out_rate()]
#' answers "how often is this source the one that's out of step," not
#' "how often is this source wrong."
#'
#' @details
#' **Structure, not just disagreement.** With 4 sources split 2-2, or
#' 5 sources split 3-1-1, no single source is "the" odd one out -- there
#' is no source whose value every *other* source shares. Only an
#' `n_reporting - 1` vs. `1` split qualifies; everything else with more
#' than one distinct value is `"no_single_outlier"`, deliberately not
#' merged into `"odd_one_out"`.
#'
#' **Normalization.** Identifiers are trimmed; attribute values are
#' trimmed and upper-cased, matching [attribute_agreement()].
#'
#' @inheritParams attribute_consensus_tier
#' @return A tibble, one row per identifier-year with >= 3 sources
#'   reporting: `id`, `year`, `n_reporting`, `n_sources_listed` (source
#'   names, sorted, joined by `"|"`), `n_distinct_values`, `structure`
#'   (a factor: `"unanimous"`, `"odd_one_out"`, `"no_single_outlier"`),
#'   and `odd_source` (the lone source's name when `structure ==
#'   "odd_one_out"`, `NA` otherwise).
#' @examples
#' s <- list(
#'   nppes = data.frame(npi = "1", year = 2024, state = "CO"),
#'   dac   = data.frame(npi = "1", year = 2024, state = "CO"),
#'   op    = data.frame(npi = "1", year = 2024, state = "NM"),
#'   pecos = data.frame(npi = "1", year = 2024, state = "CO")
#' )
#' attribute_odd_one_out(s, attribute = "state")
#' @seealso [attribute_odd_one_out_rate()] to tally this per source;
#'   [attribute_consensus_tier()] for the broader agreement-size claim;
#'   `vignette("consensus-and-corroboration")`.
#' @family no-gold-standard comparison
#' @export
attribute_odd_one_out <- function(sources, attribute, id = "npi", year = "year") {
  checkmate::assert_list(sources, min.len = 3, names = "unique")
  checkmate::assert_string(attribute, min.chars = 1)
  checkmate::assert_string(id, min.chars = 1)
  nm <- names(sources)

  long <- vector("list", length(nm))
  names(long) <- nm
  for (s in nm) {
    x <- sources[[s]]
    checkmate::assert_data_frame(x)
    for (col in c(id, attribute)) {
      if (!col %in% names(x)) {
        stop(sprintf("attribute_odd_one_out: source '%s' has no column '%s'", s, col), call. = FALSE)
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
  if (!nrow(long)) stop("attribute_odd_one_out: no non-missing attribute values in any source", call. = FALSE)

  groups <- split(long, paste(long$id, long$year, sep = ""))
  rows <- lapply(groups, function(g) {
    n_reporting <- nrow(g)
    if (n_reporting < 3L) return(NULL)
    tab <- table(g$value)
    sizes <- sort(as.integer(tab), decreasing = TRUE)
    structure_val <- if (length(tab) == 1L) {
      "unanimous"
    } else if (length(tab) == 2L && sizes[1] == n_reporting - 1L && sizes[2] == 1L) {
      "odd_one_out"
    } else {
      "no_single_outlier"
    }
    odd_source <- NA_character_
    if (structure_val == "odd_one_out") {
      odd_value <- names(tab)[tab == 1L]
      odd_source <- g$source[g$value == odd_value][1]
    }
    tibble::tibble(
      id = g$id[1], year = g$year[1], n_reporting = n_reporting,
      n_sources_listed = paste(sort(g$source), collapse = "|"),
      n_distinct_values = length(tab),
      structure = factor(structure_val, levels = c("unanimous", "odd_one_out", "no_single_outlier")),
      odd_source = odd_source
    )
  })
  out <- dplyr::bind_rows(rows)
  if (!nrow(out)) stop("attribute_odd_one_out: no identifier-year has >= 3 sources reporting", call. = FALSE)
  tibble::as_tibble(out[order(out$year, out$id), ])
}

#' Per-source rate of being the odd one out
#'
#' Tallies [attribute_odd_one_out()]'s output into a per-source rate: of
#' every identifier-year this source was part of a scorable (3+ source)
#' comparison, how often was it the lone dissenter? A source with no
#' `"odd_one_out"` rows at all still gets a row here, with `n_odd_one_out
#' = 0`, so it is never missing from the ranking just because it was
#' never the outlier.
#'
#' @param records Output of [attribute_odd_one_out()].
#' @param source_list,structure,odd_source Column names in `records`.
#'   Defaults match [attribute_odd_one_out()]'s output exactly.
#' @return A tibble, one row per source: `source`, `n_eligible`
#'   (identifier-years this source participated in), `n_odd_one_out`,
#'   `odd_rate` (`n_odd_one_out / n_eligible`, `NA` if `n_eligible == 0`).
#'   Ordered by descending `odd_rate`.
#' @examples
#' s <- list(
#'   nppes = data.frame(npi = c("1", "2"), year = 2024, state = c("CO", "NY")),
#'   dac   = data.frame(npi = c("1", "2"), year = 2024, state = c("CO", "NY")),
#'   op    = data.frame(npi = c("1", "2"), year = 2024, state = c("NM", "NY")),
#'   pecos = data.frame(npi = c("1", "2"), year = 2024, state = c("CO", "NJ"))
#' )
#' attribute_odd_one_out_rate(attribute_odd_one_out(s, attribute = "state"))
#' @seealso [attribute_odd_one_out()]; `vignette("consensus-and-corroboration")`.
#' @family no-gold-standard comparison
#' @export
attribute_odd_one_out_rate <- function(records, source_list = "n_sources_listed",
                                        structure = "structure", odd_source = "odd_source") {
  checkmate::assert_data_frame(records, min.rows = 1)
  for (col in c(source_list, structure, odd_source)) {
    if (!col %in% names(records)) {
      stop(sprintf("attribute_odd_one_out_rate: records has no column '%s'", col), call. = FALSE)
    }
  }
  participants <- strsplit(as.character(records[[source_list]]), "|", fixed = TRUE)
  is_odd <- as.character(records[[structure]]) == "odd_one_out"
  odd_src <- as.character(records[[odd_source]])

  all_sources <- sort(unique(unlist(participants)))
  rows <- lapply(all_sources, function(s) {
    eligible <- vapply(participants, function(p) s %in% p, logical(1))
    n_eligible <- sum(eligible)
    n_odd <- sum(eligible & is_odd & !is.na(odd_src) & odd_src == s)
    tibble::tibble(
      source = s, n_eligible = n_eligible, n_odd_one_out = n_odd,
      odd_rate = if (n_eligible > 0L) round(n_odd / n_eligible, 3) else NA_real_
    )
  })
  out <- dplyr::bind_rows(rows)
  tibble::as_tibble(out[order(-out$odd_rate), ])
}

#' Attribute agreement stratified by any covariate, not just spell age
#'
#' [attribute_agreement_by_spell_age()] stratifies agreement by a fixed set
#' of empirical-staleness bins. This generalizes the same question -- does
#' agreement differ across a grouping variable -- to any covariate at all:
#' rural vs. urban, provider type, years since licensure, or anything else
#' with one value per identifier. The covariate's values are used exactly
#' as given; pre-bin a continuous covariate yourself (for example with
#' `cut()`) if you want bands rather than one stratum per distinct value.
#'
#' @details
#' **Where the covariate lives.** `covariate_df` is a separate argument,
#' not assumed to be `source_a` or `source_b`, so the stratifying variable
#' can come from neither side of the comparison (for example a rurality
#' flag from a geography lookup). Pass `source_b` itself as `covariate_df`
#' to stratify by one of its own columns, as
#' [attribute_agreement_by_spell_age()] does for spell age.
#'
#' **Matching.** Rows join on `id` alone across all three data frames
#' (single cross-section only); there is no `year` argument. Pre-filter
#' each input to the year you want compared if your data is longitudinal.
#'
#' **Stratum order.** If `covariate_df`'s covariate column is a factor,
#' its levels set the output order; otherwise strata are sorted
#' alphabetically. A stratum with zero matched rows is never reported.
#'
#' @param source_a,source_b Data frames with an identifier column and
#'   the attribute column.
#' @param covariate_df Data frame with an identifier column and the
#'   covariate column (may be `source_a`, `source_b`, or neither).
#' @param attribute Name of the attribute column to compare, present in
#'   both `source_a` and `source_b`. Values are trimmed and upper-cased.
#' @param covariate Name of the stratifying column in `covariate_df`.
#' @param id Name of the identifier column, present in all three data
#'   frames. Default `"npi"`.
#' @param comparison_label Label stored in the `comparison` output column.
#' @param stratification_label Label stored in `stratification`. Default
#'   is `covariate`.
#' @return A tibble, one row per non-empty stratum: `comparison`,
#'   `stratification`, `stratum`, `n` (matched rows with non-missing
#'   attribute and covariate values), `agree_pct` (percent, one decimal).
#' @examples
#' source_a <- data.frame(npi = c("1", "2", "3"), state = c("CO", "NY", "TX"))
#' source_b <- data.frame(npi = c("1", "2", "3"), state = c("CO", "NJ", "TX"))
#' rurality <- data.frame(npi = c("1", "2", "3"), setting = c("rural", "urban", "urban"))
#' attribute_agreement_by_covariate(source_a, source_b, rurality,
#'                                  attribute = "state", covariate = "setting")
#' @seealso [attribute_agreement_by_spell_age()] for the empirical-staleness
#'   special case; `vignette("consensus-and-corroboration")`.
#' @family no-gold-standard comparison
#' @export
attribute_agreement_by_covariate <- function(source_a, source_b, covariate_df, attribute, covariate,
                                              id = "npi", comparison_label = "Source A vs Source B",
                                              stratification_label = covariate) {
  checkmate::assert_data_frame(source_a, min.rows = 1)
  checkmate::assert_data_frame(source_b, min.rows = 1)
  checkmate::assert_data_frame(covariate_df, min.rows = 1)
  checkmate::assert_string(attribute, min.chars = 1)
  checkmate::assert_string(covariate, min.chars = 1)
  for (col in c(id, attribute)) {
    if (!col %in% names(source_a)) {
      stop(sprintf("attribute_agreement_by_covariate: source_a has no column '%s'", col), call. = FALSE)
    }
    if (!col %in% names(source_b)) {
      stop(sprintf("attribute_agreement_by_covariate: source_b has no column '%s'", col), call. = FALSE)
    }
  }
  for (col in c(id, covariate)) {
    if (!col %in% names(covariate_df)) {
      stop(sprintf("attribute_agreement_by_covariate: covariate_df has no column '%s'", col), call. = FALSE)
    }
  }

  cov_raw <- covariate_df[[covariate]]
  stratum_levels <- if (is.factor(cov_raw)) {
    levels(cov_raw)
  } else {
    sort(unique(as.character(cov_raw)[!is.na(cov_raw) & nzchar(as.character(cov_raw))]))
  }

  da <- tibble::tibble(
    id = trimws(as.character(source_a[[id]])),
    value_a = toupper(trimws(as.character(source_a[[attribute]])))
  )
  db <- tibble::tibble(
    id = trimws(as.character(source_b[[id]])),
    value_b = toupper(trimws(as.character(source_b[[attribute]])))
  )
  dc <- tibble::tibble(
    id = trimws(as.character(covariate_df[[id]])),
    stratum = as.character(cov_raw)
  )

  joined <- merge(merge(da, db, by = "id"), dc, by = "id")
  joined <- joined[
    !is.na(joined$value_a) & nzchar(joined$value_a) &
      !is.na(joined$value_b) & nzchar(joined$value_b) &
      !is.na(joined$stratum) & nzchar(joined$stratum),
    , drop = FALSE
  ]
  if (!nrow(joined)) {
    stop("attribute_agreement_by_covariate: no matched rows with non-missing attribute and covariate values",
         call. = FALSE)
  }

  groups <- split(joined, factor(joined$stratum, levels = stratum_levels), drop = TRUE)
  rows <- lapply(groups, function(g) {
    tibble::tibble(
      comparison = comparison_label, stratification = stratification_label,
      stratum = g$stratum[1], n = nrow(g),
      agree_pct = round(100 * mean(g$value_a == g$value_b), 1)
    )
  })
  tibble::as_tibble(dplyr::bind_rows(rows))
}

#' Wilson score confidence interval for an agreement rate
#'
#' An agreement or concordance rate reported elsewhere in this package
#' (for example [attribute_agreement_summary()]'s `pct_agree` or
#' [attribute_consensus_tier()]'s `concordance_pct`) is a point estimate;
#' it reads very differently off 4 comparable identifier-years than off
#' 4,000. The Wilson score interval handles small `n_compared` and
#' extreme proportions (near `0` or `1`) better than a naive normal
#' approximation, and never produces a bound outside `[0, 1]`.
#'
#' Ported from a one-off midwifery-workforce-audit script's
#' `wilson_ci()` helper (`build_cross_source_comparison.R`), which applied
#' the same formula to this package's own `agree`-rate tables.
#'
#' @param n_agree,n_compared Numeric vectors of equal length: the count
#'   agreeing and the total compared. `n_agree` cannot exceed
#'   `n_compared`.
#' @param conf_level Confidence level. Default `0.95`.
#' @param as_percent If `FALSE` (default), bounds are proportions in
#'   `[0, 1]`, rounded to 4 decimals -- matching
#'   [attribute_agreement_summary()]'s `pct_agree`, itself a proportion
#'   despite the name. If `TRUE`, bounds are percentages in `[0, 100]`,
#'   rounded to 1 decimal -- matching [attribute_consensus_tier()]'s
#'   `concordance_pct`.
#' @return A tibble: `ci_low`, `ci_high`. `NA` where `n_compared` is `0`,
#'   missing, or negative.
#' @examples
#' wilson_ci(n_agree = 9, n_compared = 10)
#' wilson_ci(n_agree = c(90, 9), n_compared = c(100, 10), as_percent = TRUE)
#' @seealso [attribute_agreement_add_ci()] to attach these columns to an
#'   existing agreement table directly; `vignette("consensus-and-corroboration")`.
#' @family no-gold-standard comparison
#' @export
wilson_ci <- function(n_agree, n_compared, conf_level = 0.95, as_percent = FALSE) {
  checkmate::assert_numeric(n_agree, lower = 0)
  checkmate::assert_numeric(n_compared)
  checkmate::assert_number(conf_level, lower = 0, upper = 1)
  if (conf_level <= 0 || conf_level >= 1) {
    stop("wilson_ci: conf_level must be strictly between 0 and 1 (0 and 1 are degenerate -- ",
         "1 produces an undefined z-score, NaN bounds; 0 produces a zero-width interval ",
         "equal to the point estimate, never a real confidence interval)", call. = FALSE)
  }
  checkmate::assert_flag(as_percent)
  if (length(n_agree) != length(n_compared)) {
    stop("wilson_ci: n_agree and n_compared must be the same length", call. = FALSE)
  }
  if (any(n_agree > n_compared, na.rm = TRUE)) {
    stop("wilson_ci: n_agree cannot exceed n_compared", call. = FALSE)
  }

  z <- stats::qnorm(1 - (1 - conf_level) / 2)
  p <- n_agree / n_compared
  denom <- 1 + z^2 / n_compared
  center <- (p + z^2 / (2 * n_compared)) / denom
  halfwidth <- (z / denom) * sqrt(p * (1 - p) / n_compared + z^2 / (4 * n_compared^2))
  ci_low <- pmax(0, center - halfwidth)
  ci_high <- pmin(1, center + halfwidth)
  bad <- is.na(n_compared) | n_compared <= 0
  ci_low[bad] <- NA_real_
  ci_high[bad] <- NA_real_

  if (as_percent) {
    tibble::tibble(ci_low = round(100 * ci_low, 1), ci_high = round(100 * ci_high, 1))
  } else {
    tibble::tibble(ci_low = round(ci_low, 4), ci_high = round(ci_high, 4))
  }
}

#' Attach a Wilson confidence interval to an existing agreement table
#'
#' A thin wrapper around [wilson_ci()] that appends `ci_low`/`ci_high`
#' columns to a data frame in place, naming the input columns instead of
#' extracting and recombining vectors by hand.
#'
#' @param df A data frame with a count-agreeing and a count-compared
#'   column.
#' @param n_agree,n_compared Column names in `df`. Defaults
#'   `"n_agree"`/`"n_compared"`, matching [attribute_agreement_summary()].
#' @param conf_level,as_percent Passed to [wilson_ci()].
#' @return `df` as a tibble with `ci_low`/`ci_high` added.
#' @examples
#' s <- list(
#'   a = data.frame(npi = c("1", "2"), year = 2024, state = c("CO", "CO")),
#'   b = data.frame(npi = c("1", "2"), year = 2024, state = c("CO", "NM"))
#' )
#' attribute_agreement_add_ci(attribute_agreement_summary(attribute_agreement(s, attribute = "state")))
#' @seealso [wilson_ci()]; `vignette("consensus-and-corroboration")`.
#' @family no-gold-standard comparison
#' @export
attribute_agreement_add_ci <- function(df, n_agree = "n_agree", n_compared = "n_compared",
                                        conf_level = 0.95, as_percent = FALSE) {
  checkmate::assert_data_frame(df, min.rows = 1)
  for (col in c(n_agree, n_compared)) {
    if (!col %in% names(df)) {
      stop(sprintf("attribute_agreement_add_ci: df has no column '%s'", col), call. = FALSE)
    }
  }
  ci <- wilson_ci(df[[n_agree]], df[[n_compared]], conf_level = conf_level, as_percent = as_percent)
  df$ci_low <- ci$ci_low
  df$ci_high <- ci$ci_high
  tibble::as_tibble(df)
}
