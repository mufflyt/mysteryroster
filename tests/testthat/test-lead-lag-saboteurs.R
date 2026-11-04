# =============================================================================
# 25 Adversarial ("Saboteur") Tests for lead_lag_staleness.R in mysteryroster
# =============================================================================
# Designed to break attribute_spell_age(), attribute_lead_lag_matrix(),
# attribute_agreement_by_spell_age(), enrollment_move_lead_lag(), and
# format_no_gold_standard_prose() with boundary conditions, silent silent-join
# fanouts, NA/whitespace/factor traps, gap years, and malformed inputs.
# =============================================================================

# --- S1-S5: attribute_spell_age() saboteurs ----------------------------------

test_that("S1: attribute_spell_age breaks a spell when there is a calendar-year gap in the panel", {
  # If a provider is present in 2020 at '10 MAIN ST', disappears in 2021-2023,
  # and reappears in 2024 at '10 MAIN ST', is that a 4-year uninterrupted spell?
  # Or if they had a gap year, should a missing intermediate year reset or preserve?
  # More importantly: what if a provider has duplicate (id, year) rows in the input panel?
  dup_panel <- data.frame(
    npi = c("1", "1", "1"),
    year = c(2023, 2023, 2024),
    street = c("10 MAIN", "20 OAK", "20 OAK")
  )
  expect_error(
    attribute_spell_age(dup_panel, attribute = "street"),
    "duplicate|unique"
  )
})

test_that("S2: attribute_spell_age handles all-NA or whitespace-only attribute values without claiming a valid multi-year spell", {
  # If street is NA in 2021, 2022, 2023, spell_age_years must be NA (not 0, or worse, 2!)
  na_panel <- data.frame(
    npi = c("1", "1", "1"),
    year = c(2021, 2022, 2023),
    street = c(NA_character_, "   ", "")
  )
  out <- attribute_spell_age(na_panel, attribute = "street")
  expect_true(all(is.na(out$spell_age_years)))
  expect_true(all(is.na(out$spell_start_year)))
})

test_that("S3: attribute_spell_age rejects non-integer or NA years", {
  bad_yr <- data.frame(
    npi = c("1", "1"),
    year = c(2023, NA),
    street = c("10 MAIN", "10 MAIN")
  )
  expect_error(attribute_spell_age(bad_yr, attribute = "street"), "year|NA|missing")
})

test_that("S4: attribute_spell_age preserves row order / handles unsorted input years and factor columns", {
  unsorted <- data.frame(
    npi = factor(c("B", "A", "A", "A")),
    year = c(2024L, 2024L, 2022L, 2023L),
    street = factor(c("9 ELM", "10 MAIN", "10 MAIN", "10 MAIN"))
  )
  out <- attribute_spell_age(unsorted, attribute = "street")
  a_rows <- out[out$npi == "A", ]
  expect_equal(a_rows$year, c(2022L, 2023L, 2024L))
  expect_equal(a_rows$spell_age_years, c(0L, 1L, 2L))
})

test_that("S5: attribute_spell_age resets spell_age_years to 0 when a provider returns to an old address (A -> B -> A)", {
  aba <- data.frame(
    npi = c("1", "1", "1", "1"),
    year = c(2021, 2022, 2023, 2024),
    street = c("10 MAIN", "10 MAIN", "99 OTHER", "10 MAIN")
  )
  out <- attribute_spell_age(aba, attribute = "street")
  # In 2024, '10 MAIN' is a NEW spell starting in 2024 (age 0), NOT continuing from 2021 (age 3)!
  # Wait: base R ave(year, spell_id, FUN = min) works if spell_id is strictly increasing,
  # let's verify 2024 gets spell_start_year == 2024 and spell_age_years == 0.
  expect_equal(out$spell_start_year, c(2021L, 2021L, 2023L, 2024L))
  expect_equal(out$spell_age_years, c(0L, 1L, 0L, 0L))
})

# --- S6-S11: attribute_lead_lag_matrix() saboteurs ---------------------------

test_that("S6: attribute_lead_lag_matrix fails closed on duplicate (id, year) rows in source_a or source_b", {
  # A many-to-many join on duplicate (id, year) silently multiplies transition counts!
  sa_dup <- data.frame(
    npi = c("1", "1", "1"),
    year = c(2022, 2022, 2023),
    state = c("CO", "NM", "NM")
  )
  sb <- data.frame(
    npi = c("1", "1"),
    year = c(2022, 2023),
    state = c("CO", "CO")
  )
  expect_error(
    attribute_lead_lag_matrix(sa_dup, sb, attributes = "state"),
    "duplicate|unique"
  )
})

test_that("S7: attribute_lead_lag_matrix fails closed on duplicate id rows in cross_section", {
  sa <- data.frame(npi = c("1", "1"), year = c(2022, 2023), state = c("CO", "CO"))
  sb <- data.frame(npi = c("1", "1"), year = c(2022, 2023), state = c("CO", "CO"))
  cs_dup <- data.frame(npi = c("1", "1"), state = c("NM", "TX"))
  expect_error(
    attribute_lead_lag_matrix(sa, sb, attributes = "state", cross_section = cs_dup, cross_section_year = 2023L),
    "duplicate|unique"
  )
})

test_that("S8: attribute_lead_lag_matrix handles zero discordant pairs at t without NaN/Inf", {
  # Perfect agreement everywhere -> n_discordant_at_t == 0, disc_base_n == 0
  sa <- data.frame(npi = c("1", "1", "2", "2"), year = c(2022, 2023, 2022, 2023), state = c("CO", "CO", "NY", "NY"))
  sb <- data.frame(npi = c("1", "1", "2", "2"), year = c(2022, 2023, 2022, 2023), state = c("CO", "CO", "NY", "NY"))
  ll <- attribute_lead_lag_matrix(sa, sb, attributes = "state")
  expect_equal(ll$n_discordant_at_t, 0L)
  expect_true(is.na(ll$b_adopts_a_at_t1_pct))
  expect_false(is.nan(ll$b_adopts_a_at_t1_pct))
  expect_equal(ll$disc_base_n, 0L)
  expect_true(is.na(ll$b_adopts_a_multi_pct))
})

test_that("S9: attribute_lead_lag_matrix does not drop base_year discordances that have no follow-up years from disc_base_n consistency", {
  # Suppose NPI 1 is discordant in 2022 and observed in 2023, while NPI 2 is discordant in 2022
  # and observed in source_b in 2023, but NOT in source_a in 2023!
  # In our implementation, m_b had 2 NPIs (NPI 1 and 2) while m_a only had 1 NPI (NPI 1).
  # Yet only a single `disc_base_n` column is returned! If b_adopts_a_multi_pct uses length(by_id_b)
  # and a_reverts_b_multi_pct uses length(by_id_a), their denominators differ while reporting one disc_base_n!
  sa <- data.frame(
    npi = c("1", "1", "2"),
    year = c(2022, 2023, 2022),
    state = c("NM", "NM", "TX")
  )
  sb <- data.frame(
    npi = c("1", "1", "2", "2"),
    year = c(2022, 2023, 2022, 2023),
    state = c("CO", "NM", "CO", "CO")
  )
  ll <- attribute_lead_lag_matrix(sa, sb, attributes = "state", base_year = 2022L)
  # Check that a_reverts_b_multi_n / disc_base_n or separate denominator is tracked explicitly
  expect_true("disc_base_a_followup_n" %in% names(ll) || ll$disc_base_n == 2L)
})

test_that("S10: attribute_lead_lag_matrix handles a 3-way simultaneous change at t+1 (A_t == B_t == X, A_{t+1} = Y, B_{t+1} = Z, Y != Z)", {
  # Both A and B change at t+1, but to DIFFERENT values (Y != Z)!
  # Neither broke concordance alone (since the other didn't stay at X), AND they didn't move together (Y != Z)!
  sa <- data.frame(npi = c("1", "1"), year = c(2022, 2023), state = c("CO", "NM"))
  sb <- data.frame(npi = c("1", "1"), year = c(2022, 2023), state = c("CO", "TX"))
  ll <- attribute_lead_lag_matrix(sa, sb, attributes = "state")
  # Must not be counted as both_move_together_at_t1, nor a_breaks_concordance_first (where B stays),
  # and should be tracked or cleanly separated so counts never exceed n_concordant_at_t
  expect_equal(ll$both_move_together_at_t1, 0L)
  expect_equal(ll$a_breaks_concordance_first, 0L)
  expect_equal(ll$b_breaks_concordance_first, 0L)
})

test_that("S11: attribute_lead_lag_matrix handles mutual swap at t+1 (A_t = X, B_t = Y -> A_{t+1} = Y, B_{t+1} = X)", {
  # If A_t = NY, B_t = NJ, and at t+1 A_{t+1} = NJ and B_{t+1} = NY (they crossed!),
  # did B adopt A_t AND A revert to B_t on the exact same transition, making the sum > 100%?
  # True adoption of A_t by B requires that they actually CONVERGE or that B adopts A_t;
  # let's check what happens and ensure percentages are well-behaved.
  sa <- data.frame(npi = c("1", "1"), year = c(2022, 2023), state = c("NY", "NJ"))
  sb <- data.frame(npi = c("1", "1"), year = c(2022, 2023), state = c("NJ", "NY"))
  ll <- attribute_lead_lag_matrix(sa, sb, attributes = "state")
  expect_equal(ll$n_discordant_at_t, 1L)
  expect_lte(ll$b_adopts_a_at_t1_pct, 100)
})

# --- S12-S16: attribute_agreement_by_spell_age() saboteurs -------------------

test_that("S12: attribute_agreement_by_spell_age fails closed on duplicate keys in source_a or panel_b", {
  pb <- data.frame(
    npi = c("1", "1"),
    year = c(2024, 2024),
    state = c("CO", "CO"),
    spell_age_years = c(0L, 1L)
  )
  sa <- data.frame(npi = "1", state = "CO")
  expect_error(
    attribute_agreement_by_spell_age(sa, pb, attributes = "state", target_year = 2024L),
    "duplicate|unique"
  )
})

test_that("S13: attribute_agreement_by_spell_age rejects negative spell_age_years instead of silently bucketing them as '0 yrs'", {
  # Look at classify_age(): `yrs <= 0L ~ age_levels[1]`!
  # A corrupted negative spell_age_years (-5) was silently bucketed into "0 yrs (updated in snapshot yr)"!
  pb <- data.frame(
    npi = "1",
    year = 2024L,
    state = "CO",
    spell_age_years = -5L
  )
  sa <- data.frame(npi = "1", state = "CO")
  expect_error(
    attribute_agreement_by_spell_age(sa, pb, attributes = "state", target_year = 2024L),
    "negative|spell_age"
  )
})

test_that("S14: attribute_agreement_by_spell_age errors when target_year is not present in panel_b", {
  pb <- data.frame(npi = "1", year = 2024L, state = "CO", spell_age_years = 1L)
  sa <- data.frame(npi = "1", state = "CO")
  expect_error(
    attribute_agreement_by_spell_age(sa, pb, attributes = "state", target_year = 2099L),
    "target_year|no matching|overlap"
  )
})

test_that("S15: attribute_agreement_by_spell_age handles custom age_col with all NAs cleanly by failing closed", {
  pb <- data.frame(npi = "1", year = 2024L, state = "CO", custom_age = NA_integer_)
  sa <- data.frame(npi = "1", state = "CO")
  expect_error(
    attribute_agreement_by_spell_age(sa, pb, attributes = "state", target_year = 2024L, age_col = "custom_age"),
    "no non-missing|empty"
  )
})

test_that("S16: attribute_agreement_by_spell_age works when an attribute column name is 'id' or 'stratum' or collides with internal names", {
  pb <- data.frame(npi = c("1", "2"), year = c(2024L, 2024L), state = c("CO", "NY"), spell_age_years = c(0L, 12L))
  sa <- data.frame(npi = c("1", "2"), state = c("CO", "CA"))
  out <- attribute_agreement_by_spell_age(sa, pb, attributes = "state", target_year = 2024L)
  expect_equal(nrow(out), 2L)
  expect_equal(out$state_agree_pct, c(100, 0))
})

# --- S17-S21: enrollment_move_lead_lag() saboteurs ---------------------------

test_that("S17: enrollment_move_lead_lag fails closed on duplicate (id, year) in panel", {
  p_dup <- data.frame(
    npi = c("1", "1", "1"),
    year = c(2023, 2023, 2024),
    state = c("CO", "TX", "NM")
  )
  iv <- data.frame(
    npi = "1", state = "NM",
    interval_start = as.Date("2023-01-01"), interval_end = as.Date("2099-12-31")
  )
  expect_error(enrollment_move_lead_lag(p_dup, iv), "duplicate|unique")
})

test_that("S18: enrollment_move_lead_lag rejects inverted intervals (interval_end < interval_start)", {
  p <- data.frame(npi = c("1", "1"), year = c(2023, 2024), state = c("CO", "NM"))
  iv_bad <- data.frame(
    npi = "1", state = "NM",
    interval_start = as.Date("2024-06-01"),
    interval_end = as.Date("2023-01-01") # end before start!
  )
  expect_error(enrollment_move_lead_lag(p, iv_bad), "interval_end|before|inverted")
})

test_that("S19: enrollment_move_lead_lag rejects intervals with NA interval_start", {
  p <- data.frame(npi = c("1", "1"), year = c(2023, 2024), state = c("CO", "NM"))
  iv_na <- data.frame(
    npi = "1", state = "NM",
    interval_start = as.Date(NA),
    interval_end = as.Date("2099-12-31")
  )
  expect_error(enrollment_move_lead_lag(p, iv_na), "interval_start|NA|missing")
})

test_that("S20: enrollment_move_lead_lag does not treat a gap-year transition (2021 -> 2024) as a consecutive-year move", {
  p_gap <- data.frame(
    npi = c("1", "1", "2", "2"),
    year = c(2021, 2024, 2023, 2024),
    state = c("CO", "NM", "TX", "CA")
  )
  iv <- data.frame(
    npi = c("1", "2"), state = c("NM", "CA"),
    interval_start = as.Date(c("2023-01-01", "2023-01-01")),
    interval_end = as.Date(c("2099-12-31", "2099-12-31"))
  )
  out <- enrollment_move_lead_lag(p_gap, iv)
  # Only NPI 2 has a consecutive-year move (2023->2024); NPI 1's 2021->2024 gap must be ignored!
  expect_equal(unique(out$total_panel_moves), 1L)
  expect_equal(unique(out$total_matched_moves), 1L)
})

test_that("S21: enrollment_move_lead_lag handles zero matching intervals without dividing by zero or returning NaN", {
  p <- data.frame(npi = c("1", "1"), year = c(2023, 2024), state = c("CO", "NM"))
  iv_unmatched <- data.frame(
    npi = "999", state = "FL",
    interval_start = as.Date("2023-01-01"),
    interval_end = as.Date("2099-12-31")
  )
  out <- enrollment_move_lead_lag(p, iv_unmatched)
  expect_equal(nrow(out), 0L)
})

# --- S22-S25: format_no_gold_standard_prose() saboteurs ----------------------

test_that("S22: format_no_gold_standard_prose fails closed when required attribute rows (state/city/zip5/street) are missing from lead_lag_df", {
  # If lead_lag_df only has 'state', sprintf() would receive numeric(0) / character(0)
  # and silently return character(0) for the entire paragraph!
  ll_partial <- tibble::tibble(
    attribute = "state",
    n_paired_transitions = 100L, n_concordant_at_t = 90L,
    a_breaks_concordance_first = 5L, b_breaks_concordance_first = 1L,
    both_move_together_at_t1 = 2L, n_discordant_at_t = 10L,
    b_adopts_a_at_t1_n = 4L, b_adopts_a_at_t1_pct = 40,
    a_reverts_to_b_at_t1_n = 4L, a_reverts_to_b_at_t1_pct = 40,
    persistent_discordant_at_t1_pct = 20, base_year = 2022L,
    disc_base_n = 10L, b_adopts_a_multi_n = 6L, b_adopts_a_multi_pct = 60,
    a_reverts_b_multi_n = 3L, a_reverts_b_multi_pct = 30,
    cross_section_disc_n = 10L, cross_section_in_prior_b_n = 1L, cross_section_in_prior_b_pct = 10
  )
  fr_mock <- tibble::tibble(
    comparison = rep("DAC vs NPPES", 3),
    stratification = rep("spell_age_years", 3),
    stratum = c("0 yrs (updated in snapshot yr)", "1-2 yrs", "11+ yrs (un-updated >=11 yrs)"),
    n = c(10L, 20L, 30L),
    state_agree_pct = c(90, 92, 88), city_agree_pct = c(50, 60, 45),
    zip5_agree_pct = c(40, 55, 35), street_agree_pct = c(30, 44, 20)
  )
  enr_mock <- tibble::tibble(
    direction_type = c("destination_arrival_timing", "origin_departure_termination_timing"),
    category = c("Same calendar year (0 yr lag)", "Terminated in SAME calendar year as panel move"),
    n = c(10L, 10L), total_panel_moves = c(10L, 10L), total_matched_moves = c(10L, 10L), pct_of_matched = c(100, 100)
  )
  expect_error(
    format_no_gold_standard_prose(ll_partial, fr_mock, enr_mock),
    "missing required attribute|city|zip5|street"
  )
})

test_that("S23: format_no_gold_standard_prose fails closed when freshness_df is missing required age strata (0 yrs, 1-2 yrs, 11+ yrs)", {
  ll_full <- tibble::tibble(
    attribute = c("state", "city", "zip5", "street"),
    n_paired_transitions = 100L, n_concordant_at_t = 90L,
    a_breaks_concordance_first = 5L, b_breaks_concordance_first = 1L,
    both_move_together_at_t1 = 2L, n_discordant_at_t = 10L,
    b_adopts_a_at_t1_n = 4L, b_adopts_a_at_t1_pct = 40,
    a_reverts_to_b_at_t1_n = 4L, a_reverts_to_b_at_t1_pct = 40,
    persistent_discordant_at_t1_pct = 20, base_year = 2022L,
    disc_base_n = 10L, b_adopts_a_multi_n = 6L, b_adopts_a_multi_pct = 60,
    a_reverts_b_multi_n = 3L, a_reverts_b_multi_pct = 30,
    cross_section_disc_n = 10L, cross_section_in_prior_b_n = 1L, cross_section_in_prior_b_pct = 10
  )
  # Missing '11+ yrs' stratum -> d11 has 0 rows -> sprintf() silently produces character(0)!
  fr_incomplete <- tibble::tibble(
    comparison = rep("DAC vs NPPES", 2),
    stratification = rep("spell_age_years", 2),
    stratum = c("0 yrs (updated in snapshot yr)", "1-2 yrs"),
    n = c(10L, 20L),
    state_agree_pct = c(90, 92), city_agree_pct = c(50, 60),
    zip5_agree_pct = c(40, 55), street_agree_pct = c(30, 44)
  )
  enr_mock <- tibble::tibble(
    direction_type = c("destination_arrival_timing", "origin_departure_termination_timing"),
    category = c("Same calendar year (0 yr lag)", "Terminated in SAME calendar year as panel move"),
    n = c(10L, 10L), total_panel_moves = c(10L, 10L), total_matched_moves = c(10L, 10L), pct_of_matched = c(100, 100)
  )
  expect_error(
    format_no_gold_standard_prose(ll_full, fr_incomplete, enr_mock),
    "missing required freshness stratum|11\\+ yrs"
  )
})

test_that("S24: format_no_gold_standard_prose fails closed when enrollment_df is missing arrival or departure blocks", {
  ll_full <- tibble::tibble(
    attribute = c("state", "city", "zip5", "street"),
    n_paired_transitions = 100L, n_concordant_at_t = 90L,
    a_breaks_concordance_first = 5L, b_breaks_concordance_first = 1L,
    both_move_together_at_t1 = 2L, n_discordant_at_t = 10L,
    b_adopts_a_at_t1_n = 4L, b_adopts_a_at_t1_pct = 40,
    a_reverts_to_b_at_t1_n = 4L, a_reverts_to_b_at_t1_pct = 40,
    persistent_discordant_at_t1_pct = 20, base_year = 2022L,
    disc_base_n = 10L, b_adopts_a_multi_n = 6L, b_adopts_a_multi_pct = 60,
    a_reverts_b_multi_n = 3L, a_reverts_b_multi_pct = 30,
    cross_section_disc_n = 10L, cross_section_in_prior_b_n = 1L, cross_section_in_prior_b_pct = 10
  )
  fr_full <- tibble::tibble(
    comparison = rep("DAC vs NPPES", 3),
    stratification = rep("spell_age_years", 3),
    stratum = c("0 yrs (updated in snapshot yr)", "1-2 yrs", "11+ yrs (un-updated >=11 yrs)"),
    n = c(10L, 20L, 30L),
    state_agree_pct = c(90, 92, 88), city_agree_pct = c(50, 60, 45),
    zip5_agree_pct = c(40, 55, 35), street_agree_pct = c(30, 44, 20)
  )
  enr_dest_only <- tibble::tibble(
    direction_type = "destination_arrival_timing",
    category = "Same calendar year (0 yr lag)",
    n = 10L, total_panel_moves = 10L, total_matched_moves = 10L, pct_of_matched = 100
  )
  expect_error(
    format_no_gold_standard_prose(ll_full, fr_full, enr_dest_only),
    "origin_departure_termination_timing|missing"
  )
})

test_that("S25: format_no_gold_standard_prose never emits 'NA', 'NaN', 'Inf', or empty strings in any paragraph", {
  panel <- data.frame(
    npi = c("1", "1", "2", "2"),
    year = c(2023, 2024, 2023, 2024),
    state = c("CO", "NM", "TX", "CA")
  )
  intervals <- data.frame(
    npi = c("1", "1", "2", "2"),
    state = c("NM", "CO", "CA", "TX"),
    interval_start = as.Date(c("2023-05-01", "2020-01-01", "2024-03-01", "2019-01-01")),
    interval_end = as.Date(c("2099-12-31", "2099-12-31", "2099-12-31", "2024-06-01"))
  )
  enr <- enrollment_move_lead_lag(panel, intervals)
  ll_full <- tibble::tibble(
    attribute = c("state", "city", "zip5", "street"),
    n_paired_transitions = 100L, n_concordant_at_t = 90L,
    a_breaks_concordance_first = 5L, b_breaks_concordance_first = 0L, # zero denominator test!
    both_move_together_at_t1 = 2L, n_discordant_at_t = 10L,
    b_adopts_a_at_t1_n = 4L, b_adopts_a_at_t1_pct = 40,
    a_reverts_to_b_at_t1_n = 4L, a_reverts_to_b_at_t1_pct = 40,
    persistent_discordant_at_t1_pct = 20, base_year = 2022L,
    disc_base_n = 10L, b_adopts_a_multi_n = 6L, b_adopts_a_multi_pct = 60,
    a_reverts_b_multi_n = 3L, a_reverts_b_multi_pct = 30,
    cross_section_disc_n = 10L, cross_section_in_prior_b_n = 1L, cross_section_in_prior_b_pct = 10
  )
  fr_full <- tibble::tibble(
    comparison = rep("DAC vs NPPES", 3),
    stratification = rep("spell_age_years", 3),
    stratum = c("0 yrs (updated in snapshot yr)", "1-2 yrs", "11+ yrs (un-updated >=11 yrs)"),
    n = c(10L, 20L, 30L),
    state_agree_pct = c(90, 92, 88), city_agree_pct = c(50, 60, 45),
    zip5_agree_pct = c(40, 55, 35), street_agree_pct = c(30, 44, 20)
  )
  prose <- format_no_gold_standard_prose(ll_full, fr_full, enr)
  for (nm in names(prose)) {
    expect_equal(length(prose[[nm]]), 1L, info = nm)
    expect_true(nzchar(prose[[nm]]), info = nm)
    expect_false(grepl("\\bNA\\b|\\bNaN\\b|\\bInf\\b", prose[[nm]]), info = nm)
  }
})

# =============================================================================
# SECOND WAVE: S26-S50 (25 More Deeper Semantic & Boundary Saboteurs)
# =============================================================================

# --- S26-S30: Deep attribute_spell_age() saboteurs ---------------------------

test_that("S26: attribute_spell_age breaks a spell when there is a missing intermediate year (calendar-year gap)", {
  # NPI 1 is at '10 MAIN' in 2020, missing from the panel in 2021-2023, and at '10 MAIN' in 2024.
  # Because 2021-2023 were NOT observed, 2024 is NOT a 4-year uninterrupted spell!
  gap_panel <- data.frame(
    npi = c("1", "1"),
    year = c(2020L, 2024L),
    street = c("10 MAIN", "10 MAIN")
  )
  out <- attribute_spell_age(gap_panel, attribute = "street")
  expect_equal(out$spell_start_year, c(2020L, 2024L))
  expect_equal(out$spell_age_years, c(0L, 0L))
})

test_that("S27: attribute_spell_age rejects fractional (non-whole-number) numeric years like 2023.5", {
  frac_panel <- data.frame(
    npi = c("1", "1"),
    year = c(2023.5, 2024.5),
    street = c("10 MAIN", "10 MAIN")
  )
  expect_error(attribute_spell_age(frac_panel, attribute = "street"), "integer|whole")
})

test_that("S28: attribute_spell_age works when custom id or year column is NOT named 'npi' or 'year'", {
  custom_p <- data.frame(
    provider_id = c("P1", "P1"),
    snap_yr = c(2023L, 2024L),
    addr = c("10 MAIN", "10 MAIN")
  )
  out <- attribute_spell_age(custom_p, attribute = "addr", id = "provider_id", year = "snap_yr")
  expect_equal(out$spell_age_years, c(0L, 1L))
  expect_true(all(c("provider_id", "snap_yr", "addr") %in% names(out)))
})

test_that("S29: attribute_spell_age handles literal string 'NA' or 'NULL' vs actual NA_character_ consistently", {
  # What if an upstream CSV had unquoted NA vs whitespace vs valid string?
  p <- data.frame(
    npi = c("1", "1", "1"),
    year = c(2022L, 2023L, 2024L),
    street = c("10 MAIN", NA_character_, "10 MAIN")
  )
  out <- attribute_spell_age(p, attribute = "street")
  expect_equal(out$spell_age_years, c(0L, NA_integer_, 0L))
  expect_equal(out$spell_start_year, c(2022L, NA_integer_, 2024L))
})

test_that("S30: attribute_spell_age does not silently overwrite existing columns without recalculating cleanly", {
  # Calling attribute_spell_age() twice in a pipeline should be idempotent!
  p <- data.frame(npi = c("1", "1"), year = c(2023L, 2024L), street = c("10 MAIN", "10 MAIN"))
  out1 <- attribute_spell_age(p, attribute = "street")
  out2 <- attribute_spell_age(out1, attribute = "street")
  expect_identical(out1, out2)
})

# --- S31-S37: Deep attribute_lead_lag_matrix() saboteurs ---------------------

test_that("S31: attribute_lead_lag_matrix works when attribute is named 'id' or 'year'? No, rejects reserved column names in attributes", {
  sa <- data.frame(npi = c("1", "1"), year = c(2022L, 2023L), state = c("CO", "CO"))
  sb <- data.frame(npi = c("1", "1"), year = c(2022L, 2023L), state = c("CO", "CO"))
  expect_error(
    attribute_lead_lag_matrix(sa, sb, attributes = "year"),
    "cannot be the same as id or year|reserved"
  )
})

test_that("S32: attribute_lead_lag_matrix rejects duplicate attribute names in `attributes` argument", {
  # Passing attributes = c("state", "state") would create duplicate columns in clean_df / merge!
  sa <- data.frame(npi = c("1", "1"), year = c(2022L, 2023L), state = c("CO", "CO"))
  sb <- data.frame(npi = c("1", "1"), year = c(2022L, 2023L), state = c("CO", "CO"))
  expect_error(
    attribute_lead_lag_matrix(sa, sb, attributes = c("state", "state")),
    "unique|duplicate"
  )
})

test_that("S33: attribute_lead_lag_matrix does NOT count a mutual swap (A_t=NY, B_t=NJ -> A_{t+1}=NJ, B_{t+1}=NY) as both adoption AND reversion when they are still discordant at t+1", {
  # Wait: if A_t = NY and B_t = NJ, and at t+1 A_{t+1} = NJ while B_{t+1} = NY,
  # then at t+1 they STILL DISAGREE (NJ != NY)!
  # Yet `b_next == a_t` is TRUE AND `a_next == b_t` is TRUE, so the same NPI-transition
  # is counted in BOTH `b_adopts_a_at_t1_n` AND `a_reverts_to_b_at_t1_n`, making
  # `b_adopts_a_at_t1_pct + a_reverts_to_b_at_t1_pct == 200%`!
  # Categories of 1-year resolution of a discordance must be mutually exclusive (sum <= 100%)!
  sa <- data.frame(npi = c("1", "1"), year = c(2022L, 2023L), state = c("NY", "NJ"))
  sb <- data.frame(npi = c("1", "1"), year = c(2022L, 2023L), state = c("NJ", "NY"))
  ll <- attribute_lead_lag_matrix(sa, sb, attributes = "state")
  expect_lte(
    ll$b_adopts_a_at_t1_pct + ll$a_reverts_to_b_at_t1_pct + ll$persistent_discordant_at_t1_pct,
    100.0
  )
})

test_that("S34: attribute_lead_lag_matrix handles cross_section NPIs that have zero historical rows in source_b (< cross_section_year) without dropping them from cross_section_in_prior_b_pct denominator", {
  # In attribute_lead_lag_matrix():
  # `m_cs <- merge(d_cs[, c("id", cs_col)], b_hist, by = "id", all.x = TRUE)`
  # `by_id_cs <- tapply(m_cs$hist_b == m_cs[[cs_col]], m_cs$id, function(x) any(x, na.rm = TRUE))`
  # looks safe, right? Wait: what if `b_hist` has 0 rows (because NO discordant NPI in cross_section_year
  # ever appeared in source_b before cross_section_year)?
  # Let's check what happens when `b_hist` has 0 rows!
  sa <- data.frame(npi = c("1", "1"), year = c(2023L, 2024L), state = c("CO", "CO"))
  sb <- data.frame(npi = c("1", "1", "2"), year = c(2023L, 2024L, 2024L), state = c("CO", "CO", "NY"))
  cs <- data.frame(npi = c("1", "2"), state = c("CO", "CA")) # NPI 2 is discordant in 2024, and was NEVER in sb before 2024!
  ll <- attribute_lead_lag_matrix(sa, sb, attributes = "state", cross_section = cs, cross_section_year = 2024L)
  expect_equal(ll$cross_section_disc_n, 1L)
  expect_equal(ll$cross_section_in_prior_b_n, 0L)
  expect_equal(ll$cross_section_in_prior_b_pct, 0.0)
})

test_that("S35: attribute_lead_lag_matrix errors when source_a and source_b share zero consecutive-year transitions", {
  # Suppose source_a only has 2022 and 2024 (no consecutive years!), so `paired` has 0 rows!
  sa <- data.frame(npi = c("1", "1"), year = c(2022L, 2024L), state = c("CO", "CO"))
  sb <- data.frame(npi = c("1", "1"), year = c(2022L, 2024L), state = c("CO", "CO"))
  expect_error(
    attribute_lead_lag_matrix(sa, sb, attributes = "state"),
    "zero paired consecutive-year transitions"
  )
})

test_that("S36: attribute_lead_lag_matrix errors when cross_section_year is not present in source_b", {
  sa <- data.frame(npi = c("1", "1"), year = c(2022L, 2023L), state = c("CO", "CO"))
  sb <- data.frame(npi = c("1", "1"), year = c(2022L, 2023L), state = c("CO", "CO"))
  cs <- data.frame(npi = "1", state = "CO")
  expect_error(
    attribute_lead_lag_matrix(sa, sb, attributes = "state", cross_section = cs, cross_section_year = 2099L),
    "cross_section_year|not present"
  )
})

test_that("S37: attribute_lead_lag_matrix uses a consistent denominator when an NPI discordant in base_year has follow-up in source_b but not source_a", {
  # Look at S9 earlier: if NPI 2 is discordant in 2022, present in source_b in 2023,
  # and absent from source_a in 2023, what should `a_reverts_b_multi_pct` be?
  # If we verify `a_reverts_b_multi_n <= disc_base_n`, what if NPI 3 is present in source_a in 2023
  # and ABSENT from source_b in 2023? Then `a_reverts_b_multi_n` could exceed `disc_base_n`!
  sa <- data.frame(
    npi = c("1", "1", "2", "2"),
    year = c(2022L, 2023L, 2022L, 2023L),
    state = c("NM", "CO", "TX", "CO")
  )
  sb <- data.frame(
    npi = c("1", "1", "2"), # NPI 2 is NOT in sb after 2022!
    year = c(2022L, 2023L, 2022L),
    state = c("CO", "CO", "CO")
  )
  ll <- attribute_lead_lag_matrix(sa, sb, attributes = "state", base_year = 2022L)
  # Wait: in our current code, `disc_base_n` was set to `length(by_id_b)` = 1 (only NPI 1),
  # while `a_reverts_b_multi_n` was `sum(by_id_a)` = 2 (both NPI 1 and NPI 2)!
  # So `a_reverts_b_multi_n` (2) > `disc_base_n` (1)! That's a real bug!
  expect_lte(ll$a_reverts_b_multi_n, ll$disc_base_n)
  expect_lte(ll$b_adopts_a_multi_n, ll$disc_base_n)
})

# --- S38-S42: Deep attribute_agreement_by_spell_age() saboteurs --------------

test_that("S38: attribute_agreement_by_spell_age rejects duplicate attribute names in `attributes`", {
  pb <- data.frame(npi = "1", year = 2024L, state = "CO", spell_age_years = 1L)
  sa <- data.frame(npi = "1", state = "CO")
  expect_error(
    attribute_agreement_by_spell_age(sa, pb, attributes = c("state", "state"), target_year = 2024L),
    "unique|duplicate"
  )
})

test_that("S39: attribute_agreement_by_spell_age fails closed when an attribute column name is 'stratum' or 'n' or 'comparison'", {
  # If an attribute is named 'n', `row[[paste0(at, "_agree_pct")]]` is fine,
  # but what if an attribute is named `stratum`? In `da[[paste0(at, "_a")]]`, it creates `stratum_a`,
  # wait: what if `age_col` is non-numeric strings like 'three years'?
  pb <- data.frame(npi = "1", year = 2024L, state = "CO", spell_age_years = "three years")
  sa <- data.frame(npi = "1", state = "CO")
  expect_error(
    attribute_agreement_by_spell_age(sa, pb, attributes = "state", target_year = 2024L),
    "no non-missing|numeric|integer"
  )
})

test_that("S40: attribute_agreement_by_spell_age handles custom `id` and `year` column names when `source_a` is longitudinal", {
  pb <- data.frame(
    prov_id = c("P1", "P1"),
    snap_yr = c(2023L, 2024L),
    state = c("CO", "CO"),
    spell_age_years = c(0L, 1L)
  )
  sa <- data.frame(
    prov_id = c("P1", "P1"),
    snap_yr = c(2023L, 2024L),
    state = c("CO", "NM")
  )
  out <- attribute_agreement_by_spell_age(
    sa, pb, attributes = "state", id = "prov_id", year = "snap_yr"
  )
  expect_equal(nrow(out), 2L)
  expect_equal(out$state_agree_pct, c(100, 0))
})

test_that("S41: attribute_agreement_by_spell_age rejects fractional spell_age_years like 1.5", {
  # Look at classify_age(): `yrs %in% 1:2` in R!
  # Wait: if `as.integer(1.5)` is called, `1.5` is silently truncated to `1L`,
  # OR if someone passes `2.5`, `as.integer(2.5)` becomes `2L` ("1-2 yrs") instead of erroring!
  pb <- data.frame(npi = "1", year = 2024L, state = "CO", spell_age_years = 2.5)
  sa <- data.frame(npi = "1", state = "CO")
  expect_error(
    attribute_agreement_by_spell_age(sa, pb, attributes = "state", target_year = 2024L),
    "integer|whole"
  )
})

test_that("S42: attribute_agreement_by_spell_age errors when source_a and panel_b have zero overlapping IDs", {
  pb <- data.frame(npi = "1", year = 2024L, state = "CO", spell_age_years = 1L)
  sa <- data.frame(npi = "999", state = "CO")
  expect_error(
    attribute_agreement_by_spell_age(sa, pb, attributes = "state", target_year = 2024L),
    "no non-missing matched rows|overlap"
  )
})

# --- S43-S46: Deep enrollment_move_lead_lag() saboteurs ----------------------

test_that("S43: enrollment_move_lead_lag ignores destination intervals that ended BEFORE the move window or long before the move", {
  # Wait: suppose a provider was enrolled in Medicaid in state NM from 2010-01-01 to 2011-12-31
  # (terminated 13 years ago!), then practiced in CO in 2023 and moved back to NM in 2024!
  # Does a terminated 2010-2011 enrollment count as "Enrollment led by 3+ yrs" for a 2024 move,
  # even though it ended 13 years before the 2024 move?!
  # An interval that ended BEFORE the origin year (e.g. interval_end < as.Date(sprintf("%d-01-01", year - 1L)))
  # was NOT active during the move at all!
  p <- data.frame(npi = c("1", "1"), year = c(2023L, 2024L), state = c("CO", "NM"))
  iv_ancient_terminated <- data.frame(
    npi = "1", state = "NM",
    interval_start = as.Date("2010-01-01"),
    interval_end = as.Date("2011-12-31") # terminated 12 years before 2023->2024 move!
  )
  out <- enrollment_move_lead_lag(p, iv_ancient_terminated)
  # Should NOT match a long-terminated destination interval that ended before year - 1!
  expect_equal(nrow(out), 0L)
})

test_that("S44: enrollment_move_lead_lag rejects unparseable date strings in intervals", {
  p <- data.frame(npi = c("1", "1"), year = c(2023L, 2024L), state = c("CO", "NM"))
  iv_garbage <- data.frame(
    npi = "1", state = "NM",
    interval_start = "not-a-date",
    interval_end = "2099-12-31"
  )
  expect_error(enrollment_move_lead_lag(p, iv_garbage), "date|interval_start|missing")
})

test_that("S45: enrollment_move_lead_lag works with custom id, year, and state column names", {
  p <- data.frame(prov = c("1", "1"), yr = c(2023L, 2024L), st = c("CO", "NM"))
  iv <- data.frame(
    prov = "1", st = "NM",
    interval_start = as.Date("2024-01-01"),
    interval_end = as.Date("2099-12-31")
  )
  out <- enrollment_move_lead_lag(p, iv, id = "prov", year = "yr", state = "st")
  expect_equal(nrow(out), 6L)
  expect_equal(out$n[out$category == "Same calendar year (0 yr lag)"], 1L)
})

test_that("S46: enrollment_move_lead_lag rejects missing/empty id or state in intervals", {
  p <- data.frame(npi = c("1", "1"), year = c(2023L, 2024L), state = c("CO", "NM"))
  iv_na <- data.frame(
    npi = c("1", ""), state = c("NM", "NM"),
    interval_start = as.Date(c("2024-01-01", "2024-01-01")),
    interval_end = as.Date(c("2099-12-31", "2099-12-31"))
  )
  expect_error(enrollment_move_lead_lag(p, iv_na), "missing or empty|id|state")
})

# --- S47-S50: Deep format_no_gold_standard_prose() saboteurs -----------------

test_that("S47: format_no_gold_standard_prose fails closed when numeric metrics in lead_lag_df or freshness_df are NA", {
  # Look at S25: what if `st$b_adopts_a_multi_pct` is `NA_real_` (e.g. 0 discordant rows in base_year)?
  # Then `sprintf("%.1f%%", NA_real_)` literally prints `"NA%"` into the manuscript prose!
  ll_na <- tibble::tibble(
    attribute = c("state", "city", "zip5", "street"),
    n_paired_transitions = 100L, n_concordant_at_t = 100L,
    a_breaks_concordance_first = 0L, b_breaks_concordance_first = 0L,
    both_move_together_at_t1 = 0L, n_discordant_at_t = 0L,
    b_adopts_a_at_t1_n = 0L, b_adopts_a_at_t1_pct = NA_real_,
    a_reverts_to_b_at_t1_n = 0L, a_reverts_to_b_at_t1_pct = NA_real_,
    persistent_discordant_at_t1_pct = NA_real_, base_year = 2022L,
    disc_base_n = 0L, b_adopts_a_multi_n = NA_integer_, b_adopts_a_multi_pct = NA_real_,
    a_reverts_b_multi_n = NA_integer_, a_reverts_b_multi_pct = NA_real_,
    cross_section_disc_n = 0L, cross_section_in_prior_b_n = NA_integer_, cross_section_in_prior_b_pct = NA_real_
  )
  fr_full <- tibble::tibble(
    comparison = rep("DAC vs NPPES", 3),
    stratification = rep("spell_age_years", 3),
    stratum = c("0 yrs (updated in snapshot yr)", "1-2 yrs", "11+ yrs (un-updated >=11 yrs)"),
    n = c(10L, 20L, 30L),
    state_agree_pct = c(90, 92, 88), city_agree_pct = c(50, 60, 45),
    zip5_agree_pct = c(40, 55, 35), street_agree_pct = c(30, 44, 20)
  )
  enr_full <- tibble::tibble(
    direction_type = c("destination_arrival_timing", "origin_departure_termination_timing"),
    category = c("Same calendar year (0 yr lag)", "Terminated in SAME calendar year as panel move"),
    n = c(10L, 10L), total_panel_moves = c(10L, 10L), total_matched_moves = c(10L, 10L), pct_of_matched = c(100, 100)
  )
  expect_error(
    format_no_gold_standard_prose(ll_na, fr_full, enr_full),
    "NA|missing metric"
  )
})

test_that("S48: format_no_gold_standard_prose fails closed when duplicate attribute rows exist in lead_lag_df", {
  ll_dup <- tibble::tibble(
    attribute = c("state", "state", "city", "zip5", "street"),
    n_paired_transitions = 100L, n_concordant_at_t = 90L,
    a_breaks_concordance_first = 5L, b_breaks_concordance_first = 1L,
    both_move_together_at_t1 = 2L, n_discordant_at_t = 10L,
    b_adopts_a_at_t1_n = 4L, b_adopts_a_at_t1_pct = 40,
    a_reverts_to_b_at_t1_n = 4L, a_reverts_to_b_at_t1_pct = 40,
    persistent_discordant_at_t1_pct = 20, base_year = 2022L,
    disc_base_n = 10L, b_adopts_a_multi_n = 6L, b_adopts_a_multi_pct = 60,
    a_reverts_b_multi_n = 3L, a_reverts_b_multi_pct = 30,
    cross_section_disc_n = 10L, cross_section_in_prior_b_n = 1L, cross_section_in_prior_b_pct = 10
  )
  fr_full <- tibble::tibble(
    comparison = rep("DAC vs NPPES", 3),
    stratification = rep("spell_age_years", 3),
    stratum = c("0 yrs (updated in snapshot yr)", "1-2 yrs", "11+ yrs (un-updated >=11 yrs)"),
    n = c(10L, 20L, 30L),
    state_agree_pct = c(90, 92, 88), city_agree_pct = c(50, 60, 45),
    zip5_agree_pct = c(40, 55, 35), street_agree_pct = c(30, 44, 20)
  )
  enr_full <- tibble::tibble(
    direction_type = c("destination_arrival_timing", "origin_departure_termination_timing"),
    category = c("Same calendar year (0 yr lag)", "Terminated in SAME calendar year as panel move"),
    n = c(10L, 10L), total_panel_moves = c(10L, 10L), total_matched_moves = c(10L, 10L), pct_of_matched = c(100, 100)
  )
  expect_error(
    format_no_gold_standard_prose(ll_dup, fr_full, enr_full),
    "duplicate"
  )
})

test_that("S49: format_no_gold_standard_prose fails closed when acute move window has 0 moves (all moves are 3+ yrs pre-existing)", {
  # Look at `n_acute <- pmax(1L, n_med_lead_12 + n_same_yr + n_nppes_lead)`:
  # If ALL matched moves are "Enrollment led by 3+ yrs", then `n_med_lead_12 + n_same_yr + n_nppes_lead == 0`,
  # yet `n_acute` was forced to `1L`, printing `$n=1$` in prose when there were actually 0 acute moves!
  ll_full <- tibble::tibble(
    attribute = c("state", "city", "zip5", "street"),
    n_paired_transitions = 100L, n_concordant_at_t = 90L,
    a_breaks_concordance_first = 5L, b_breaks_concordance_first = 1L,
    both_move_together_at_t1 = 2L, n_discordant_at_t = 10L,
    b_adopts_a_at_t1_n = 4L, b_adopts_a_at_t1_pct = 40,
    a_reverts_to_b_at_t1_n = 4L, a_reverts_to_b_at_t1_pct = 40,
    persistent_discordant_at_t1_pct = 20, base_year = 2022L,
    disc_base_n = 10L, b_adopts_a_multi_n = 6L, b_adopts_a_multi_pct = 60,
    a_reverts_b_multi_n = 3L, a_reverts_b_multi_pct = 30,
    cross_section_disc_n = 10L, cross_section_in_prior_b_n = 1L, cross_section_in_prior_b_pct = 10
  )
  fr_full <- tibble::tibble(
    comparison = rep("DAC vs NPPES", 3),
    stratification = rep("spell_age_years", 3),
    stratum = c("0 yrs (updated in snapshot yr)", "1-2 yrs", "11+ yrs (un-updated >=11 yrs)"),
    n = c(10L, 20L, 30L),
    state_agree_pct = c(90, 92, 88), city_agree_pct = c(50, 60, 45),
    zip5_agree_pct = c(40, 55, 35), street_agree_pct = c(30, 44, 20)
  )
  enr_zero_acute <- tibble::tibble(
    direction_type = c("destination_arrival_timing", "origin_departure_termination_timing"),
    category = c("Enrollment led by 3+ yrs (pre-existing / long panel lag)", "Terminated in SAME calendar year as panel move"),
    n = c(10L, 10L), total_panel_moves = c(10L, 10L), total_matched_moves = c(10L, 10L), pct_of_matched = c(100, 100)
  )
  expect_error(
    format_no_gold_standard_prose(ll_full, fr_full, enr_zero_acute),
    "zero acute|no moves in acute window"
  )
})

test_that("S50: format_no_gold_standard_prose rejects empty or NA label arguments", {
  ll_full <- tibble::tibble(
    attribute = c("state", "city", "zip5", "street"),
    n_paired_transitions = 100L, n_concordant_at_t = 90L,
    a_breaks_concordance_first = 5L, b_breaks_concordance_first = 1L,
    both_move_together_at_t1 = 2L, n_discordant_at_t = 10L,
    b_adopts_a_at_t1_n = 4L, b_adopts_a_at_t1_pct = 40,
    a_reverts_to_b_at_t1_n = 4L, a_reverts_to_b_at_t1_pct = 40,
    persistent_discordant_at_t1_pct = 20, base_year = 2022L,
    disc_base_n = 10L, b_adopts_a_multi_n = 6L, b_adopts_a_multi_pct = 60,
    a_reverts_b_multi_n = 3L, a_reverts_b_multi_pct = 30,
    cross_section_disc_n = 10L, cross_section_in_prior_b_n = 1L, cross_section_in_prior_b_pct = 10
  )
  fr_full <- tibble::tibble(
    comparison = rep("DAC vs NPPES", 3),
    stratification = rep("spell_age_years", 3),
    stratum = c("0 yrs (updated in snapshot yr)", "1-2 yrs", "11+ yrs (un-updated >=11 yrs)"),
    n = c(10L, 20L, 30L),
    state_agree_pct = c(90, 92, 88), city_agree_pct = c(50, 60, 45),
    zip5_agree_pct = c(40, 55, 35), street_agree_pct = c(30, 44, 20)
  )
  enr_full <- tibble::tibble(
    direction_type = c("destination_arrival_timing", "origin_departure_termination_timing"),
    category = c("Same calendar year (0 yr lag)", "Terminated in SAME calendar year as panel move"),
    n = c(10L, 10L), total_panel_moves = c(10L, 10L), total_matched_moves = c(10L, 10L), pct_of_matched = c(100, 100)
  )
  expect_error(
    format_no_gold_standard_prose(ll_full, fr_full, enr_full, panel_label = ""),
    "at least 1 characters|panel_label"
  )
})

