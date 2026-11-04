test_that("attribute_spell_age computes consecutive spell age per identifier", {
  p <- data.frame(
    npi = c("1", "1", "1", "1", "2", "2"),
    year = c(2021, 2022, 2023, 2024, 2023, 2024),
    street = c("10 MAIN", "10 MAIN", "20 OAK", "20 OAK", "5 ELM", "5 ELM")
  )
  out <- attribute_spell_age(p, attribute = "street")
  expect_equal(out$spell_age_years[out$npi == "1"], c(0L, 1L, 0L, 1L))
  expect_equal(out$spell_start_year[out$npi == "1"], c(2021L, 2021L, 2023L, 2023L))
  expect_equal(out$spell_age_years[out$npi == "2"], c(0L, 1L))
})

test_that("attribute_lead_lag_matrix resolves 1-yr and multi-yr directional convergence", {
  # NPI 1: concordant in 2022, Source A moves first in 2023, Source B adopts in 2024
  # NPI 2: discordant in 2022 (A=NY, B=NJ), Source B adopts NY in 2023
  # NPI 3: discordant in 2022 (A=TX, B=CO), Source A reverts to CO in 2023
  source_a <- data.frame(
    npi = c("1", "1", "1", "2", "2", "2", "3", "3", "3"),
    year = c(2022, 2023, 2024, 2022, 2023, 2024, 2022, 2023, 2024),
    state = c("CO", "NM", "NM", "NY", "NY", "NY", "TX", "CO", "CO"),
    city = c("DENVER", "SANTA FE", "SANTA FE", "NYC", "NYC", "NYC", "AUSTIN", "DENVER", "DENVER"),
    zip5 = c("80202", "87501", "87501", "10001", "10001", "10001", "78701", "80202", "80202"),
    street = c("1 A", "2 B", "2 B", "3 C", "3 C", "3 C", "4 D", "1 A", "1 A")
  )
  source_b <- data.frame(
    npi = c("1", "1", "1", "2", "2", "2", "3", "3", "3"),
    year = c(2022, 2023, 2024, 2022, 2023, 2024, 2022, 2023, 2024),
    state = c("CO", "CO", "NM", "NJ", "NY", "NY", "CO", "CO", "CO"),
    city = c("DENVER", "DENVER", "SANTA FE", "NEWARK", "NYC", "NYC", "DENVER", "DENVER", "DENVER"),
    zip5 = c("80202", "80202", "87501", "07102", "10001", "10001", "80202", "80202", "80202"),
    street = c("1 A", "1 A", "2 B", "9 Z", "3 C", "3 C", "1 A", "1 A", "1 A")
  )
  cross_sec <- data.frame(
    npi = c("1", "2", "3"),
    state = c("NM", "PA", "CO"),
    city = c("SANTA FE", "PHILA", "DENVER"),
    zip5 = c("87501", "19104", "80202"),
    street = c("2 B", "8 Y", "1 A")
  )

  ll <- attribute_lead_lag_matrix(
    source_a, source_b,
    attributes = c("state", "city", "zip5", "street"),
    base_year = 2022L,
    cross_section = cross_sec,
    cross_section_year = 2024L
  )
  expect_equal(nrow(ll), 4)
  st <- ll[ll$attribute == "state", ]
  expect_equal(st$a_breaks_concordance_first, 1L)
  expect_equal(st$b_breaks_concordance_first, 0L)
  expect_equal(st$b_adopts_a_multi_n, 1L)
  expect_equal(st$a_reverts_b_multi_n, 1L)
  expect_equal(st$cross_section_disc_n, 1L)
  expect_equal(st$cross_section_in_prior_b_n, 0L)
})

test_that("attribute_agreement_by_spell_age stratifies agreement by spell age", {
  panel_b <- data.frame(
    npi = c("1", "1", "2", "2", "2"),
    year = c(2023, 2024, 2022, 2023, 2024),
    state = c("CO", "CO", "NY", "NY", "NY"),
    city = c("DENVER", "DENVER", "NYC", "NYC", "NYC"),
    zip5 = c("80202", "80202", "10001", "10001", "10001"),
    street = c("1 A", "2 B", "2 B", "2 B", "2 B")
  )
  panel_b <- attribute_spell_age(panel_b, attribute = "street")
  source_a <- data.frame(
    npi = c("1", "2"),
    state = c("CO", "NY"),
    city = c("DENVER", "ALBANY"),
    zip5 = c("80202", "12207"),
    street = c("2 B", "9 Z")
  )
  fr <- attribute_agreement_by_spell_age(
    source_a, panel_b,
    attributes = c("state", "city", "zip5", "street"),
    target_year = 2024L,
    comparison_label = "DAC (2026) vs NPPES (2026)",
    stratification_label = "nppes_street_spell_age"
  )
  expect_equal(nrow(fr), 2)
  expect_equal(fr$street_agree_pct[grepl("^0 yrs", fr$stratum)], 100)
  expect_equal(fr$street_agree_pct[fr$stratum == "1-2 yrs"], 0)
})

test_that("enrollment_move_lead_lag and format_no_gold_standard_prose generate 3 paragraphs", {
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
  expect_true(nrow(enr) > 0)

  # Verify format_no_gold_standard_prose produces all 3 paragraphs cleanly
  ll_mock <- tibble::tibble(
    attribute = c("state", "city", "zip5", "street"),
    n_paired_transitions = 9047L,
    n_concordant_at_t = c(8726L, 7804L, 7513L, 7234L),
    a_breaks_concordance_first = c(245L, 831L, 1007L, 1124L),
    b_breaks_concordance_first = c(15L, 34L, 32L, 32L),
    both_move_together_at_t1 = c(85L, 182L, 217L, 235L),
    n_discordant_at_t = c(321L, 1243L, 1534L, 1813L),
    b_adopts_a_at_t1_n = c(126L, 325L, 376L, 409L),
    b_adopts_a_at_t1_pct = c(39.3, 26.1, 24.5, 22.6),
    a_reverts_to_b_at_t1_n = c(155L, 599L, 725L, 851L),
    a_reverts_to_b_at_t1_pct = c(48.3, 48.2, 47.3, 46.9),
    persistent_discordant_at_t1_pct = c(12.5, 13.7, 13.0, 12.0),
    base_year = 2022L,
    disc_base_n = c(152L, 511L, 600L, 681L),
    b_adopts_a_multi_n = c(96L, 232L, 263L, 271L),
    b_adopts_a_multi_pct = c(63.2, 45.4, 43.8, 39.8),
    a_reverts_b_multi_n = c(52L, 181L, 205L, 225L),
    a_reverts_b_multi_pct = c(45.6, 47.1, 45.5, 43.4),
    cross_section_disc_n = c(567L, 2476L, 2952L, 3708L),
    cross_section_in_prior_b_n = c(64L, 190L, 173L, 121L),
    cross_section_in_prior_b_pct = c(11.3, 7.7, 5.9, 3.3)
  )
  fr_mock <- tibble::tibble(
    comparison = rep("DAC (2026) vs NPPES (2026)", 3),
    stratification = rep("nppes_street_spell_age", 3),
    stratum = c("0 yrs (updated in snapshot yr)", "1-2 yrs", "11+ yrs (un-updated >=11 yrs)"),
    n = c(700L, 1529L, 1011L),
    state_agree_pct = c(87.6, 91.8, 89.1),
    city_agree_pct = c(51.9, 62.7, 47.7),
    zip5_agree_pct = c(43.4, 57.8, 36.3),
    street_agree_pct = c(34.0, 43.7, 20.4)
  )
  prose <- format_no_gold_standard_prose(ll_mock, fr_mock, enr)
  expect_named(prose, c("paragraph_1_lead_lag", "paragraph_2_freshness", "paragraph_3_enrollment", "combined_text"))
  expect_match(prose$paragraph_1_lead_lag, "genuine relocation lead")
  expect_match(prose$paragraph_2_freshness, "staleness decay curve")
  expect_match(prose$paragraph_3_enrollment, "administrative asymmetry")
})
