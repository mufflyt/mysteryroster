# Two sources, one shared NPI-year where they agree, one where they disagree
# with known freshness dates on both sides, one where only one side has a date.
nppes <- data.frame(
  npi = c("1", "1", "2", "3"), year = c(2023, 2024, 2024, 2024),
  state = c("CO", "CO", "NM", "TX"),
  last_updated = c("2022-01-01", "2023-06-01", "2019-01-01", "2020-01-01")
)
board <- data.frame(
  npi = c("1", "1", "2", "3"), year = c(2023, 2024, 2024, 2024),
  state = c("CO", "CO", "CO", "TX"),
  last_updated = c("2021-01-01", "2024-01-01", NA, NA)
)

test_that("record_age_years/record_age_band match isochrones' cut points", {
  expect_equal(record_age_years("2024-01-01", as_of = "2025-01-01"), 1, tolerance = 0.01)
  expect_equal(as.character(record_age_band("2024-06-01", as_of = "2024-12-01")), "<1 yr")
  expect_equal(as.character(record_age_band("2020-01-01", as_of = "2024-01-01")), "3-5 yr")
  expect_equal(as.character(record_age_band("2010-01-01", as_of = "2024-01-01")), "5+ yr")
  expect_true(is.na(record_age_years("not-a-date")))
})

test_that("classify_record_staleness is symmetric and NA-safe", {
  expect_equal(classify_record_staleness("2020-01-01", "2024-01-01"), "a_predates_b")
  expect_equal(classify_record_staleness("2024-01-01", "2020-01-01"), "b_predates_a")
  expect_equal(classify_record_staleness("2020-01-01", "2020-01-01"), "same_date")
  expect_equal(classify_record_staleness(NA, "2020-01-01"), "no_temporal_basis")
})

test_that("attribute_agreement compares only identifier-years both sources cover", {
  ag <- attribute_agreement(list(nppes = nppes, board = board), attribute = "state",
                             observed_at = "last_updated")
  expect_equal(nrow(ag), 4)
  expect_equal(attr(ag, "sources"), c("nppes", "board"))

  row_2023 <- ag[ag$year == 2023, ]
  expect_true(row_2023$agree)

  row_npi2 <- ag[ag$id == "2", ]
  expect_false(row_npi2$agree)
  expect_equal(row_npi2$value_a, "NM")
  expect_equal(row_npi2$value_b, "CO")
  # board has no last_updated for npi 2 -> no temporal basis despite disagreeing
  expect_equal(row_npi2$staleness, "no_temporal_basis")

  row_npi3 <- ag[ag$id == "3", ]
  expect_true(row_npi3$agree)
})

test_that("attribute_agreement classifies which side is provably older when both have dates", {
  older_nppes <- data.frame(npi = "9", year = 2024, state = "CO", last_updated = "2015-01-01")
  newer_board <- data.frame(npi = "9", year = 2024, state = "NM", last_updated = "2024-01-01")
  ag <- attribute_agreement(list(nppes = older_nppes, board = newer_board), attribute = "state",
                             observed_at = "last_updated")
  expect_false(ag$agree)
  expect_equal(ag$staleness, "a_predates_b")
  expect_equal(as.character(ag$age_band_a), "5+ yr")
})

test_that("attribute_agreement works with no year and no observed_at columns at all", {
  a <- data.frame(npi = c("1", "2"), state = c("CO", "NM"))
  b <- data.frame(npi = c("1", "2"), state = c("CO", "CO"))
  ag <- attribute_agreement(list(a = a, b = b), attribute = "state", year = NULL)
  expect_equal(nrow(ag), 2)
  expect_true(all(ag$staleness == "no_temporal_basis"))
  expect_true(all(is.na(ag$age_years_a)))
})

test_that("attribute values are trimmed and upper-cased before comparison", {
  a <- data.frame(npi = "1", year = 2024, state = " co ")
  b <- data.frame(npi = "1", year = 2024, state = "CO")
  ag <- attribute_agreement(list(a = a, b = b), attribute = "state", year = "year")
  expect_true(ag$agree)
})

test_that("bad inputs fail closed", {
  expect_error(attribute_agreement(list(a = nppes), attribute = "state"), "length >= 2")
  expect_error(attribute_agreement(list(a = nppes, b = data.frame(npi = "1")), attribute = "state"),
               "no column 'state'")
  empty <- data.frame(npi = character(0), year = integer(0), state = character(0))
  expect_error(attribute_agreement(list(a = empty, b = empty), attribute = "state"),
               "no non-missing attribute values")
  disjoint_a <- data.frame(npi = "1", year = 2024, state = "CO")
  disjoint_b <- data.frame(npi = "2", year = 2024, state = "CO")
  expect_error(attribute_agreement(list(a = disjoint_a, b = disjoint_b), attribute = "state"),
               "no identifier-year is covered by more than one source")
})

test_that("attribute_agreement_summary aggregates coverage and agreement per pair and year", {
  ag <- attribute_agreement(list(nppes = nppes, board = board), attribute = "state",
                             observed_at = "last_updated")
  summ <- attribute_agreement_summary(ag)
  expect_equal(nrow(summ), 2)  # one row per year (2023, 2024)
  row_2024 <- summ[summ$year == 2024, ]
  expect_equal(row_2024$n_compared, 3)  # npi 1, 2, 3 all have 2024 rows in both sources
  expect_equal(row_2024$n_agree, 2)     # npi 1 and npi 3 agree; npi 2 disagrees
  expect_equal(row_2024$pct_agree, 2 / 3)
  expect_equal(row_2024$n_disagree_no_temporal_basis, 1)  # npi 2's disagreement has no board date

  expect_error(attribute_agreement_summary(data.frame(x = 1)), "missing column")
})
