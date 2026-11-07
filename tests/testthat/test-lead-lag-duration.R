test_that("attribute_lead_lag_duration finds the first adoption year, not just whether it happens", {
  a <- data.frame(
    npi = c("1", "1", "1", "1"), year = 2020:2023, state = c("CO", "CO", "CO", "CO")
  )
  b <- data.frame(
    npi = c("1", "1", "1", "1"), year = 2020:2023, state = c("NM", "NM", "CO", "CO")
  )
  dur <- attribute_lead_lag_duration(a, b, attribute = "state", base_year = 2020)
  expect_equal(dur$disc_base_n, 1)
  expect_equal(dur$b_adopts$n, 1)
  expect_equal(dur$b_adopts$years, 2L) # adopts in 2022, base_year 2020 -> 2 years
  expect_equal(dur$b_adopts$median, 2)
  expect_equal(dur$a_reverts$n, 0) # source_a never becomes NM
})

test_that("attribute_lead_lag_duration only counts the EARLIEST adoption, not a later re-occurrence", {
  a <- data.frame(npi = "1", year = 2020:2024, state = "CO")
  # b: NM, NM, CO (adopts at +2), NM (reverts away), CO (adopts again at +4) -- first adoption still +2
  b <- data.frame(npi = "1", year = 2020:2024, state = c("NM", "NM", "CO", "NM", "CO"))
  dur <- attribute_lead_lag_duration(a, b, attribute = "state", base_year = 2020)
  expect_equal(dur$b_adopts$years, 2L)
})

test_that("attribute_lead_lag_duration summarizes multiple identifiers with median/IQR/mean/CI", {
  ids <- as.character(1:6)
  a <- data.frame(npi = rep(ids, each = 5), year = rep(2020:2024, 6), state = "CO")
  b <- data.frame(npi = rep(ids, each = 5), year = rep(2020:2024, 6), state = "NM")
  lag_years <- c(1, 1, 2, 2, 3, 3) # adoption at base_year + lag
  for (i in seq_along(ids)) {
    yrs <- 2020 + lag_years[i]
    b$state[b$npi == ids[i] & b$year >= yrs] <- "CO"
  }
  dur <- attribute_lead_lag_duration(a, b, attribute = "state", base_year = 2020,
                                      source_a_label = "A", source_b_label = "B")
  expect_equal(dur$disc_base_n, 6)
  expect_equal(dur$b_adopts$n, 6)
  expect_equal(sort(dur$b_adopts$years), c(1, 1, 2, 2, 3, 3))
  expect_equal(dur$b_adopts$median, 2)
  expect_equal(dur$b_adopts$mean, 2)
  expect_true(!is.na(dur$b_adopts$ci_low))
  expect_true(dur$b_adopts$ci_low < 2 && dur$b_adopts$ci_high > 2)
  expect_match(dur$b_adopts_sentence, "^Of 6 identifiers where A and B disagreed in 2020")
  expect_match(dur$b_adopts_sentence, "median of 2\\.0 years")
  expect_match(dur$b_adopts_sentence, "95% CI")
})

test_that("attribute_lead_lag_duration uses singular 'identifier' for n = 1", {
  a <- data.frame(npi = "1", year = 2020:2021, state = c("CO", "CO"))
  b <- data.frame(npi = "1", year = 2020:2021, state = c("NM", "NM"))
  dur <- attribute_lead_lag_duration(a, b, attribute = "state", base_year = 2020)
  expect_match(dur$b_adopts_sentence, "^Of 1 identifier where")
  expect_no_match(dur$b_adopts_sentence, "1 identifiers")
})

test_that("attribute_lead_lag_duration reports a clear sentence when nothing is discordant at base_year", {
  a <- data.frame(npi = c("1", "1"), year = 2020:2021, state = c("CO", "CO"))
  b <- data.frame(npi = c("1", "1"), year = 2020:2021, state = c("CO", "CO"))
  dur <- attribute_lead_lag_duration(a, b, attribute = "state", base_year = 2020)
  expect_equal(dur$disc_base_n, 0)
  expect_match(dur$b_adopts_sentence, "^No identifiers had both")
  expect_equal(dur$b_adopts_sentence, dur$a_reverts_sentence)
})

test_that("attribute_lead_lag_duration reports a clear sentence when a direction never resolves", {
  a <- data.frame(npi = "1", year = 2020:2023, state = c("CO", "CO", "CO", "CO"))
  b <- data.frame(npi = "1", year = 2020:2023, state = c("NM", "NM", "NM", "NM"))
  dur <- attribute_lead_lag_duration(a, b, attribute = "state", base_year = 2020,
                                      source_a_label = "A", source_b_label = "B")
  expect_match(dur$b_adopts_sentence, "B never adopted A's 2020 value in any later year")
  expect_match(dur$a_reverts_sentence, "A never reverted to B's 2020 value in any later year")
})

test_that("attribute_lead_lag_duration resolves base_year the same way attribute_lead_lag_matrix does", {
  a <- data.frame(npi = "1", year = 2021:2023, state = c("CO", "CO", "NM"))
  b <- data.frame(npi = "1", year = 2021:2023, state = c("NM", "CO", "NM"))
  dur <- attribute_lead_lag_duration(a, b, attribute = "state") # base_year = NULL
  expect_equal(dur$base_year, 2021L) # earliest shared year with a follow-up year
})

test_that("attribute_lead_lag_duration's result gets a class and a concise print method", {
  a <- data.frame(npi = "1", year = 2020:2023, state = c("CO", "CO", "CO", "CO"))
  b <- data.frame(npi = "1", year = 2020:2023, state = c("NM", "NM", "CO", "CO"))
  dur <- attribute_lead_lag_duration(a, b, attribute = "state", base_year = 2020)
  expect_s3_class(dur, "mysteryroster_lead_lag_duration")
  expect_equal(dur$b_adopts$n, 1) # the class doesn't change list access to the real data
  out <- capture.output(print(dur))
  expect_match(out[1], "^<mysteryroster_lead_lag_duration>$")
  expect_match(paste(out, collapse = " "), "base year: 2020 \\| 1 identifier discordant")
  expect_match(paste(out, collapse = " "), "median of 2.0 years")
  expect_invisible(print(dur))
})

test_that("attribute_lead_lag_duration validates inputs", {
  a <- data.frame(npi = "1", year = 2020, state = "CO")
  b <- data.frame(npi = "1", year = 2020, state = "NM")
  expect_error(attribute_lead_lag_duration(a, b, attribute = "missing_col"),
               "source_a has no column")
  expect_error(attribute_lead_lag_duration(a, b, attribute = c("state", "other")),
               "Assertion on 'attribute' failed") # must be a single string, not a vector
})
