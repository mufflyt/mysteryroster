test_that("attribute_catch_up_curve matches a hand-computed Kaplan-Meier curve", {
  # id "1": a arrives at NM 2 years after b does. id "2": no change in either source.
  a <- data.frame(npi = c("1", "1", "1", "1", "2", "2", "2", "2"), year = rep(2020:2023, 2),
                   state = c("CO", "CO", "CO", "NM", "TX", "TX", "TX", "TX"))
  b <- data.frame(npi = c("1", "1", "1", "1", "2", "2", "2", "2"), year = rep(2020:2023, 2),
                   state = c("CO", "NM", "NM", "NM", "TX", "TX", "TX", "TX"))
  cu <- attribute_catch_up_curve(list(a = a, b = b), attribute = "state")

  ca <- cu$curve[cu$curve$source == "a", ]
  expect_equal(ca$pct_not_yet_shown, c(100, 100, 0, 0))
  expect_equal(ca$n_events, c(0, 0, 1, 0))

  cb <- cu$curve[cu$curve$source == "b", ]
  expect_equal(cb$pct_not_yet_shown, c(0, 0, 0, 0)) # b establishes start itself -> shown at horizon 0

  expect_equal(cu$median_horizon$median_horizon[cu$median_horizon$source == "a"], 2L)
  expect_equal(cu$median_horizon$median_horizon[cu$median_horizon$source == "b"], 0L)
})

test_that("attribute_catch_up_curve censors an event once its own follow-up runs out, never treating it as a permanent failure", {
  # id 3's arrival at GA happens in the very last panel year for source a, and source b
  # never shows it at all -- that event has essentially zero follow-up time.
  a <- data.frame(npi = c("1", "1", "1", "3", "3", "3"), year = rep(2020:2022, 2),
                   state = c("CO", "CO", "NM", "TX", "TX", "GA"))
  b <- data.frame(npi = c("1", "1", "1", "3", "3", "3"), year = rep(2020:2022, 2),
                   state = c("CO", "NM", "NM", "TX", "TX", "TX"))
  cu <- attribute_catch_up_curve(list(a = a, b = b), attribute = "state")

  cb <- cu$curve[cu$curve$source == "b", ]
  # Without censoring, a naive estimator would keep dragging b's curve toward 0 because it
  # "never" shows id 3's GA. With correct censoring, that event drops out of the risk set at
  # horizon 1 (its own 0-year follow-up has already elapsed) and b's curve holds at 50%,
  # not 0%.
  expect_equal(cb$pct_not_yet_shown, c(50, 50, 50))
  expect_equal(cb$n_at_risk, c(2, 0, 0)) # the never-shown event correctly drops out, not counted as a failure
})

test_that("attribute_catch_up_curve's median_horizon is NA when a source's curve never crosses 50%", {
  # Source b never shows either of the two values source a arrives at, so it never
  # experiences either tracked event -- its survival curve should stay at 100% throughout
  # (never degrading, since an event that's never shown but still within its own follow-up
  # window just stays "at risk", contributing no failures), giving NA rather than a
  # fabricated median.
  a <- data.frame(npi = c("1", "1", "1", "2", "2", "2"), year = rep(2020:2022, 2),
                   state = c("CO", "NM", "NM", "TX", "GA", "GA"))
  b <- data.frame(npi = c("1", "1", "1", "2", "2", "2"), year = rep(2020:2022, 2),
                   state = c("CO", "CO", "CO", "TX", "TX", "TX")) # never shows either arrival
  cu <- attribute_catch_up_curve(list(a = a, b = b), attribute = "state")
  cb <- cu$curve[cu$curve$source == "b", ]
  expect_equal(cb$pct_not_yet_shown, c(100, 100, 100))
  med_b <- cu$median_horizon$median_horizon[cu$median_horizon$source == "b"]
  expect_true(is.na(med_b))
})

test_that("attribute_catch_up_curve does not treat a second return to an earlier value as a new event", {
  # id 1 in source a: CO -> NM -> CO. The second arrival at CO is NOT a new tracked event --
  # (id, "CO") already has a start from year 2020 (the very first observed value, which is
  # never itself an "arrival"). Confirmed: only the NM arrival is tracked as an event.
  a <- data.frame(npi = "1", year = 2020:2022, state = c("CO", "NM", "CO"))
  b <- data.frame(npi = "1", year = 2020:2022, state = c("CO", "CO", "CO"))
  cu <- attribute_catch_up_curve(list(a = a, b = b), attribute = "state")
  expect_equal(nrow(cu$curve[cu$curve$source == "a", ]), length(0:2)) # one event tracked (NM)
  # only a shows "NM" at all -- exactly one (id, value) pair in the pooled event set
})

test_that("attribute_catch_up_curve respects an explicit max_horizon", {
  a <- data.frame(npi = "1", year = 2020:2025, state = c("CO", "CO", "CO", "CO", "CO", "NM"))
  b <- data.frame(npi = "1", year = 2020:2025, state = rep("CO", 6))
  cu <- attribute_catch_up_curve(list(a = a, b = b), attribute = "state", max_horizon = 2L)
  expect_equal(unique(cu$curve$horizon), 0:2)
})

test_that("attribute_catch_up_curve's output composes with format_ranking_prose", {
  a <- data.frame(npi = c("1", "1", "1"), year = 2020:2022, state = c("CO", "CO", "NM"))
  b <- data.frame(npi = c("1", "1", "1"), year = 2020:2022, state = c("CO", "NM", "NM"))
  cu <- attribute_catch_up_curve(list(a = a, b = b), attribute = "state")
  r <- format_ranking_prose(
    group = cu$median_horizon$source, value = cu$median_horizon$median_horizon,
    value_label = "median years to catch up", fmt = "%.0f", decreasing = FALSE
  )
  expect_match(r$sentence, "^Ranked by median years to catch up, lowest to highest")
})

test_that("attribute_catch_up_curve validates inputs", {
  a <- data.frame(npi = "1", year = 2020, state = "CO")
  expect_error(attribute_catch_up_curve(list(a = a), attribute = "state"), "length >= 2")
  expect_error(attribute_catch_up_curve(list(a = a, b = a), attribute = "missing_col"), "no column")
  expect_error(
    attribute_catch_up_curve(list(a = a, b = a), attribute = "state", max_horizon = -1),
    "Assertion on 'max_horizon' failed"
  )
})

test_that("attribute_catch_up_curve errors clearly when no source has any consecutive-year arrival", {
  a <- data.frame(npi = "1", year = c(2020, 2022), state = c("CO", "NM")) # a gap, not consecutive
  b <- data.frame(npi = "1", year = c(2020, 2022), state = c("CO", "CO"))
  expect_error(attribute_catch_up_curve(list(a = a, b = b), attribute = "state"),
               "no consecutive-year arrivals")
})
