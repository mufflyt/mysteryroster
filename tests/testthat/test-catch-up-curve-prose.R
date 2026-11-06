make_cu <- function() {
  a <- data.frame(npi = c("1", "1", "1", "2", "2", "2"), year = rep(2020:2022, 2),
                   state = c("CO", "CO", "NM", "TX", "TX", "TX"))
  b <- data.frame(npi = c("1", "1", "1", "2", "2", "2"), year = rep(2020:2022, 2),
                   state = c("CO", "NM", "NM", "TX", "GA", "GA"))
  c <- data.frame(npi = c("1", "1", "1", "2", "2", "2"), year = rep(2020:2022, 2),
                   state = c("CO", "CO", "CO", "TX", "TX", "TX")) # never shows either arrival
  attribute_catch_up_curve(list(a = a, b = b, c = c), attribute = "state")
}

test_that("attribute_catch_up_curve_prose ranks resolved sources and sentences unresolved ones separately", {
  cu <- make_cu()
  p <- attribute_catch_up_curve_prose(cu)
  expect_equal(p$n_events, 2L)
  expect_false(is.null(p$ranking))
  expect_match(p$ranking$sentence, "^Ranked by median years to catch up")
  expect_equal(nrow(p$unresolved), 1L)
  expect_equal(p$unresolved$source, "c")
  expect_match(p$sentence, "We identified 2 state-arrival events tracked across 3 sources\\.")
  expect_match(p$sentence, "c never reached 50% catch-up within the observed panel")
})

test_that("attribute_catch_up_curve_prose applies display_label consistently in both the ranking and unresolved sentences", {
  cu <- make_cu()
  p <- attribute_catch_up_curve_prose(cu, display_label = c(a = "Source A", b = "Source B", c = "Source C"))
  expect_match(p$sentence, "Source A")
  expect_match(p$sentence, "Source B")
  expect_match(p$sentence, "Source C never reached 50%")
})

test_that("attribute_catch_up_curve_prose reports a single resolved source as a plain sentence, not a ranking", {
  a <- data.frame(npi = "1", year = 2020:2021, state = c("CO", "NM"))
  b <- data.frame(npi = "1", year = 2020:2021, state = c("CO", "CO")) # never shows NM
  cu <- attribute_catch_up_curve(list(a = a, b = b), attribute = "state")
  p <- attribute_catch_up_curve_prose(cu)
  expect_null(p$ranking)
  expect_match(p$sentence, "^We identified 1 state-arrival event")
  expect_match(p$sentence, "reached 50% catch-up after a median of 0 years")
  expect_match(p$sentence, "never reached 50%")
})

test_that("attribute_catch_up_curve_prose clamps unresolved_horizon to a source's own maximum horizon", {
  a <- data.frame(npi = "1", year = 2020:2022, state = c("CO", "CO", "NM"))
  b <- data.frame(npi = "1", year = 2020:2022, state = c("CO", "CO", "CO")) # never shows NM
  cu <- attribute_catch_up_curve(list(a = a, b = b), attribute = "state")
  expect_equal(max(cu$curve$horizon), 2L)
  p <- attribute_catch_up_curve_prose(cu, unresolved_horizon = 10L)
  expect_equal(p$unresolved$horizon, 2L)
  expect_match(p$sentence, "2 years later")
})

test_that("attribute_catch_up_curve_prose uses singular 'year' when the clamped horizon is 1", {
  a <- data.frame(npi = "1", year = 2020:2021, state = c("CO", "NM"))
  b <- data.frame(npi = "1", year = 2020:2021, state = c("CO", "CO"))
  cu <- attribute_catch_up_curve(list(a = a, b = b), attribute = "state")
  p <- attribute_catch_up_curve_prose(cu, unresolved_horizon = 1L)
  expect_match(p$sentence, "1 year later(?!s)", perl = TRUE)
})

test_that("attribute_catch_up_curve_prose validates its inputs", {
  expect_error(attribute_catch_up_curve_prose(list(curve = data.frame())),
               "median_horizon")
  expect_error(attribute_catch_up_curve_prose(list(curve = data.frame(x = 1), median_horizon = data.frame(source = "a", median_horizon = 1))),
               "no column")
})
