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

test_that("attribute_catch_up_curve_prose's register = 'clinical' avoids catch-up/arrival jargon and em dashes", {
  cu <- make_cu()
  p <- attribute_catch_up_curve_prose(
    cu, display_label = c(a = "the first registry", b = "the second registry", c = "the third registry"),
    register = "clinical"
  )
  expect_match(p$sentence, "^A total of 2 changes in the tracked value were identified across 3 sources\\.")
  expect_match(p$sentence, "Among the 2 groups compared, the lowest median years to confirmation was observed for")
  expect_match(p$sentence, "the third registry did not reach confirmation for even half of the changes tracked")
  expect_false(grepl("catch.up", p$sentence, ignore.case = TRUE))
  expect_false(grepl("arrival", p$sentence, ignore.case = TRUE))
  expect_false(grepl("—", p$sentence))
})

test_that("attribute_catch_up_curve_prose's register = 'clinical' reports a single resolved source in plain language", {
  a <- data.frame(npi = "1", year = 2020:2021, state = c("CO", "NM"))
  b <- data.frame(npi = "1", year = 2020:2021, state = c("CO", "CO"))
  cu <- attribute_catch_up_curve(list(a = a, b = b), attribute = "state")
  p <- attribute_catch_up_curve_prose(cu, register = "clinical")
  expect_match(p$sentence, "^A total of 1 change in the tracked value was identified across 2 sources\\.")
  expect_match(p$sentence, "The estimated time for a to confirm a change already reported by another source was 0 years")
  expect_match(p$sentence, "did not reach confirmation for even half of the changes tracked")
  expect_false(grepl("—", p$sentence))
})

test_that("attribute_catch_up_curve_prose defaults to register = 'technical'", {
  cu <- make_cu()
  p1 <- attribute_catch_up_curve_prose(cu)
  p2 <- attribute_catch_up_curve_prose(cu, register = "technical")
  expect_equal(p1$sentence, p2$sentence)
})

test_that("attribute_catch_up_curve_prose's n_events uses the true pooled total under reference = 'exclude_self', not the largest source's own reduced risk set", {
  # Three events, each detected by exactly one source: a alone detects id 1's NM arrival,
  # b alone detects id 2's GA arrival, c alone detects id 3's OH arrival. Under
  # exclude_self, every source's own event is unanchored for every OTHER source that
  # excludes it (each source can anchor only the two events it did NOT itself detect), so
  # every source's own horizon-0 n_at_risk is 2, strictly less than the true pooled total
  # of 3 -- max() across sources would silently understate the real count unless
  # cu$n_tracked is used directly.
  a <- data.frame(npi = c("1", "1", "1", "2", "2", "2", "3", "3", "3"), year = rep(2020:2022, 3),
                   state = c("CO", "CO", "NM", "TX", "TX", "TX", "PA", "PA", "PA"))
  b <- data.frame(npi = c("1", "1", "1", "2", "2", "2", "3", "3", "3"), year = rep(2020:2022, 3),
                   state = c("CO", "CO", "CO", "TX", "TX", "GA", "PA", "PA", "PA"))
  c <- data.frame(npi = c("1", "1", "1", "2", "2", "2", "3", "3", "3"), year = rep(2020:2022, 3),
                   state = c("CO", "CO", "CO", "TX", "TX", "TX", "PA", "PA", "OH"))
  cu <- attribute_catch_up_curve(list(a = a, b = b, c = c), attribute = "state", reference = "exclude_self")
  expect_equal(cu$n_tracked, 3L)
  expect_true(all(cu$curve$n_at_risk[cu$curve$horizon == 0] < cu$n_tracked)) # confirms the bug scenario is live
  p <- attribute_catch_up_curve_prose(cu)
  expect_equal(p$n_events, 3L)
})

test_that("attribute_catch_up_curve_prose reports each resolved source's confidence interval and at-risk count", {
  cu <- make_cu()
  p <- attribute_catch_up_curve_prose(cu)
  # a's median_ci_high is NA (never crosses within the observed horizons) -- grammar must
  # not trail a bare "years" after the "not estimable" text, and singular/plural must be
  # correct for both the bounded (b: 0 to 0 years) and open-ended (a: 1 year to not
  # estimable) cases.
  expect_match(p$sentence, "a: 95% CI 1 year to not estimable \\(n at risk at crossing = 2\\)\\.")
  expect_match(p$sentence, "b: 95% CI 0 to 0 years \\(n at risk at crossing = 2\\)\\.")
  expect_false(grepl("not estimable within the period observed years", p$sentence, fixed = TRUE))
  expect_false(grepl("not estimable years", p$sentence, fixed = TRUE)) # no dangling "years" right after "not estimable"
})

test_that("attribute_catch_up_curve_prose's register = 'clinical' reports the confidence interval in plain language", {
  cu <- make_cu()
  p <- attribute_catch_up_curve_prose(cu, register = "clinical")
  expect_match(p$sentence, "This estimate for a was based on 2 events still being tracked at the time the median was reached, with a 95% confidence interval of 1 year, with no upper bound estimable within the period observed\\.")
  expect_match(p$sentence, "This estimate for b was based on 2 events still being tracked at the time the median was reached, with a 95% confidence interval of 0 to 0 years\\.")
  expect_false(grepl("—", p$sentence))
})

test_that("attribute_catch_up_curve_prose's ci_level only changes the displayed label, not the computation", {
  cu <- make_cu()
  p90 <- attribute_catch_up_curve_prose(cu, ci_level = 0.90)
  expect_match(p90$sentence, "90% CI")
  expect_false(grepl("95% CI", p90$sentence))
})

test_that("attribute_catch_up_curve_prose skips confidence-interval reporting gracefully when cu lacks the CI columns", {
  cu_old_shape <- list(
    curve = data.frame(source = c("a", "a"), horizon = c(0L, 1L), n_at_risk = c(2L, 2L),
                       n_events = c(0L, 2L), pct_not_yet_shown = c(100, 0)),
    median_horizon = data.frame(source = "a", median_horizon = 1L)
  )
  p <- attribute_catch_up_curve_prose(cu_old_shape)
  expect_false(grepl("CI", p$sentence))
})

test_that("attribute_catch_up_curve_prose validates ci_level", {
  cu <- make_cu()
  expect_error(attribute_catch_up_curve_prose(cu, ci_level = 1.5), "Assertion on 'ci_level' failed")
})

test_that("attribute_catch_up_curve_prose validates its inputs", {
  expect_error(attribute_catch_up_curve_prose(list(curve = data.frame())),
               "median_horizon")
  expect_error(attribute_catch_up_curve_prose(list(curve = data.frame(x = 1), median_horizon = data.frame(source = "a", median_horizon = 1))),
               "no column")
})
