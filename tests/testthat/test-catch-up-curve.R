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

test_that("attribute_catch_up_curve credits a source that shows the matching value after a multi-year gap in its own records", {
  # a detects the arrival (CO -> NM, consecutive years) so the pooled event starts in 2021.
  # b has no record at all in 2021-2023 and shows NM for the first time in 2024 -- a 4-year
  # gap from b's own prior (2020) record, nowhere close to "consecutive". A naive rule
  # requiring b's own transition to be consecutive would never credit this match at all,
  # even though b plainly does show the correct value by 2024.
  a <- data.frame(npi = "1", year = 2020:2021, state = c("CO", "NM"))
  b <- data.frame(npi = "1", year = c(2020, 2024), state = c("CO", "NM"))
  cu <- attribute_catch_up_curve(list(a = a, b = b), attribute = "state")
  cb <- cu$curve[cu$curve$source == "b", ]
  expect_equal(cb$pct_not_yet_shown, c(100, 100, 100, 0, 0))
  expect_equal(cb$n_events, c(0, 0, 0, 1, 0))
  expect_equal(cu$median_horizon$median_horizon[cu$median_horizon$source == "b"], 3L)
})

test_that("attribute_catch_up_curve does not drop an event from the risk set when a source's only matching record predates the event's start", {
  # b happens to show "NM" in 2019, two years before the pooled event (detected via a) even
  # starts in 2021 -- a coincidental earlier occurrence, unrelated to the tracked arrival.
  # b never shows NM again (its only other record, in 2021, is a different value, and the
  # 2019-2021 gap is not itself a consecutive-year transition, so b contributes no arrival
  # event of its own). This must not count as catching up, but the event must also not
  # silently vanish from b's risk set -- it stays at risk (not resolved) for its own
  # follow-up window.
  a <- data.frame(npi = "1", year = 2020:2021, state = c("CO", "NM"))
  b <- data.frame(npi = "1", year = c(2019, 2021), state = c("NM", "CO"))
  cu <- attribute_catch_up_curve(list(a = a, b = b), attribute = "state")
  cb <- cu$curve[cu$curve$source == "b", ]
  expect_equal(cb$pct_not_yet_shown, c(100, 100, 100))
  expect_equal(cb$n_at_risk, c(1, 0, 0)) # retained in the risk set at horizon 0, not dropped
  expect_true(is.na(cu$median_horizon$median_horizon[cu$median_horizon$source == "b"]))
})

test_that("attribute_catch_up_curve left-truncates a source's risk set until its own observation window begins", {
  # a detects the arrival (CO -> NM) in 2011, but b's own data does not begin until 2015 --
  # b cannot possibly be "at risk" of showing the value before its own data collection
  # started. b should not enter the risk set until horizon 4 (2015 - 2011), not horizon 0.
  a <- data.frame(npi = "1", year = 2010:2011, state = c("CO", "NM"))
  b <- data.frame(npi = "1", year = c(2015, 2016), state = c("CO", "NM"))
  cu <- attribute_catch_up_curve(list(a = a, b = b), attribute = "state")
  cb <- cu$curve[cu$curve$source == "b", ]
  expect_equal(cb$n_at_risk[cb$horizon %in% 0:3], c(0, 0, 0, 0)) # not yet entered b's window
  expect_equal(cb$n_at_risk[cb$horizon == 4], 1L) # enters at horizon 4 = 2015 - 2011
  expect_equal(cb$n_at_risk[cb$horizon == 6], 0L) # censored at horizon 6: b's own data ends 2016
  expect_equal(cu$median_horizon$median_horizon[cu$median_horizon$source == "b"], 5L)
})

test_that("attribute_catch_up_curve excludes an identifier a source never observes at all from that source's risk set", {
  # id 1: a detects CO -> NM; b has records for id 1 (shows CO throughout) but never NM --
  # ordinary "watched, unresolved". id 2: a detects TX -> GA; b has NO records for id 2 at
  # all, in any year -- structural non-coverage, not merely an unresolved watch.
  a <- data.frame(npi = c("1", "1", "1", "2", "2", "2"), year = rep(2020:2022, 2),
                   state = c("CO", "CO", "NM", "TX", "TX", "GA"))
  b <- data.frame(npi = c("1", "1", "1"), year = 2020:2022, state = c("CO", "CO", "CO"))
  cu <- attribute_catch_up_curve(list(a = a, b = b), attribute = "state")

  nc <- cu$n_no_coverage
  expect_equal(nc$n_no_coverage[nc$source == "a"], 0L) # a observes both identifiers
  expect_equal(nc$n_no_coverage[nc$source == "b"], 1L) # b never observes id 2 at all

  cb <- cu$curve[cu$curve$source == "b", ]
  expect_equal(cb$n_at_risk[cb$horizon == 0], 1L) # id 2's event dropped entirely, not just unresolved
  expect_true(is.na(cu$median_horizon$median_horizon[cu$median_horizon$source == "b"]))
})

test_that("attribute_catch_up_curve's reference = 'exclude_self' removes circularity and reports n_unanchored", {
  # id 1: only a ever shows NM -- this event has no independent confirmation at all.
  # id 2: both a (2022) and b (2021, earlier) detect the TX -> GA arrival independently.
  a <- data.frame(npi = c("1", "1", "1", "2", "2", "2"), year = rep(2020:2022, 2),
                   state = c("CO", "CO", "NM", "TX", "TX", "GA"))
  b <- data.frame(npi = c("1", "1", "1", "2", "2", "2"), year = rep(2020:2022, 2),
                   state = c("CO", "CO", "CO", "TX", "GA", "GA"))
  c <- data.frame(npi = c("1", "1", "1", "2", "2", "2"), year = rep(2020:2022, 2),
                   state = c("CO", "CO", "CO", "TX", "TX", "TX"))
  cu <- attribute_catch_up_curve(list(a = a, b = b, c = c), attribute = "state", reference = "exclude_self")

  expect_equal(cu$reference, "exclude_self")
  expect_equal(cu$n_tracked, 2L) # constant regardless of reference mode
  nu <- cu$n_unanchored
  expect_equal(nu$n_unanchored[nu$source == "a"], 1L) # id 1's NM arrival: only a ever detected it
  expect_equal(nu$n_unanchored[nu$source == "b"], 0L)
  expect_equal(nu$n_unanchored[nu$source == "c"], 0L)

  # a is scored against only 1 event (id 2's, anchored via b's 2021 detection) -- id 1's
  # event is dropped entirely for a, not merely left unresolved.
  ca <- cu$curve[cu$curve$source == "a", ]
  expect_equal(ca$n_at_risk[ca$horizon == 0], 1L)
  expect_equal(cu$median_horizon$median_horizon[cu$median_horizon$source == "a"], 1L)
})

test_that("attribute_catch_up_curve's reference = 'pooled' default matches prior behavior and reports n_tracked", {
  a <- data.frame(npi = c("1", "1", "1"), year = 2020:2022, state = c("CO", "CO", "NM"))
  b <- data.frame(npi = c("1", "1", "1"), year = 2020:2022, state = c("CO", "NM", "NM"))
  cu <- attribute_catch_up_curve(list(a = a, b = b), attribute = "state")
  expect_equal(cu$reference, "pooled")
  expect_equal(cu$n_tracked, 1L)
  expect_null(cu$n_unanchored)
})

test_that("attribute_catch_up_curve's lineage_groups excludes a source's whole evidence family under reference = 'exclude_self'", {
  # id 1's NM arrival is detected by a and b. id 2's GA arrival is detected by b and c only
  # -- a never detects it at all. b and c share underlying provenance (e.g. a
  # facility-affiliation extract derived from the same upstream file as an enrollment
  # source); grouping them means b can no longer use c (or vice versa) to "independently"
  # confirm an event, even though c's own source_id differs from b's.
  a <- data.frame(npi = c("1", "1", "1", "2", "2", "2"), year = rep(2020:2022, 2),
                   state = c("CO", "CO", "NM", "TX", "TX", "TX"))
  b <- data.frame(npi = c("1", "1", "1", "2", "2", "2"), year = rep(2020:2022, 2),
                   state = c("CO", "CO", "NM", "TX", "GA", "GA"))
  c <- data.frame(npi = c("1", "1", "1", "2", "2", "2"), year = rep(2020:2022, 2),
                   state = c("CO", "CO", "CO", "TX", "TX", "GA"))

  # Without lineage_groups: b can use c to confirm id 2's event -- fully anchored.
  cu_flat <- attribute_catch_up_curve(list(a = a, b = b, c = c), attribute = "state", reference = "exclude_self")
  nu_flat <- cu_flat$n_unanchored
  expect_equal(nu_flat$n_unanchored[nu_flat$source == "b"], 0L)

  # With b and c grouped as one evidence family: b can no longer use c (its own group-mate)
  # to confirm id 2's event, and a never detected it either -- now unanchored for b.
  cu_grouped <- attribute_catch_up_curve(
    list(a = a, b = b, c = c), attribute = "state", reference = "exclude_self",
    lineage_groups = list(bc_family = c("b", "c"))
  )
  nu_grouped <- cu_grouped$n_unanchored
  expect_equal(nu_grouped$n_unanchored[nu_grouped$source == "b"], 1L)
  expect_equal(nu_grouped$n_unanchored[nu_grouped$source == "c"], 1L)
  expect_equal(nu_grouped$n_unanchored[nu_grouped$source == "a"], 0L) # a is ungrouped, unaffected
})

test_that("attribute_catch_up_curve validates lineage_groups", {
  a <- data.frame(npi = "1", year = 2020:2021, state = c("CO", "NM"))
  b <- data.frame(npi = "1", year = 2020:2021, state = c("CO", "CO"))
  expect_error(
    attribute_catch_up_curve(list(a = a, b = b), attribute = "state", reference = "exclude_self",
                              lineage_groups = list(g1 = c("a", "b"), g2 = c("b"))),
    "more than one lineage group"
  )
  expect_error(
    attribute_catch_up_curve(list(a = a, b = b), attribute = "state", reference = "exclude_self",
                              lineage_groups = list(g1 = c("a", "nonexistent"))),
    "unknown source"
  )
})

test_that("attribute_catch_up_curve collapses a repeated CO -> TX -> CO -> TX episode into one event per value", {
  # The spec scenario: a clinician's own data makes two separate round trips through the
  # same two values. Only 2 distinct (id, value) events are tracked (TX, CO), not the 3
  # genuine transitions that actually occurred -- confirming, via an explicit synthetic
  # case, the documented "repeated arrival" collapse limitation.
  a <- data.frame(npi = "1", year = 2020:2023, state = c("CO", "TX", "CO", "TX"))
  b <- data.frame(npi = "1", year = 2020:2023, state = c("CO", "CO", "CO", "CO")) # padding 2nd source
  cu <- attribute_catch_up_curve(list(a = a, b = b), attribute = "state")
  expect_equal(cu$n_tracked, 2L) # TX and CO, not 3 genuine transitions
  ca <- cu$curve[cu$curve$source == "a", ]
  # a resolves both of its own tracked values immediately (it's the only source detecting
  # them), regardless of which of the two real TX episodes the pooled start collapsed to.
  expect_equal(ca$n_at_risk[ca$horizon == 0], 2L)
  expect_equal(ca$n_events[ca$horizon == 0], 2L)
})

test_that("attribute_catch_up_curve errors clearly when no source has any consecutive-year arrival", {
  a <- data.frame(npi = "1", year = c(2020, 2022), state = c("CO", "NM")) # a gap, not consecutive
  b <- data.frame(npi = "1", year = c(2020, 2022), state = c("CO", "CO"))
  expect_error(attribute_catch_up_curve(list(a = a, b = b), attribute = "state"),
               "no consecutive-year arrivals")
})
