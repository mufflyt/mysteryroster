test_that("attribute_consensus_tier classifies unanimous, majority, and split correctly", {
  s <- list(
    nppes = data.frame(npi = c("1", "2", "3"), year = 2024, state = c("CO", "CO", "TX")),
    dac   = data.frame(npi = c("1", "2", "3"), year = 2024, state = c("CO", "NM", "TX")),
    op    = data.frame(npi = c("1", "2", "3"), year = 2024, state = c("CO", "NM", "OK")),
    pecos = data.frame(npi = c("1", "2", "3"), year = 2024, state = c("CO", "CO", "TX"))
  )
  out <- attribute_consensus_tier(s, attribute = "state")
  expect_equal(nrow(out), 3)
  expect_equal(as.character(out$tier[out$id == "1"]), "unanimous")
  expect_equal(out$concordance_pct[out$id == "1"], 100)
  # npi 2: CO, NM, NM, CO -> largest group 2 of 4, not a strict majority -> split
  expect_equal(as.character(out$tier[out$id == "2"]), "split")
  expect_equal(out$n_distinct_values[out$id == "2"], 2)
  # npi 3: TX, TX, OK, TX -> 3 of 4, strict majority
  expect_equal(as.character(out$tier[out$id == "3"]), "majority")
  expect_equal(out$n_agree_with_largest_group[out$id == "3"], 3)
})

test_that("attribute_consensus_tier drops identifier-years below min_sources", {
  s <- list(
    a = data.frame(npi = c("1", "2"), year = 2024, state = c("CO", "CO")),
    b = data.frame(npi = c("1"), year = 2024, state = c("CO")),
    c = data.frame(npi = c("1"), year = 2024, state = c("CO"))
  )
  out <- attribute_consensus_tier(s, attribute = "state", min_sources = 2)
  expect_equal(out$id, "1") # npi 2 only has one source (a) reporting -> dropped
  expect_equal(out$n_reporting, 3)
})

test_that("attribute_consensus_tier normalizes values/ids and handles year = NULL", {
  s <- list(
    a = data.frame(npi = " 1 ", state = " co "),
    b = data.frame(npi = "1", state = "CO")
  )
  out <- attribute_consensus_tier(s, attribute = "state", year = NULL)
  expect_equal(out$id, "1")
  expect_equal(as.character(out$tier), "unanimous")
})

test_that("attribute_consensus_tier fails closed on bad inputs", {
  expect_error(attribute_consensus_tier(list(a = data.frame(npi = "1", x = "CO")),
                                         attribute = "state"), "length >= 2")
  expect_error(attribute_consensus_tier(
    list(a = data.frame(npi = "1", state = "CO"), b = data.frame(x = "1", state = "CO")),
    attribute = "state"), "no column 'npi'")
  s <- list(a = data.frame(npi = "1", year = 2024, state = NA_character_),
            b = data.frame(npi = "1", year = 2024, state = NA_character_))
  expect_error(attribute_consensus_tier(s, attribute = "state"), "no non-missing attribute values")
})

test_that("attribute_consensus_tier_flow counts year-over-year transitions for a fixed cohort", {
  tiers <- data.frame(
    id = rep(c("1", "2", "3"), 2), year = rep(c(2023, 2024), each = 3),
    tier = c("unanimous", "split", "majority", "unanimous", "unanimous", "split")
  )
  out <- attribute_consensus_tier_flow(tiers)
  expect_equal(nrow(out), 3)
  expect_equal(out$n[out$tier_from == "unanimous" & out$tier_to == "unanimous"], 1)
  expect_equal(out$n[out$tier_from == "split" & out$tier_to == "unanimous"], 1)
  expect_equal(out$n[out$tier_from == "majority" & out$tier_to == "split"], 1)
})

test_that("attribute_consensus_tier_flow handles a non-contiguous year gap and labels it", {
  tiers <- data.frame(
    id = c("1", "1", "2", "2"), year = c(2021, 2024, 2021, 2024),
    tier = c("unanimous", "split", "majority", "majority")
  )
  out <- attribute_consensus_tier_flow(tiers)
  expect_equal(unique(out$year_from), 2021)
  expect_equal(unique(out$year_to), 2024)
})

test_that("attribute_consensus_tier_flow's require_all_years restricts to a fixed cohort", {
  tiers <- data.frame(
    id = c("1", "1", "2"), year = c(2023, 2024, 2023),
    tier = c("unanimous", "unanimous", "split")
  )
  out <- attribute_consensus_tier_flow(tiers, require_all_years = TRUE)
  expect_equal(nrow(out), 1) # only npi 1 is present in both years

  out_all <- attribute_consensus_tier_flow(tiers, require_all_years = FALSE)
  expect_equal(nrow(out_all), 1) # npi 2 has no 2024 row to transition to either
})

test_that("attribute_consensus_tier_flow fails closed on bad inputs", {
  expect_error(attribute_consensus_tier_flow(data.frame(id = "1", year = 2024, tier = "x")),
               "fewer than 2 distinct years")
  dup <- data.frame(id = c("1", "1", "1"), year = c(2023, 2023, 2024), tier = c("a", "a", "b"))
  expect_error(attribute_consensus_tier_flow(dup), "duplicate")
})

test_that("attribute_source_corroboration ranks the source already at the eventual value", {
  a <- data.frame(npi = c("1", "1"), year = c(2022, 2023), state = c("CO", "NM"))
  b <- data.frame(npi = c("1", "1"), year = c(2022, 2023), state = c("NM", "NM"))
  c <- data.frame(npi = c("1", "1"), year = c(2022, 2023), state = c("CO", "NM"))
  out <- attribute_source_corroboration(list(a = a, b = b, c = c), attributes = "state")
  expect_equal(out$lead_rate[out$source == "b"], 1)
  expect_equal(out$lead_rate[out$source == "a"], 0)
  expect_equal(out$lead_rate[out$source == "c"], 0)
  expect_equal(out$n_pairs[out$source == "b"], 2)
})

test_that("attribute_source_corroboration skips a pair that errors (no shared years) with a message", {
  a <- data.frame(npi = c("1", "1"), year = c(2022, 2023), state = c("CO", "NM"))
  b <- data.frame(npi = c("1", "1"), year = c(2022, 2023), state = c("NM", "NM"))
  d <- data.frame(npi = "1", year = 2025, state = "CO") # shares no year with a or b
  expect_message(
    out <- suppressWarnings(attribute_source_corroboration(list(a = a, b = b, d = d), attributes = "state")),
    "skipping pair"
  )
  expect_equal(sort(out$source), c("a", "b")) # d never contributes a row
})

test_that("attribute_source_corroboration errors when no pair has a usable disagreement", {
  a <- data.frame(npi = c("1", "1"), year = c(2022, 2023), state = c("CO", "CO"))
  b <- data.frame(npi = c("1", "1"), year = c(2022, 2023), state = c("CO", "CO"))
  expect_error(attribute_source_corroboration(list(a = a, b = b), attributes = "state"),
               "no pair of sources produced a usable")
})
