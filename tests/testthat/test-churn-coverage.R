# --- attribute_churn_rate --------------------------------------------------

test_that("attribute_churn_rate computes overall, by_year, and by_identifier correctly", {
  panel <- data.frame(
    npi = c("1", "1", "1", "2", "2", "2"),
    year = c(2022, 2023, 2024, 2022, 2023, 2024),
    state = c("CO", "CO", "CO", "NM", "TX", "NM")
  )
  churn <- attribute_churn_rate(panel, attribute = "state")
  expect_equal(churn$overall$n_transitions, 4)
  expect_equal(churn$overall$n_changed, 2)
  expect_equal(churn$overall$pct, 50)

  expect_equal(nrow(churn$by_year), 2)
  expect_equal(churn$by_year$pct, c(50, 50))

  expect_equal(nrow(churn$by_identifier), 2)
  npi1 <- churn$by_identifier[churn$by_identifier$id == "1", ]
  npi2 <- churn$by_identifier[churn$by_identifier$id == "2", ]
  expect_equal(npi1$n_changed, 0)
  expect_equal(npi2$n_changed, 2)

  dist <- churn$change_count_distribution
  expect_equal(dist$n[dist$n_changes == "0"], 1L)
  expect_equal(dist$n[dist$n_changes == "2"], 1L)
})

test_that("attribute_churn_rate does not count a transition across a calendar-year gap", {
  panel <- data.frame(npi = c("1", "1"), year = c(2022, 2024), state = c("CO", "NM"))
  expect_error(
    attribute_churn_rate(panel, attribute = "state"),
    "no consecutive-year transitions"
  )
})

test_that("attribute_churn_rate does not count a transition with a missing value on either side", {
  # both 2022->2023 and 2023->2024 touch the NA -- zero valid transitions for this one id
  panel <- data.frame(npi = c("1", "1", "1"), year = c(2022, 2023, 2024), state = c("CO", NA, "CO"))
  expect_error(attribute_churn_rate(panel, attribute = "state"), "no consecutive-year transitions")

  # adding a second id with a real transition confirms the NA'd id contributes nothing, not an error
  panel2 <- data.frame(
    npi = c("1", "1", "1", "2", "2"), year = c(2022, 2023, 2024, 2022, 2023),
    state = c("CO", NA, "CO", "CO", "NM")
  )
  churn <- attribute_churn_rate(panel2, attribute = "state")
  expect_equal(churn$overall$n_transitions, 1)
  expect_equal(churn$overall$n_changed, 1)
})

test_that("attribute_churn_rate's overall feeds directly into format_proportion_comparison_prose", {
  panel_a <- data.frame(npi = c("1", "1", "1"), year = 2022:2024, state = c("CO", "NM", "TX"))
  panel_b <- data.frame(npi = c("1", "1", "1"), year = 2022:2024, state = c("CO", "CO", "CO"))
  churn_a <- attribute_churn_rate(panel_a, attribute = "state")
  churn_b <- attribute_churn_rate(panel_b, attribute = "state")
  cmp <- format_proportion_comparison_prose(
    n_a = churn_a$overall$n_changed, n_total_a = churn_a$overall$n_transitions,
    n_b = churn_b$overall$n_changed, n_total_b = churn_b$overall$n_transitions,
    label_a = "A", label_b = "B", unit_label = "transitions", condition_label = "changed"
  )
  expect_equal(cmp$a$pct, 100)
  expect_equal(cmp$b$pct, 0)
  expect_match(cmp$comparison_sentence, "100\\.0% of transitions changed in A")
})

test_that("attribute_churn_rate validates inputs", {
  panel <- data.frame(npi = "1", year = 2020, state = "CO")
  expect_error(attribute_churn_rate(panel, attribute = "missing"), "no column")
})

# --- attribute_coverage -----------------------------------------------------

test_that("attribute_coverage ranks sources by coverage of an explicit universe", {
  nppes <- data.frame(npi = c("1", "1", "2"), year = c(2022, 2023, 2022), state = c("CO", "CO", "NM"))
  dac <- data.frame(npi = c("1"), year = c(2022), state = c("CO"))
  universe <- data.frame(npi = c("1", "1", "2"), year = c(2022, 2023, 2022))
  cov <- attribute_coverage(list(nppes = nppes, dac = dac), attribute = "state", universe = universe)

  expect_equal(nrow(cov$overall), 2)
  expect_equal(cov$overall$source[1], "nppes") # ranked by descending pct
  nppes_row <- cov$overall[cov$overall$source == "nppes", ]
  dac_row <- cov$overall[cov$overall$source == "dac", ]
  expect_equal(nppes_row$n_covered, 3)
  expect_equal(nppes_row$n_total, 3)
  expect_equal(nppes_row$pct, 100)
  expect_equal(dac_row$n_covered, 1)
  expect_equal(dac_row$pct, round(100 / 3, 1))
})

test_that("attribute_coverage's by_year breaks coverage down per universe year", {
  nppes <- data.frame(npi = c("1", "1", "2"), year = c(2022, 2023, 2022), state = c("CO", "CO", "NM"))
  dac <- data.frame(npi = c("1"), year = c(2022), state = c("CO"))
  universe <- data.frame(npi = c("1", "1", "2"), year = c(2022, 2023, 2022))
  cov <- attribute_coverage(list(nppes = nppes, dac = dac), attribute = "state", universe = universe)

  dac_2022 <- cov$by_year[cov$by_year$source == "dac" & cov$by_year$year == 2022, ]
  dac_2023 <- cov$by_year[cov$by_year$source == "dac" & cov$by_year$year == 2023, ]
  expect_equal(dac_2022$n_covered, 1)
  expect_equal(dac_2022$n_total, 2)
  expect_equal(dac_2023$n_covered, 0)
  expect_equal(dac_2023$n_total, 1)
})

test_that("attribute_coverage treats a missing/empty attribute value as non-coverage, not just a missing row", {
  nppes <- data.frame(npi = c("1", "2"), year = c(2022, 2022), state = c("CO", NA))
  universe <- data.frame(npi = c("1", "2"), year = c(2022, 2022))
  cov <- attribute_coverage(list(nppes = nppes), attribute = "state", universe = universe)
  expect_equal(cov$overall$n_covered, 1) # npi 2's row exists but state is NA -- does not count
})

test_that("attribute_coverage defaults universe to the union of all sources when not supplied", {
  a <- data.frame(npi = "1", year = 2022, state = "CO")
  b <- data.frame(npi = "2", year = 2023, state = "NM")
  cov <- attribute_coverage(list(a = a, b = b), attribute = "state")
  expect_equal(unique(cov$overall$n_total), 2) # union of (1,2022) and (2,2023)
  a_row <- cov$overall[cov$overall$source == "a", ]
  expect_equal(a_row$n_covered, 1) # a only covers its own row
})

test_that("attribute_coverage works with a single source", {
  a <- data.frame(npi = c("1", "2"), year = c(2022, 2022), state = c("CO", "NM"))
  cov <- attribute_coverage(list(a = a), attribute = "state")
  expect_equal(nrow(cov$overall), 1)
  expect_equal(cov$overall$pct, 100)
})

test_that("attribute_coverage validates inputs", {
  a <- data.frame(npi = "1", year = 2022, state = "CO")
  expect_error(attribute_coverage(list(a = a), attribute = "missing_col"), "no column")
  expect_error(
    attribute_coverage(list(a = a), attribute = "state", universe = data.frame(npi = "1")),
    "no column"
  )
})
