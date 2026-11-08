test_that("attribute_set_agreement_summary aggregates comparable count and both agreement rates", {
  s <- list(
    nppes = data.frame(npi = c("1", "1", "2"), year = 2024, state = c("CO", "NM", "CO")),
    pecos = data.frame(npi = c("1", "2"), year = 2024, state = c("CO", "NM"))
  )
  summary <- attribute_set_agreement_summary(attribute_set_agreement(s, attribute = "state"))
  expect_equal(summary$n_compared, 2)
  expect_equal(summary$n_any_shared, 1) # npi 1: {CO,NM} vs {CO} shares CO
  expect_equal(summary$pct_any_shared, 0.5)
  expect_equal(summary$n_same_set, 0) # neither npi has exactly matching sets
  expect_equal(summary$pct_same_set, 0)
})

test_that("attribute_set_agreement_summary reports proportions in [0, 1], not percentages", {
  s <- list(
    a = data.frame(npi = "1", year = 2024, state = "CO"),
    b = data.frame(npi = "1", year = 2024, state = "CO")
  )
  summary <- attribute_set_agreement_summary(attribute_set_agreement(s, attribute = "state"))
  expect_equal(summary$pct_any_shared, 1)
  expect_equal(summary$pct_same_set, 1)
})

test_that("attribute_set_agreement_summary groups by source pair and year separately", {
  sa1 <- attribute_set_agreement(
    list(a = data.frame(npi = "1", year = 2023, state = "CO"), b = data.frame(npi = "1", year = 2023, state = "CO")),
    attribute = "state"
  )
  sa2 <- attribute_set_agreement(
    list(a = data.frame(npi = "1", year = 2024, state = "CO"), b = data.frame(npi = "1", year = 2024, state = "NM")),
    attribute = "state"
  )
  combined <- rbind(sa1, sa2)
  summary <- attribute_set_agreement_summary(combined)
  expect_equal(nrow(summary), 2)
  expect_equal(summary$pct_any_shared[summary$year == 2023], 1)
  expect_equal(summary$pct_any_shared[summary$year == 2024], 0)
})

test_that("attribute_set_agreement_summary requires the standard columns", {
  expect_error(
    attribute_set_agreement_summary(data.frame(source_a = "a", source_b = "b")),
    "missing column"
  )
})
