# --- attribute_odd_one_out / attribute_odd_one_out_rate ---------------------

test_that("attribute_odd_one_out distinguishes a lone dissenter from a tied split", {
  s <- list(
    nppes = data.frame(npi = c("1", "2"), year = 2024, state = c("CO", "CO")),
    dac   = data.frame(npi = c("1", "2"), year = 2024, state = c("CO", "CO")),
    op    = data.frame(npi = c("1", "2"), year = 2024, state = c("NM", "NM")),
    pecos = data.frame(npi = c("1", "2"), year = 2024, state = c("CO", "NM"))
  )
  out <- attribute_odd_one_out(s, attribute = "state")
  # npi 1: CO, CO, NM, CO -> op is the lone dissenter
  expect_equal(as.character(out$structure[out$id == "1"]), "odd_one_out")
  expect_equal(out$odd_source[out$id == "1"], "op")
  # npi 2: CO, CO, NM, NM -> tied 2-2, no single outlier
  expect_equal(as.character(out$structure[out$id == "2"]), "no_single_outlier")
  expect_true(is.na(out$odd_source[out$id == "2"]))
})

test_that("attribute_odd_one_out marks full agreement as unanimous, not odd_one_out", {
  s <- list(
    a = data.frame(npi = "1", year = 2024, state = "CO"),
    b = data.frame(npi = "1", year = 2024, state = "CO"),
    c = data.frame(npi = "1", year = 2024, state = "CO")
  )
  out <- attribute_odd_one_out(s, attribute = "state")
  expect_equal(as.character(out$structure), "unanimous")
  expect_true(is.na(out$odd_source))
})

test_that("attribute_odd_one_out requires >= 3 sources and drops thinner identifier-years", {
  expect_error(attribute_odd_one_out(list(a = data.frame(npi = "1", state = "CO"),
                                           b = data.frame(npi = "1", state = "CO")),
                                      attribute = "state"), "length >= 3")
  s <- list(
    a = data.frame(npi = c("1", "2"), year = 2024, state = c("CO", "CO")),
    b = data.frame(npi = c("1", "2"), year = 2024, state = c("CO", "NM")),
    c = data.frame(npi = c("1"), year = 2024, state = c("CO"))
  )
  out <- attribute_odd_one_out(s, attribute = "state")
  expect_equal(out$id, "1") # npi 2 only has 2 sources reporting -> dropped
})

test_that("attribute_odd_one_out_rate gives every source a row, even at a zero rate", {
  s <- list(
    nppes = data.frame(npi = c("1", "2"), year = 2024, state = c("CO", "NY")),
    dac   = data.frame(npi = c("1", "2"), year = 2024, state = c("CO", "NY")),
    op    = data.frame(npi = c("1", "2"), year = 2024, state = c("NM", "NY")),
    pecos = data.frame(npi = c("1", "2"), year = 2024, state = c("CO", "NJ"))
  )
  out <- attribute_odd_one_out_rate(attribute_odd_one_out(s, attribute = "state"))
  expect_equal(nrow(out), 4)
  expect_equal(out$odd_rate[out$source == "op"], 0.5)
  expect_equal(out$odd_rate[out$source == "pecos"], 0.5)
  expect_equal(out$odd_rate[out$source == "nppes"], 0)
  expect_equal(out$odd_rate[out$source == "dac"], 0)
  expect_true(all(out$n_eligible == 2))
})

# --- attribute_agreement_by_covariate ---------------------------------------

test_that("attribute_agreement_by_covariate stratifies agreement by an arbitrary covariate", {
  source_a <- data.frame(npi = c("1", "2", "3"), state = c("CO", "NY", "TX"))
  source_b <- data.frame(npi = c("1", "2", "3"), state = c("CO", "NJ", "TX"))
  rurality <- data.frame(npi = c("1", "2", "3"), setting = c("rural", "urban", "urban"))
  out <- attribute_agreement_by_covariate(source_a, source_b, rurality,
                                           attribute = "state", covariate = "setting")
  expect_equal(out$agree_pct[out$stratum == "rural"], 100)
  expect_equal(out$agree_pct[out$stratum == "urban"], 50)
  expect_equal(out$n[out$stratum == "urban"], 2)
})

test_that("attribute_agreement_by_covariate respects factor level order and drops empty strata", {
  source_a <- data.frame(npi = c("1", "2"), state = c("CO", "CO"))
  source_b <- data.frame(npi = c("1", "2"), state = c("CO", "CO"))
  cov <- data.frame(npi = c("1", "2"), tier = factor(c("high", "low"), levels = c("low", "mid", "high")))
  out <- attribute_agreement_by_covariate(source_a, source_b, cov, attribute = "state", covariate = "tier")
  expect_equal(out$stratum, c("low", "high")) # "mid" has zero rows and is dropped, order preserved
})

test_that("attribute_agreement_by_covariate fails closed on missing columns and empty matches", {
  expect_error(attribute_agreement_by_covariate(
    data.frame(npi = "1", x = "CO"), data.frame(npi = "1", state = "CO"),
    data.frame(npi = "1", setting = "rural"), attribute = "state", covariate = "setting"
  ), "no column 'state'")
  expect_error(attribute_agreement_by_covariate(
    data.frame(npi = "1", state = NA_character_), data.frame(npi = "1", state = "CO"),
    data.frame(npi = "1", setting = "rural"), attribute = "state", covariate = "setting"
  ), "no matched rows")
})

# --- wilson_ci / attribute_agreement_add_ci ---------------------------------

test_that("wilson_ci returns proportions by default and percentages when asked", {
  prop <- wilson_ci(9, 10)
  expect_true(prop$ci_low > 0 && prop$ci_low < 0.9 && prop$ci_high > 0.9 && prop$ci_high <= 1)
  pct <- wilson_ci(9, 10, as_percent = TRUE)
  expect_equal(pct$ci_low, round(prop$ci_low * 100, 1), tolerance = 0.2)
  expect_equal(pct$ci_high, round(prop$ci_high * 100, 1), tolerance = 0.2)
})

test_that("wilson_ci is NA for zero/missing n_compared and never exceeds [0, 1]", {
  out <- wilson_ci(n_agree = c(0, NA, 5), n_compared = c(0, 10, 5))
  expect_true(is.na(out$ci_low[1]) && is.na(out$ci_high[1]))
  expect_true(is.na(out$ci_low[2]) && is.na(out$ci_high[2]))
  expect_equal(out$ci_high[3], 1) # n_agree == n_compared -> upper bound capped at 1
})

test_that("wilson_ci fails closed on mismatched lengths and n_agree > n_compared", {
  expect_error(wilson_ci(c(1, 2), 10), "same length")
  expect_error(wilson_ci(11, 10), "cannot exceed")
})

test_that("attribute_agreement_add_ci attaches ci_low/ci_high without disturbing existing columns", {
  s <- list(
    a = data.frame(npi = c("1", "2"), year = 2024, state = c("CO", "CO")),
    b = data.frame(npi = c("1", "2"), year = 2024, state = c("CO", "NM"))
  )
  summ <- attribute_agreement_summary(attribute_agreement(s, attribute = "state"))
  out <- attribute_agreement_add_ci(summ)
  expect_equal(out$n_agree, summ$n_agree)
  expect_true(all(c("ci_low", "ci_high") %in% names(out)))
  expect_true(out$ci_low <= out$pct_agree && out$pct_agree <= out$ci_high)
})

test_that("attribute_agreement_add_ci errors on a missing column name", {
  expect_error(attribute_agreement_add_ci(data.frame(x = 1)), "no column 'n_agree'")
})
