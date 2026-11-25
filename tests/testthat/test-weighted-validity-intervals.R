testthat::test_that("weighted identification bounds use the chosen population", {
  calls <- base::data.frame(
    listing_status = listing_status_levels(), weight = c(8, 2, 6, 4)
  )
  attempted <- validity_sensitivity_bounds(calls, weights = "weight")
  testthat::expect_equal(attempted$n_valid, c(8, 14, 8))
  testthat::expect_equal(attempted$denominator, c(10, 16, 16))
  testthat::expect_equal(attempted$pct_valid, c(80, 87.5, 50))
  sampled <- validity_sensitivity_bounds(
    calls, weights = calls$weight, denominator = "all_sampled"
  )
  testthat::expect_equal(sampled$denominator, c(10, 20, 20))
  testthat::expect_equal(sampled$pct_valid, c(80, 90, 40))
  testthat::expect_match(sampled$scenario[2], "unattempted")
  design <- survey::svydesign(ids = ~1, weights = ~weight, data = calls)
  weighted <- validity_sensitivity_bounds(
    design, denominator = "all_sampled"
  )
  testthat::expect_equal(weighted, sampled)
})

testthat::test_that("unresolved bounds and empty denominators are explicit", {
  calls <- base::data.frame(listing_status = base::rep("unresolved", 4))
  bounds <- validity_sensitivity_bounds(calls)
  testthat::expect_equal(bounds$pct_valid, c(NA_real_, 100, 0))
  calls$listing_status <- "not_attempted"
  bounds <- validity_sensitivity_bounds(calls)
  testthat::expect_true(base::all(base::is.na(bounds$pct_valid)))
  bounds <- validity_sensitivity_bounds(calls, denominator = "all_sampled")
  testthat::expect_equal(bounds$pct_valid, c(NA_real_, 100, 0))
})

testthat::test_that("weights and sensitivity statuses are validated", {
  calls <- base::data.frame(
    stratum = c("a", "a"), weight = c(1, Inf),
    listing_status = c("confirmed_valid", "unresolved")
  )
  testthat::expect_error(listing_validity_design(calls), "finite")
  for (weight in base::list(c(1, Inf), c(1, NA), c(1, 0), c(1, -1))) {
    testthat::expect_error(
      validity_sensitivity_bounds(calls, weights = weight)
    )
  }
  calls$listing_status[1] <- NA_character_
  testthat::expect_error(validity_sensitivity_bounds(calls), "status")
  testthat::expect_error(
    validity_sensitivity_bounds(calls, denominator = "unknown")
  )
})

testthat::test_that("beta survey intervals match survey directly near zero", {
  calls <- base::data.frame(
    stratum = base::rep(c("a", "b"), each = 20),
    weight = base::rep(c(2, 5), each = 20),
    listing_status = c("confirmed_valid", base::rep("confirmed_invalid", 39))
  )
  design <- listing_validity_design(calls)
  design$variables$valid <- base::as.integer(
    design$variables$listing_status == "confirmed_valid"
  )
  reference <- survey::svyciprop(
    ~valid, design, method = "beta", level = 0.90
  )
  estimate <- validity_estimates(design, ci_method = "beta", level_ci = 0.90)
  testthat::expect_equal(estimate$estimate, base::as.numeric(reference))
  testthat::expect_equal(estimate$se, base::as.numeric(survey::SE(reference)))
  testthat::expect_equal(
    c(estimate$ci_low, estimate$ci_high),
    base::as.numeric(stats::confint(reference))
  )
  testthat::expect_equal(
    validity_estimates(design)$ci_low,
    validity_estimates(design, ci_method = "beta")$ci_low
  )
  legacy <- survey::svymean(~valid, design)
  interval <- stats::confint(legacy, level = 0.90)
  estimate <- validity_estimates(design, ci_method = "mean", level_ci = 0.90)
  testthat::expect_equal(estimate$ci_low, base::max(0, interval[1]))
  testthat::expect_equal(estimate$ci_high, base::min(1, interval[2]))
})

testthat::test_that("grouped CIs and counts use the same definitive domain", {
  calls <- base::data.frame(
    stratum = base::rep("one", 8), weight = c(1, 2, 3, 4, 2, 3, 4, 5),
    group = base::rep(c("a", "b"), each = 4),
    listing_status = c(
      "confirmed_valid", "confirmed_invalid", "confirmed_invalid",
      "unresolved", base::rep("unresolved", 4)
    )
  )
  design <- listing_validity_design(calls)
  grouped <- validity_estimates(
    design, by = "group", definitive_only = TRUE, ci_method = "beta"
  )
  testthat::expect_equal(grouped$group, c("a", "b"))
  testthat::expect_equal(grouped$n, c(3L, 0L))
  testthat::expect_true(base::is.na(grouped$estimate[2]))
  domain <- base::subset(
    design, listing_status %in% c("confirmed_valid", "confirmed_invalid")
  )
  reference <- survey::svyciprop(
    ~I(listing_status == "confirmed_valid"), domain, method = "beta"
  )
  testthat::expect_equal(grouped$estimate[1], base::as.numeric(reference))
  testthat::expect_equal(
    c(grouped$ci_low[1], grouped$ci_high[1]),
    base::as.numeric(stats::confint(reference))
  )
})

testthat::test_that("boundary and empty samples do not invent intervals", {
  for (status_value in c("confirmed_valid", "confirmed_invalid", "unresolved")) {
    calls <- base::data.frame(
      stratum = base::rep("a", 4), weight = base::rep(1, 4),
      listing_status = base::rep(status_value, 4)
    )
    estimate <- validity_estimates(
      listing_validity_design(calls), definitive_only = TRUE
    )
    testthat::expect_true(base::is.na(estimate$ci_low))
    testthat::expect_true(base::is.na(estimate$ci_high))
    testthat::expect_equal(
      estimate$n, if (status_value == "unresolved") 0L else 4L
    )
    expected <- switch(
      status_value, confirmed_valid = 1, confirmed_invalid = 0,
      unresolved = NA_real_
    )
    testthat::expect_equal(estimate$estimate, expected)
  }
})

testthat::test_that("status and interval arguments reject invalid values", {
  calls <- base::data.frame(
    stratum = c("a", "a"), weight = c(1, 1),
    listing_status = c("confirmed_valid", "confirmed_invalid")
  )
  design <- listing_validity_design(calls)
  for (confidence in c(0, 1, NA, Inf)) {
    testthat::expect_error(validity_estimates(design, level_ci = confidence))
  }
  testthat::expect_error(validity_estimates(design, ci_method = "unknown"))
  testthat::expect_error(validity_estimates(design, level = "unknown"))
  design$variables$listing_status[1] <- NA_character_
  testthat::expect_error(validity_estimates(design), "status")
})
