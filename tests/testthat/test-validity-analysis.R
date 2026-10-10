make_sample <- function() {
  set.seed(7)
  n <- c(A = 40, B = 60)
  df <- data.frame(stratum = rep(names(n), n), stringsAsFactors = FALSE)
  df$weight <- ifelse(df$stratum == "A", 10, 2)       # N_A = 400, N_B = 120
  df$listing_status <- factor(c(rep(c("confirmed_valid", "confirmed_invalid"), c(30, 10)),
                                rep(c("confirmed_valid", "confirmed_invalid", "unresolved", "not_attempted"), c(24, 24, 6, 6))),
                              levels = listing_status_levels())
  df$reached <- ifelse(df$listing_status == "not_attempted", NA, TRUE)
  df$rural <- factor(rep(c("urban", "rural"), 50))
  df
}

test_that("the design-weighted proportion equals the hand-computed value", {
  df <- make_sample()
  d <- listing_validity_design(df)
  est <- validity_estimates(d)
  # weighted valid = 30*10 + 24*2 = 348 over total weight 400 + 120 = 520
  expect_equal(est$estimate, 348 / 520, tolerance = 1e-10)
  expect_equal(est$n, 100)
  by <- validity_estimates(d, by = "stratum")
  expect_equal(by$estimate[by$stratum == "A"], 0.75)
  expect_equal(by$estimate[by$stratum == "B"], 0.40)
  expect_true(all(by$ci_low <= by$estimate & by$estimate <= by$ci_high))
})

test_that("definitive_only changes the denominator to confirmed listings", {
  d <- listing_validity_design(make_sample())
  est <- validity_estimates(d, definitive_only = TRUE)
  # weighted: valid 348; invalid 10*10 + 24*2 = 148 -> 348/496
  expect_equal(est$estimate, 348 / 496, tolerance = 1e-10)
  expect_equal(est$n, 88)
})

test_that("the design refuses missing or non-positive weights and missing columns", {
  df <- make_sample(); df$weight[1] <- 0
  expect_error(listing_validity_design(df), "positive")
  expect_error(listing_validity_design(make_sample(), strata = "nope"), "no column")
})

test_that("sensitivity bounds are ordered and exclude not-attempted from every denominator", {
  b <- validity_sensitivity_bounds(make_sample())
  expect_equal(b$denominator, c(88, 94, 94))
  expect_equal(b$n_valid, c(54, 60, 54))
  expect_true(b$pct_valid[3] <= b$pct_valid[1] && b$pct_valid[1] <= b$pct_valid[2])
  expect_equal(attr(b, "not_attempted"), 6)
  bad <- make_sample(); bad$listing_status <- as.character(bad$listing_status); bad$listing_status[1] <- "maybe"
  expect_error(validity_sensitivity_bounds(bad), "outside listing_status_levels")
})

test_that("the survey-weighted model recovers a built-in stratum effect as an odds ratio", {
  d <- listing_validity_design(make_sample())
  m <- validity_model(d, ~ stratum)
  # OR(B vs A) = (0.5/0.5) / (0.75/0.25) = 1/3 on definitive listings
  or_b <- m$estimate[m$term == "stratumB"]
  expect_equal(or_b, 1 / 3, tolerance = 1e-6)
  expect_true(all(c("conf.low", "conf.high") %in% names(m)))
})
