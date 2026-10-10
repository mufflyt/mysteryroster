test_that("listing_weights attaches N/n per stratum and fails closed", {
  df <- data.frame(stratum = c("A", "A", "B"), ok = c(TRUE, FALSE, TRUE))
  w <- listing_weights(df, pop_N = c(A = 10L, B = 4L))
  expect_equal(w$weight, c(5, 5, 4))
  expect_equal(w$samp_frac, c(0.2, 0.2, 0.25))
  expect_equal(attr(w, "allocation")$n_sampled, c(2L, 1L))
  expect_error(listing_weights(df, pop_N = c(A = 10L)), "no population for strata: B")
  expect_error(listing_weights(df, pop_N = c(A = 1L, B = 4L)), "exceeds the stratum population for: A")
})

test_that("validation_cascade reports both denominators, weighted and by group", {
  df <- data.frame(
    stratum   = c("A", "A", "A", "A", "B", "B"),
    attempted = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE),
    reached   = c(TRUE, TRUE, TRUE, FALSE, TRUE, NA),
    location  = c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE),
    workforce = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE),
    weight    = c(2, 2, 2, 2, 5, 5)
  )
  st <- c("attempted", "reached", "location", "workforce")
  u <- validation_cascade(df, st)
  expect_equal(u$n, c(5L, 4L, 2L, 1L))
  expect_equal(u$count, u$n)
  expect_equal(u$pct_of_first, c(100, 80, 40, 20))
  expect_equal(u$pct_of_prior, c(NA, 80, 50, 50))
  w <- validation_cascade(df, st, by = "stratum", weights = "weight")
  all <- w[w$group == "all", ]
  expect_equal(all$count, c(13, 11, 4, 2))
  expect_equal(all$pct_of_prior[2], 100 * 11 / 13)
  b <- w[w$group == "B", ]
  expect_equal(b$n, c(1L, 1L, 0L, 0L))
  expect_true(is.na(b$pct_of_prior[4]))
  bad <- df; bad$workforce[3] <- TRUE
  expect_error(validation_cascade(bad, st), "stages must nest")
  expect_error(validation_cascade(df, c("attempted", "weight")), "is not logical")
})

test_that("validity_estimates accepts an outcome column and drops NA outcomes", {
  df <- data.frame(
    stratum = rep(c("A", "B"), each = 4),
    weight  = rep(c(2, 3), each = 4),
    listing_status = rep("confirmed_valid", 8),
    location_valid = c(TRUE, TRUE, FALSE, NA, TRUE, FALSE, FALSE, FALSE)
  )
  d <- listing_validity_design(df)
  e <- validity_estimates(d, outcome = "location_valid")
  expect_equal(e$n, 7L)
  expect_equal(e$estimate, (2 * 2 + 3 * 1) / (2 * 3 + 3 * 4))
  byg <- validity_estimates(d, outcome = "location_valid", by = "stratum")
  expect_equal(byg$estimate[byg$stratum == "A"], 2 / 3)
  expect_equal(byg$n, c(3L, 4L))
  expect_error(validity_estimates(d, outcome = "weight"), "is not logical")
  expect_error(validity_estimates(d, outcome = "nope"), "no column")
})
