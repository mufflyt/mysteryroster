frame <- data.frame(id = sprintf("n%03d", 1:100),
                    stratum = rep(c("A", "B", "C"), c(60, 30, 10)),
                    stringsAsFactors = FALSE)

test_that("the sample carries pop_N, n_sampled, samp_frac and weight = N/n per stratum", {
  s <- stratified_listing_sample(frame, c(A = 6, B = 15, C = 10), seed = 1)
  expect_equal(nrow(s), 31)
  expect_equal(unique(s$weight[s$stratum == "A"]), 10)
  expect_equal(unique(s$weight[s$stratum == "B"]), 2)
  expect_equal(unique(s$weight[s$stratum == "C"]), 1)
  expect_equal(unique(s$samp_frac[s$stratum == "A"]), 0.1)
  expect_equal(sum(attr(s, "allocation")$n_sampled), 31)
  expect_false(anyDuplicated(s$id) > 0)
})

test_that("the draw is reproducible by seed and changes with it", {
  a <- stratified_listing_sample(frame, c(A = 5), seed = 42)
  b <- stratified_listing_sample(frame, c(A = 5), seed = 42)
  c <- stratified_listing_sample(frame, c(A = 5), seed = 43)
  expect_identical(a$id, b$id)
  expect_false(identical(a$id, c$id))
})

test_that("an impossible allocation stops before any draw", {
  expect_error(stratified_listing_sample(frame, c(C = 11), seed = 1), "exceeds the stratum population")
  expect_error(stratified_listing_sample(frame, c(Z = 1), seed = 1), "absent from the frame")
  expect_error(stratified_listing_sample(frame, c(A = 5)), "seed")
})

test_that("listing_weights fails closed on missing strata instead of dropping rows silently", {
  # table() drops NA, so an unmapped row used to escape both n_sampled and the
  # weight column, vanishing from weighted analyses without a trace.
  df <- data.frame(id = 1:3, stratum = c("a", NA, "b"))
  expect_error(listing_weights(df, pop_N = c(a = 10, b = 20)), "missing stratum")
})

test_that("listing_weights tolerates population strata nobody sampled", {
  # A superset population table is legitimate: the study may sample two zones
  # of four. Weights for the sampled strata must be unaffected.
  df <- data.frame(id = 1:2, stratum = c("a", "a"))
  out <- listing_weights(df, pop_N = c(a = 10, b = 20, unused = 99))
  expect_equal(out$weight, c(5, 5))
})
