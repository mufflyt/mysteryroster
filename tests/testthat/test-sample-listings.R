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
