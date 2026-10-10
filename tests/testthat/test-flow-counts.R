test_that("flow counts partition the sample overall and per stratum", {
  df <- data.frame(
    stratum = rep(c("A", "B"), each = 5),
    listing_status = factor(c("confirmed_valid", "confirmed_invalid", "unresolved", "not_attempted", "confirmed_valid",
                              "confirmed_valid", "unresolved", "unresolved", "not_attempted", "confirmed_invalid"),
                            levels = listing_status_levels()),
    reached = c(TRUE, TRUE, FALSE, NA, TRUE, TRUE, FALSE, TRUE, NA, TRUE))
  fc <- listing_flow_counts(df, by = "stratum")
  all <- fc[fc$group == "all", ]
  expect_equal(all$sampled, 10); expect_equal(all$not_attempted, 2); expect_equal(all$attempted, 8)
  expect_equal(all$reached, 6); expect_equal(all$definitive, 5); expect_equal(all$unresolved, 3)
  expect_equal(fc$sampled[fc$group == "A"], 5)
  expect_equal(sum(fc$definitive[fc$group != "all"]), all$definitive)
})

test_that("a status outside the four levels breaks the partition and stops", {
  df <- data.frame(listing_status = c("confirmed_valid", "maybe"), reached = c(TRUE, TRUE))
  expect_error(listing_flow_counts(df), "do not partition")
})
