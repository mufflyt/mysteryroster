# Three registries, hand-computed zones. ids: nppes 1-6, board 3-8, society 5,6,9.
src <- list(nppes = as.character(1:6), board = as.character(3:8), society = c("5", "6", "9"))

test_that("registry_overlap labels every identifier with its exact zone", {
  ov <- registry_overlap(src)
  expect_equal(nrow(ov), 9)
  z <- setNames(ov$overlap_zone, ov$id)
  expect_equal(unname(z[c("1", "2")]), rep("nppes", 2))
  expect_equal(unname(z[c("3", "4")]), rep("nppes+board", 2))
  expect_equal(unname(z[c("5", "6")]), rep("nppes+board+society", 2))
  expect_equal(unname(z[c("7", "8")]), rep("board", 2))
  expect_equal(unname(z["9"]), "society")
  expect_equal(attr(ov, "registries"), c("nppes", "board", "society"))
})

test_that("zone counts close on the identifier total and on every registry total", {
  ov <- registry_overlap(src)
  ct <- overlap_zone_counts(ov)
  expect_equal(sum(ct$n), 9)
  expect_equal(ct$n[ct$zone == "nppes+board+society"], 2)
  expect_equal(sum(ct$n[grepl("board", ct$zone)]), 6)
  # negative control: corrupt a membership flag and the closure guard fires
  bad <- ov; bad$in_board[bad$id == "7"] <- FALSE
  expect_error(overlap_zone_counts(bad), "does not close")
})

test_that("identifiers are trimmed, deduplicated, and missing ones are dropped and counted", {
  ov <- registry_overlap(list(a = c(" 1", "1", NA, ""), b = data.frame(npi = c(1, 2))))
  expect_equal(sort(ov$id), c("1", "2"))
  expect_equal(unname(attr(ov, "dropped")["a"]), 2L)
  expect_equal(ov$overlap_zone[ov$id == "1"], "a+b")
})

test_that("bad inputs fail closed", {
  expect_error(registry_overlap(list(a = "1", a = "2")), "unique")
  expect_error(registry_overlap(list("a+b" = "1")), "without '\\+'")
  expect_error(registry_overlap(list(a = data.frame(x = 1))), "no column 'npi'")
  expect_error(registry_overlap(list(a = c(NA, ""))), "no identifiers")
})

test_that("frame_strata keeps only the frame registry and orders strata by registry count", {
  fs <- frame_strata(registry_overlap(src), frame = "nppes")
  expect_equal(nrow(fs), 6)
  expect_true(all(fs$in_nppes))
  expect_equal(levels(fs$stratum), c("nppes", "nppes+board", "nppes+board+society"))
  expect_error(frame_strata(registry_overlap(src), frame = "nope"))
})

test_that("plot_registry_overlap uses the same counts as the table", {
  skip_if_not_installed("eulerr")
  fit <- plot_registry_overlap(registry_overlap(src), shape = "ellipse")
  expect_s3_class(fit, "euler")
  expect_equal(sum(fit$original.values), 9)
})
