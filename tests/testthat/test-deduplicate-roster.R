test_that("deduplicate_roster keeps the highest-quality row and returns removals", {
  x <- data.frame(
    physician_name = c("Smith Jane", "SMITH JANE", "Doe John"),
    city = c("Denver", " Denver ", "Chicago"),
    state = c("CO", "co", "IL"),
    quality = c(1, 10, 0),
    stringsAsFactors = FALSE
  )
  out <- deduplicate_roster(x, key_cols = c("physician_name", "city", "state"),
                            score = "quality")
  expect_equal(nrow(out$data), 2L)
  expect_equal(nrow(out$removed), 1L)
  expect_identical(out$removed$quality, 1)
  expect_identical(out$removed$duplicate_resolution, "removed_duplicate")
  expect_identical(out$data$duplicate_resolution[out$data$physician_name == "SMITH JANE"],
                   "kept_best_record")
  expect_equal(out$counts$duplicate_groups, 1L)
})

test_that("NPI-aware key preserves same-name clinicians with different NPIs", {
  x <- data.frame(
    npi = c("1111111111", "2222222222"),
    physician_name = c("Smith Jane", "Smith Jane"),
    city = c("Denver", "Denver"),
    state = c("CO", "CO"),
    stringsAsFactors = FALSE
  )
  out <- deduplicate_roster(x, key_cols = c("physician_name", "city", "state"),
                            npi_col = "npi")
  expect_equal(nrow(out$data), 2L)
  expect_equal(nrow(out$removed), 0L)
})

test_that("present and missing NPIs do not collapse into one identity", {
  x <- data.frame(
    npi = c("1111111111", NA_character_),
    physician_name = c("Smith Jane", "Smith Jane"),
    city = c("Denver", "Denver"),
    state = c("CO", "CO"),
    stringsAsFactors = FALSE
  )
  out <- deduplicate_roster(x, key_cols = c("physician_name", "city", "state"),
                            npi_col = "npi")
  expect_equal(nrow(out$data), 2L)
})

test_that("same NPI duplicate group collapses even when case and whitespace differ", {
  x <- data.frame(
    npi = c("1111111111", " 1111111111 "),
    physician_name = c("Smith Jane", "SMITH JANE"),
    city = c("Denver", " Denver "),
    state = c("CO", "co"),
    quality = c(0, 5),
    stringsAsFactors = FALSE
  )
  out <- deduplicate_roster(x, key_cols = c("physician_name", "city", "state"),
                            npi_col = "npi", score = "quality")
  expect_equal(nrow(out$data), 1L)
  expect_identical(out$data$quality, 5)
})

test_that("conflicting attribute groups are returned explicitly", {
  x <- data.frame(
    physician_name = c("Smith Jane", "Smith Jane"),
    city = c("Denver", "Denver"),
    state = c("CO", "CO"),
    subspecialty_name = c("MFM", "FPMRS"),
    quality = c(10, 1),
    stringsAsFactors = FALSE
  )
  out <- deduplicate_roster(x, key_cols = c("physician_name", "city", "state"),
                            score = "quality", conflict_col = "subspecialty_name")
  expect_equal(out$counts$conflict_groups, 1L)
  expect_equal(nrow(out$conflicts), 2L)
  expect_true(all(c("MFM", "FPMRS") %in% out$conflicts$subspecialty_name))
  expect_true("dedup_group" %in% names(out$conflicts))
})

test_that("duplicate values that agree are not conflicts", {
  x <- data.frame(
    physician_name = c("Smith Jane", "Smith Jane"),
    city = c("Denver", "Denver"),
    state = c("CO", "CO"),
    subspecialty_name = c("MFM", "MFM"),
    stringsAsFactors = FALSE
  )
  out <- deduplicate_roster(x, key_cols = c("physician_name", "city", "state"),
                            conflict_col = "subspecialty_name")
  expect_equal(out$counts$conflict_groups, 0L)
  expect_equal(nrow(out$conflicts), 0L)
})

test_that("ties keep the earliest input row and output order is input order", {
  x <- data.frame(
    physician_name = c("Zed", "Smith Jane", "Smith Jane", "Alpha"),
    city = c("A", "Denver", "Denver", "B"),
    state = c("CO", "CO", "CO", "CO"),
    quality = c(0, 5, 5, 0),
    stringsAsFactors = FALSE
  )
  out <- deduplicate_roster(x, key_cols = c("physician_name", "city", "state"),
                            score = "quality")
  expect_identical(out$data$physician_name, c("Zed", "Smith Jane", "Alpha"))
  expect_identical(out$removed$physician_name, "Smith Jane")
})

test_that("input validation is loud", {
  x <- data.frame(name = "A", quality = 1)
  expect_error(deduplicate_roster(x, key_cols = "missing"), "missing key")
  expect_error(deduplicate_roster(x, key_cols = "name", npi_col = "missing"), "missing npi")
  expect_error(deduplicate_roster(x, key_cols = "name", conflict_col = "missing"), "missing conflict")
  expect_error(deduplicate_roster(x, key_cols = "name", score = "missing"), "missing score")

  x$quality <- NA_real_
  expect_error(deduplicate_roster(x, key_cols = "name", score = "quality"), "finite numeric")
})

test_that("empty input returns an empty audit object", {
  x <- data.frame(name = character(), stringsAsFactors = FALSE)
  out <- deduplicate_roster(x, key_cols = "name")
  expect_equal(nrow(out$data), 0L)
  expect_equal(out$counts$input_rows, 0L)
  expect_equal(out$counts$output_rows, 0L)
})
