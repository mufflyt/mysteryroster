# Each test isolates one failure in enrollment input handling.
oig_linkage_inputs <- function() {
  base::list(
    individual = base::data.frame(
      npi = "1", year = 2024L, enrollment_id = "individual_a"),
    group = base::data.frame(
      npi = "1", year = 2024L, enrollment_id = "group_a"),
    locations = base::data.frame(
      year = 2024L, enrollment_id = "group_a",
      location_id = base::c("site_a", "site_b"),
      practice_zip5 = base::c("80204", "80045"))
  )
}

testthat::test_that("group-only linkage accepts an empty direct table", {
  f <- oig_linkage_inputs()
  linkage <- base::suppressMessages(attribute_enrollment_locations(
    f$individual[0L, ], f$group, f$locations))
  testthat::expect_equal(base::nrow(linkage$links), 2L)
  testthat::expect_true(base::all(linkage$links$location_linked))
  testthat::expect_true(base::all(
    linkage$links$linkage_path == "group_reassignment"))
})

testthat::test_that("direct-only linkage retains unmatched direct edges", {
  f <- oig_linkage_inputs()
  linkage <- base::suppressMessages(attribute_enrollment_locations(
    f$individual, f$group[0L, ], f$locations))
  testthat::expect_equal(base::nrow(linkage$links), 1L)
  testthat::expect_equal(base::nrow(linkage$unmatched_enrollments), 1L)
  testthat::expect_equal(base::nrow(linkage$unlinked_locations), 2L)
})

testthat::test_that("no locations retains every enrollment edge", {
  f <- oig_linkage_inputs()
  linkage <- base::suppressMessages(attribute_enrollment_locations(
    f$individual, f$group, f$locations[0L, ]))
  testthat::expect_equal(base::nrow(linkage$links), 2L)
  testthat::expect_false(base::any(linkage$links$location_linked))
  testthat::expect_true(base::all(
    base::is.na(linkage$links$practice_zip5)))
})

testthat::test_that("no edges preserves all unlinked locations", {
  f <- oig_linkage_inputs()
  linkage <- base::suppressMessages(attribute_enrollment_locations(
    f$individual[0L, ], f$group[0L, ], f$locations))
  testthat::expect_equal(base::nrow(linkage$links), 0L)
  testthat::expect_equal(base::nrow(linkage$unlinked_locations), 2L)
})

testthat::test_that("all-empty linkage retains typed output columns", {
  f <- oig_linkage_inputs()
  linkage <- base::suppressMessages(attribute_enrollment_locations(
    f$individual[0L, ], f$group[0L, ], f$locations[0L, ]))
  testthat::expect_equal(base::nrow(linkage$links), 0L)
  testthat::expect_type(linkage$links$location_linked, "logical")
  testthat::expect_type(linkage$links$clinical_presence_verified, "logical")
  testthat::expect_type(linkage$links$practice_zip5, "character")
})

testthat::test_that("normalized duplicate enrollment edges are rejected", {
  f <- oig_linkage_inputs()
  duplicate <- f$group
  duplicate$npi <- " 1 "
  duplicate$enrollment_id <- " group_a "
  f$group <- base::rbind(f$group, duplicate)
  testthat::expect_error(attribute_enrollment_locations(
    f$individual, f$group, f$locations), "duplicate keys")
})

testthat::test_that("normalized duplicate locations are rejected", {
  f <- oig_linkage_inputs()
  duplicate <- f$locations[1L, ]
  duplicate$enrollment_id <- " group_a "
  duplicate$location_id <- " site_a "
  f$locations <- base::rbind(f$locations, duplicate)
  testthat::expect_error(attribute_enrollment_locations(
    f$individual, f$group, f$locations), "duplicate keys")
})

testthat::test_that("reserved metadata cannot corrupt linkage provenance", {
  f <- oig_linkage_inputs()
  for (column in base::c("id", "linkage_path", "location_linked",
                        "clinical_presence_verified")) {
    locations <- f$locations
    locations[[column]] <- "injected"
    testthat::expect_error(attribute_enrollment_locations(
      f$individual, f$group, locations), "reserved")
  }
})

testthat::test_that("normalization still links legitimate padded keys", {
  f <- oig_linkage_inputs()
  f$group$enrollment_id <- " group_a "
  linkage <- base::suppressMessages(attribute_enrollment_locations(
    f$individual, f$group, f$locations))
  testthat::expect_equal(base::sum(linkage$links$location_linked), 2L)
  testthat::expect_true(base::all(
    base::is.na(linkage$links$clinical_presence_verified)))
})
