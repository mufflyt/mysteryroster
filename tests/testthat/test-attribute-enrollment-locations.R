linkage_fixture <- function() {
  list(
    individual = data.frame(npi = "1", year = 2024, enrollment_id = "individual_a"),
    reassignments = data.frame(npi = "1", year = 2024, enrollment_id = "group_a"),
    locations = data.frame(
      year = 2024, enrollment_id = "group_a",
      location_id = c("site_a", "site_b"), practice_zip5 = c("80204", "80045")
    )
  )
}

test_that("attribute_enrollment_locations never sets clinical_presence_verified from group linkage", {
  f <- linkage_fixture()
  linkage <- suppressMessages(attribute_enrollment_locations(f$individual, f$reassignments, f$locations))
  expect_true(all(is.na(linkage$links$clinical_presence_verified)))
})

test_that("attribute_enrollment_locations retains a direct enrollment edge with no matching location", {
  f <- linkage_fixture()
  linkage <- suppressMessages(attribute_enrollment_locations(f$individual, f$reassignments, f$locations))
  expect_equal(nrow(linkage$unmatched_enrollments), 1)
  expect_equal(linkage$unmatched_enrollments$linkage_path, "direct_enrollment")
})

test_that("attribute_enrollment_locations keeps two group sites as two traceable rows", {
  f <- linkage_fixture()
  linkage <- suppressMessages(attribute_enrollment_locations(f$individual, f$reassignments, f$locations))
  linked <- linkage$links[linkage$links$location_linked, ]
  expect_equal(nrow(linked), 2)
  expect_setequal(linked$location_id, c("site_a", "site_b"))
  expect_true(all(linked$linkage_path == "group_reassignment"))
})

test_that("attribute_enrollment_locations cannot link across different years", {
  f <- linkage_fixture()
  f$locations$year <- 2025
  linkage <- suppressMessages(attribute_enrollment_locations(f$individual, f$reassignments, f$locations))
  expect_false(any(linkage$links$location_linked))
  expect_equal(nrow(linkage$unlinked_locations), 2)
})

test_that("attribute_enrollment_locations preserves every physician-site edge when a group is shared", {
  f <- linkage_fixture()
  f$reassignments <- rbind(f$reassignments, data.frame(npi = "2", year = 2024, enrollment_id = "group_a"))
  linkage <- suppressMessages(attribute_enrollment_locations(f$individual, f$reassignments, f$locations))
  linked <- linkage$links[linkage$links$location_linked, ]
  expect_equal(nrow(linked), 4) # 2 physicians x 2 sites, not collapsed
  counts <- table(linked$id)
  expect_true(all(counts == 2))
  expect_true(all(linked$linkage_path == "group_reassignment"))
})

test_that("attribute_enrollment_locations does not cross-link a reused location_id under an unrelated enrollment", {
  f <- linkage_fixture()
  extra <- f$locations[1, ]
  extra$enrollment_id <- "unrelated_group"
  extra$practice_zip5 <- "99999"
  f$locations <- rbind(f$locations, extra)
  linkage <- suppressMessages(attribute_enrollment_locations(f$individual, f$reassignments, f$locations))
  expect_equal(nrow(linkage$unlinked_locations), 1)
  expect_equal(linkage$unlinked_locations$enrollment_id, "unrelated_group")
  expect_false(any(linkage$links$practice_zip5 == "99999", na.rm = TRUE))
})

test_that("attribute_enrollment_locations leaves a missing direct address missing, even if an unrelated enrollment has locations", {
  f <- linkage_fixture()
  f$locations$enrollment_id <- "unrelated_group" # none of these belong to individual_a or group_a
  linkage <- suppressMessages(attribute_enrollment_locations(f$individual, f$reassignments, f$locations))
  expect_equal(nrow(linkage$unmatched_enrollments), 2)
  expect_false(any(linkage$links$location_linked))
  expect_equal(nrow(linkage$unlinked_locations), 2)
})

test_that("attribute_enrollment_locations rejects a duplicate enrollment edge", {
  f <- linkage_fixture()
  f$reassignments <- rbind(f$reassignments, f$reassignments)
  expect_error(
    attribute_enrollment_locations(f$individual, f$reassignments, f$locations),
    "duplicate keys"
  )
})

test_that("attribute_enrollment_locations rejects a duplicate location key", {
  f <- linkage_fixture()
  f$locations <- rbind(f$locations, f$locations[1, ])
  expect_error(
    attribute_enrollment_locations(f$individual, f$reassignments, f$locations),
    "duplicate keys"
  )
})

test_that("attribute_enrollment_locations requires the standard columns on every input", {
  f <- linkage_fixture()
  expect_error(
    attribute_enrollment_locations(f$individual[, "npi", drop = FALSE], f$reassignments, f$locations),
    "missing column"
  )
})

test_that("attribute_enrollment_locations carries extra location columns through to links", {
  f <- linkage_fixture()
  linkage <- suppressMessages(attribute_enrollment_locations(f$individual, f$reassignments, f$locations))
  linked <- linkage$links[linkage$links$location_linked, ]
  expect_true("practice_zip5" %in% names(linked))
  expect_setequal(linked$practice_zip5, c("80204", "80045"))
})
