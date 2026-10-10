m <- ent_disposition_map()
calls <- data.frame(
  disposition = c("Yes", "No-practice is full but physician does still see established patients",
                  "No-changed practices", "No-retired or no longer practicing medicine",
                  "No-not specified why", NA, NA, "Yes", NA),
  reached     = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, NA, TRUE, FALSE),
  phone_valid = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE),
  stringsAsFactors = FALSE)

test_that("the ENT codebook maps to the four statuses exactly as the ENT analysis did", {
  out <- classify_listing_status(calls, phone_valid = "phone_valid")
  expect_equal(as.character(out$listing_status),
               c("confirmed_valid", "confirmed_valid", "confirmed_invalid", "confirmed_invalid",
                 "unresolved", "unresolved", "not_attempted", "confirmed_valid", "confirmed_invalid"))
  expect_equal(out$physician_at_location[3], "confirmed_no")
  expect_equal(out$physician_active[3], "confirmed_yes")      # relocated = active elsewhere
  expect_equal(out$physician_active[4], "confirmed_no")
  # staff confirmation outranks a phone-format flag (row 8); an unreached invalid number is invalid (row 9)
  expect_equal(out$invalid_reason[c(3, 4, 8, 9)],
               c("relocated", "retired_inactive", NA, "wrong_disconnected_number"))
  expect_true(all(is.na(out$invalid_reason[out$listing_status != "confirmed_invalid"])))
  expect_equal(levels(out$listing_status), listing_status_levels())
})

test_that("statuses partition every row: exhaustive and mutually exclusive", {
  out <- classify_listing_status(calls, phone_valid = "phone_valid")
  expect_false(anyNA(out$listing_status))
  expect_equal(sum(table(out$listing_status)), nrow(calls))
})

test_that("a disposition outside the codebook stops by default and is unresolved only on request", {
  bad <- calls; bad$disposition[1] <- "Maybe"
  expect_error(classify_listing_status(bad, phone_valid = "phone_valid"), "not in the map: Maybe")
  expect_message(out <- classify_listing_status(bad, phone_valid = "phone_valid", unmapped = "unresolved"), "unresolved")
  expect_equal(as.character(out$listing_status[1]), "unresolved")
})

test_that("presence is never inferred from a reached office alone", {
  d <- data.frame(disposition = NA_character_, reached = TRUE)
  expect_equal(as.character(classify_listing_status(d)$listing_status), "unresolved")
})

test_that("an overlapping or malformed map fails closed", {
  bad <- m; bad$relocated <- c(bad$relocated, "Yes")
  expect_error(classify_listing_status(calls, map = bad), "overlap")
  expect_error(classify_listing_status(calls, map = m[1:3]), "exactly the categories")
})

test_that("the draft OB/GYN map has the same four disjoint categories and classifies like the ENT map", {
  om <- obgyn_disposition_map()
  expect_setequal(names(om), names(ent_disposition_map()))
  expect_false(anyDuplicated(unlist(om)) > 0)
  d <- data.frame(disposition = c("Sees patients here-not accepting new patients", "Moved to another practice location",
                                  "No longer provides this subspecialty care", "No-reason not given", NA),
                  reached = c(TRUE, TRUE, TRUE, TRUE, NA))
  out <- classify_listing_status(d, map = om)
  expect_equal(as.character(out$listing_status),
               c("confirmed_valid", "confirmed_invalid", "confirmed_invalid", "unresolved", "not_attempted"))
  expect_equal(out$physician_active[2:3], c("confirmed_yes", "confirmed_no"))
})
