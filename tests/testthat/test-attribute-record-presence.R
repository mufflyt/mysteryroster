test_that("attribute_record_presence distinguishes field_not_in_extract from populated", {
  coverage <- data.frame(
    id = c("1", "1"), year = 2024, source = c("nppes", "pecos"),
    record_present = c(TRUE, TRUE), record_status = "present"
  )
  dictionary <- data.frame(source = c("nppes", "pecos"), attribute = "practice_zip5")
  source_fields <- data.frame(
    year = 2024, source = c("nppes", "pecos"), attribute = "practice_zip5",
    field_available = c(TRUE, FALSE)
  )
  claims <- data.frame(npi = "1", year = 2024, source = "nppes", attribute = "practice_zip5", value = "80204")
  indicators <- suppressMessages(attribute_record_presence(coverage, dictionary, source_fields, claims))

  nppes <- indicators[indicators$source == "nppes", ]
  pecos <- indicators[indicators$source == "pecos", ]
  expect_equal(nppes$field_status, "populated")
  expect_true(nppes$completeness_eligible)
  expect_equal(pecos$field_status, "field_not_in_extract")
  expect_false(pecos$completeness_eligible)
})

test_that("attribute_record_presence distinguishes field_row_missing from field_not_in_extract", {
  coverage <- data.frame(
    id = c("1", "2"), year = 2024, source = "nppes",
    record_present = TRUE, record_status = "present"
  )
  dictionary <- data.frame(source = "nppes", attribute = "practice_zip5")
  source_fields <- data.frame(year = 2024, source = "nppes", attribute = "practice_zip5", field_available = TRUE)
  claims <- data.frame(npi = "1", year = 2024, source = "nppes", attribute = "practice_zip5", value = "80204")
  indicators <- suppressMessages(attribute_record_presence(coverage, dictionary, source_fields, claims))

  has_claim <- indicators[indicators$id == "1", ]
  no_claim <- indicators[indicators$id == "2", ]
  expect_equal(has_claim$field_status, "populated")
  expect_equal(no_claim$field_status, "field_row_missing") # field is available but no row was ever submitted
})

test_that("attribute_record_presence marks a present-but-blank value as field_value_missing", {
  coverage <- data.frame(id = "1", year = 2024, source = "nppes", record_present = TRUE, record_status = "present")
  dictionary <- data.frame(source = "nppes", attribute = "practice_zip5")
  source_fields <- data.frame(year = 2024, source = "nppes", attribute = "practice_zip5", field_available = TRUE)
  claims <- data.frame(npi = "1", year = 2024, source = "nppes", attribute = "practice_zip5", value = "  ")
  indicators <- suppressMessages(attribute_record_presence(coverage, dictionary, source_fields, claims))
  expect_equal(indicators$field_status, "field_value_missing")
})

test_that("attribute_record_presence passes record_status through when the record itself is absent", {
  coverage <- data.frame(
    id = "1", year = 2024, source = "nppes",
    record_present = FALSE, record_status = "absent_from_declared_extract"
  )
  dictionary <- data.frame(source = "nppes", attribute = "practice_zip5")
  source_fields <- data.frame(year = 2024, source = "nppes", attribute = "practice_zip5", field_available = TRUE)
  claims <- data.frame(npi = character(0), year = numeric(0), source = character(0),
                        attribute = character(0), value = character(0))
  indicators <- suppressMessages(attribute_record_presence(coverage, dictionary, source_fields, claims))
  expect_equal(indicators$field_status, "absent_from_declared_extract")
  expect_false(indicators$completeness_eligible)
})

test_that("attribute_record_presence counts every row when a field legitimately has multiple values", {
  coverage <- data.frame(id = "1", year = 2024, source = "nppes", record_present = TRUE, record_status = "present")
  dictionary <- data.frame(source = "nppes", attribute = "practice_zip5")
  source_fields <- data.frame(year = 2024, source = "nppes", attribute = "practice_zip5", field_available = TRUE)
  claims <- data.frame(
    npi = c("1", "1"), year = 2024, source = "nppes", attribute = "practice_zip5",
    value = c("80204", "80045")
  )
  indicators <- suppressMessages(attribute_record_presence(coverage, dictionary, source_fields, claims))
  expect_equal(indicators$n_field_rows, 2)
  expect_equal(indicators$n_populated, 2)
  expect_equal(indicators$field_status, "populated")
})

test_that("attribute_record_presence rejects a claim under a record coverage marks absent", {
  coverage <- data.frame(id = "1", year = 2024, source = "nppes", record_present = FALSE,
                          record_status = "absent_from_declared_extract")
  dictionary <- data.frame(source = "nppes", attribute = "practice_zip5")
  source_fields <- data.frame(year = 2024, source = "nppes", attribute = "practice_zip5", field_available = TRUE)
  claims <- data.frame(npi = "1", year = 2024, source = "nppes", attribute = "practice_zip5", value = "80204")
  expect_error(
    attribute_record_presence(coverage, dictionary, source_fields, claims),
    "contradict"
  )
})

test_that("attribute_record_presence rejects a claim under a field source_fields marks unavailable", {
  coverage <- data.frame(id = "1", year = 2024, source = "nppes", record_present = TRUE, record_status = "present")
  dictionary <- data.frame(source = "nppes", attribute = "practice_zip5")
  source_fields <- data.frame(year = 2024, source = "nppes", attribute = "practice_zip5", field_available = FALSE)
  claims <- data.frame(npi = "1", year = 2024, source = "nppes", attribute = "practice_zip5", value = "80204")
  expect_error(
    attribute_record_presence(coverage, dictionary, source_fields, claims),
    "contradict"
  )
})

test_that("attribute_record_presence requires field_available for every implied (year, source, attribute)", {
  coverage <- data.frame(
    id = "1", year = 2024, source = c("nppes", "pecos"),
    record_present = TRUE, record_status = "present"
  )
  dictionary <- data.frame(source = c("nppes", "pecos"), attribute = "practice_zip5")
  source_fields <- data.frame(year = 2024, source = "nppes", attribute = "practice_zip5", field_available = TRUE)
  claims <- data.frame(npi = character(0), year = numeric(0), source = character(0),
                        attribute = character(0), value = character(0))
  expect_error(
    attribute_record_presence(coverage, dictionary, source_fields, claims),
    "field_available"
  )
})

test_that("attribute_record_presence rejects NA in field_available", {
  coverage <- data.frame(id = "1", year = 2024, source = "nppes", record_present = TRUE, record_status = "present")
  dictionary <- data.frame(source = "nppes", attribute = "practice_zip5")
  source_fields <- data.frame(year = 2024, source = "nppes", attribute = "practice_zip5", field_available = NA)
  claims <- data.frame(npi = character(0), year = numeric(0), source = character(0),
                        attribute = character(0), value = character(0))
  expect_error(attribute_record_presence(coverage, dictionary, source_fields, claims))
})

test_that("attribute_record_presence rejects a whitespace-padded duplicate coverage key", {
  # Regression: keys must be checked on the normalized (trimmed) frame, not the raw
  # input -- otherwise " 1 " and "1" pass validation as distinct and then silently
  # collide once trimws() normalizes them to the same id.
  coverage <- data.frame(
    id = c("1", " 1 "), year = 2024, source = "nppes",
    record_present = TRUE, record_status = "present"
  )
  dictionary <- data.frame(source = "nppes", attribute = "practice_zip5")
  source_fields <- data.frame(year = 2024, source = "nppes", attribute = "practice_zip5", field_available = TRUE)
  claims <- data.frame(npi = character(0), year = numeric(0), source = character(0),
                        attribute = character(0), value = character(0))
  expect_error(
    attribute_record_presence(coverage, dictionary, source_fields, claims),
    "duplicate keys"
  )
})

test_that("attribute_record_presence rejects a dictionary missing a coverage source", {
  coverage <- data.frame(
    id = "1", year = 2024, source = c("nppes", "pecos"),
    record_present = TRUE, record_status = "present"
  )
  dictionary <- data.frame(source = "nppes", attribute = "practice_zip5")
  source_fields <- data.frame(year = 2024, source = "nppes", attribute = "practice_zip5", field_available = TRUE)
  claims <- data.frame(npi = character(0), year = numeric(0), source = character(0),
                        attribute = character(0), value = character(0))
  expect_error(
    attribute_record_presence(coverage, dictionary, source_fields, claims),
    "missing coverage source"
  )
})

test_that("attribute_record_presence ignores claims for an attribute the dictionary doesn't track", {
  coverage <- data.frame(id = "1", year = 2024, source = "nppes", record_present = TRUE, record_status = "present")
  dictionary <- data.frame(source = "nppes", attribute = "practice_zip5")
  source_fields <- data.frame(year = 2024, source = "nppes", attribute = "practice_zip5", field_available = TRUE)
  claims <- data.frame(
    npi = c("1", "1"), year = 2024, source = "nppes",
    attribute = c("practice_zip5", "credential"), value = c("80204", "MD")
  )
  indicators <- suppressMessages(attribute_record_presence(coverage, dictionary, source_fields, claims))
  expect_equal(nrow(indicators), 1) # credential has no row in indicators, so its claim is silently out of scope
  expect_equal(indicators$n_field_rows, 1)
})

test_that("attribute_record_presence requires the standard columns on every input", {
  coverage <- data.frame(id = "1", year = 2024, source = "nppes", record_present = TRUE, record_status = "present")
  dictionary <- data.frame(source = "nppes", attribute = "practice_zip5")
  source_fields <- data.frame(year = 2024, source = "nppes", attribute = "practice_zip5", field_available = TRUE)
  claims <- data.frame(npi = character(0), year = numeric(0), source = character(0),
                        attribute = character(0), value = character(0))
  expect_error(
    attribute_record_presence(coverage[, "id", drop = FALSE], dictionary, source_fields, claims),
    "missing column"
  )
})
