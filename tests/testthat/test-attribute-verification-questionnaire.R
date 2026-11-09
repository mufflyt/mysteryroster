test_that("attribute_verification_codebook maps all six answers to the four accuracy buckets", {
  codebook <- attribute_verification_codebook()
  expect_equal(nrow(codebook), 6)
  expect_setequal(codebook$accuracy, c("correct", "incorrect", "unresolved", "not_applicable"))
  expect_equal(codebook$accuracy[codebook$answer == "formerly_correct"], "incorrect")
  expect_equal(codebook$accuracy[codebook$answer == "never_correct"], "incorrect")
})

test_that("attribute_verification_questionnaire generates one question per sampled provider", {
  sample <- data.frame(npi = c("1", "2", "3"), year = 2026L)
  claims <- data.frame(npi = sample$npi, year = 2026L, source = "nppes",
                        attribute = "practice_zip5", value = "80204")
  questions <- suppressMessages(attribute_verification_questionnaire(sample, claims))
  expect_equal(nrow(questions), 3)
  expect_equal(questions$question_id, sprintf("Q%07d", 1:3))
})

test_that("attribute_verification_questionnaire generates one question per distinct value when an attribute is multi-valued", {
  sample <- data.frame(npi = "1", year = 2024)
  claims <- data.frame(npi = c("1", "1"), year = 2024, source = "nppes",
                        attribute = "practice_zip5", value = c("80204", "80045"))
  questions <- suppressMessages(attribute_verification_questionnaire(sample, claims))
  expect_equal(nrow(questions), 2)
  expect_setequal(questions$value, c("80204", "80045"))
})

test_that("attribute_verification_questionnaire treats a missing value as a question, not an error", {
  sample <- data.frame(npi = "1", year = 2024)
  claims <- data.frame(npi = "1", year = 2024, source = "nppes",
                        attribute = "practice_zip5", value = NA_character_)
  questions <- suppressMessages(attribute_verification_questionnaire(sample, claims))
  expect_true(questions$value_missing)
  expect_true(grepl("\\[missing\\]", questions$prompt))
})

test_that("attribute_verification_questionnaire requires every sampled identifier-year to get a question", {
  sample <- data.frame(npi = c("1", "2"), year = 2024)
  claims <- data.frame(npi = "1", year = 2024, source = "nppes", attribute = "practice_zip5", value = "80204")
  expect_error(
    attribute_verification_questionnaire(sample, claims),
    "every sampled identifier-year needs"
  )
})

test_that("attribute_verification_questionnaire is idempotent under a duplicated raw assertion", {
  sample <- data.frame(npi = c("1", "2"), year = 2024)
  claims <- data.frame(npi = sample$npi, year = 2024, source = "nppes", attribute = "practice_zip5", value = "80204")
  before <- suppressMessages(attribute_verification_questionnaire(sample, claims))
  after <- suppressMessages(attribute_verification_questionnaire(sample, rbind(claims, claims)))
  expect_identical(before, after)
})

test_that("attribute_verification_questionnaire is stable under permuted claim row order", {
  sample <- data.frame(npi = c("1", "2"), year = 2024)
  claims <- data.frame(npi = sample$npi, year = 2024, source = "nppes", attribute = "practice_zip5", value = "80204")
  before <- suppressMessages(attribute_verification_questionnaire(sample, claims))
  after <- suppressMessages(attribute_verification_questionnaire(sample, claims[rev(seq_len(nrow(claims))), ]))
  expect_identical(before, after)
})

test_that("attribute_verification_questionnaire requires claims$value to be character", {
  sample <- data.frame(npi = "1", year = 2024)
  claims <- data.frame(npi = "1", year = 2024, source = "nppes", attribute = "practice_zip5", value = 80204)
  expect_error(attribute_verification_questionnaire(sample, claims))
})

test_that("attribute_verification_questionnaire requires unique, non-blank sample keys", {
  sample <- data.frame(npi = c("1", "1"), year = 2024)
  claims <- data.frame(npi = "1", year = 2024, source = "nppes", attribute = "practice_zip5", value = "80204")
  expect_error(
    attribute_verification_questionnaire(sample, claims),
    "duplicate keys"
  )
})

test_that("attribute_verification_questionnaire rejects a whitespace-padded duplicate sample key", {
  # Regression: keys must be checked on the normalized (trimmed) frame, not the raw
  # input -- otherwise " 1 " and "1" pass validation as distinct and then silently
  # collide once trimws() normalizes them to the same id.
  sample <- data.frame(npi = c("1", " 1 "), year = 2024)
  claims <- data.frame(npi = "1", year = 2024, source = "nppes", attribute = "practice_zip5", value = "80204")
  expect_error(
    attribute_verification_questionnaire(sample, claims),
    "duplicate keys"
  )
})

test_that("attribute_verification_questionnaire requires the standard columns on both inputs", {
  sample <- data.frame(npi = "1", year = 2024)
  claims <- data.frame(npi = "1", year = 2024, source = "nppes", attribute = "practice_zip5", value = "80204")
  expect_error(
    attribute_verification_questionnaire(sample[, "npi", drop = FALSE], claims),
    "missing column"
  )
  expect_error(
    attribute_verification_questionnaire(sample, claims[, "npi", drop = FALSE]),
    "missing column"
  )
})
