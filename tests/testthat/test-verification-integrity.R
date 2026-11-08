verification_status_fixture <- function() {
  sample <- base::data.frame(
    npi = base::as.character(1:9), year = 2020,
    weight = 1:9, stratum = "a", inclusion_probability = 1 / (1:9),
    eligibility_status = base::c(
      base::rep("eligible", 6), "confirmed_ineligible",
      "eligible", "eligible"
    )
  )
  claims <- base::data.frame(
    npi = base::c("1", "1", "2", "2", "3", "3", "4", "5", "6",
                 "7", "8", "9"),
    year = 2020, source = "registry", attribute = "zip",
    value = base::c("10001", "10002", "10001", "10002", "10001",
                   "10002", NA, "10001", "10001", "10001",
                   "10001", "10001")
  )
  questions <- base::suppressMessages(
    attribute_verification_questionnaire(sample, claims)
  )
  selected <- questions$id %in% base::c("1", "2", "3", "4", "5", "7", "9")
  selected <- selected & !(questions$id %in% base::c("2", "3") &
                            questions$value %in% "10002")
  answered <- questions[selected, ]
  answers <- base::data.frame(
    question_id = answered$question_id,
    answer = base::ifelse(
      answered$id == "3", "never_correct",
      base::ifelse(answered$id == "5", "not_applicable", "correct")
    ),
    verified_at = base::as.Date("2020-11-08"),
    respondent_role = "physician",
    verification_method = base::ifelse(
      answered$id == "1" & answered$value == "10002",
      "document_review", "telephone"
    )
  )
  answers$verification_method[answered$id == "9"] <- "consensus"
  contacts <- base::data.frame(
    npi = "6", year = 2020, contacted_at = base::as.Date("2020-11-08"),
    channel = "telephone", outcome = "no_answer"
  )
  audit <- base::suppressMessages(
    attribute_nonresponse_audit(sample, questions, answers, contacts)
  )
  base::list(
    audit = audit,
    targets = base::data.frame(
      source = "registry", attribute = base::c("zip", "phone")
    ),
    methods = base::c("telephone", "document_review")
  )
}

testthat::test_that("field validity keeps partial and missing evidence explicit", {
  fixture <- verification_status_fixture()
  classified <- attribute_verification_status(
    fixture$audit, fixture$targets, fixture$methods
  )
  zip <- classified$fields[classified$fields$attribute == "zip", ]
  testthat::expect_equal(
    zip$listing_status,
    base::c("confirmed_valid", "unresolved", "confirmed_invalid",
            NA, NA, "unresolved", NA, "not_attempted", "unresolved")
  )
  testthat::expect_equal(
    zip$exclusion_reason,
    base::c(NA, NA, NA, "all_missing_values", "not_applicable", NA,
            "confirmed_ineligible", NA, NA)
  )
  testthat::expect_equal(base::nrow(classified$fields), 18)
  testthat::expect_equal(base::nrow(classified$validity), 6)
  testthat::expect_equal(base::nrow(classified$exclusions), 12)
  testthat::expect_equal(
    classified$fields$exclusion_reason[
      classified$fields$attribute == "phone"
    ], base::rep("no_listed_value", 9)
  )
})

testthat::test_that("field validity preserves sampling and item provenance", {
  fixture <- verification_status_fixture()
  classified <- attribute_verification_status(
    fixture$audit, fixture$targets, fixture$methods
  )
  zip <- classified$fields[classified$fields$attribute == "zip", ]
  testthat::expect_identical(zip$weight, fixture$audit$sample$weight)
  testthat::expect_identical(
    zip$inclusion_probability, fixture$audit$sample$inclusion_probability
  )
  testthat::expect_identical(zip$stratum, fixture$audit$sample$stratum)
  testthat::expect_identical(classified$items, fixture$audit$items)
  testthat::expect_equal(base::length(zip$question_ids[[1]]), 2)
  testthat::expect_setequal(
    zip$verification_methods[[1]],
    base::c("telephone", "document_review")
  )
  design <- listing_validity_design(classified$validity)
  testthat::expect_equal(base::nrow(design$variables), 6)
  testthat::expect_equal(
    validity_estimates(design, definitive_only = TRUE)$n, 2
  )
  testthat::expect_equal(
    validity_sensitivity_bounds(classified$validity)$denominator,
    base::c(2, 5, 5)
  )
})

testthat::test_that("field validity requires explicit targets and evidence", {
  fixture <- verification_status_fixture()
  testthat::expect_error(
    attribute_verification_status(fixture$audit, fixture$targets),
    "independent_methods"
  )
  duplicates <- base::rbind(fixture$targets, fixture$targets[1, ])
  duplicates$source[3] <- " registry "
  testthat::expect_error(
    attribute_verification_status(
      fixture$audit, duplicates, fixture$methods
    ), "duplicate"
  )
  testthat::expect_error(
    attribute_verification_status(
      fixture$audit, fixture$targets[2, ], fixture$methods
    ), "target"
  )
  missing_method <- fixture$audit
  missing_method$items$verification_method <- NULL
  testthat::expect_error(
    attribute_verification_status(
      missing_method, fixture$targets, fixture$methods
    ), "verification_method"
  )
  duplicated_sample <- fixture$audit
  duplicated_sample$sample <- base::rbind(
    duplicated_sample$sample, duplicated_sample$sample[1, ]
  )
  duplicated_sample$sample$npi[10] <- " 1 "
  testthat::expect_error(
    attribute_verification_status(
      duplicated_sample, fixture$targets, fixture$methods
    ), "duplicate"
  )
})

testthat::test_that("normalized population keys cannot multiply denominators", {
  population <- base::data.frame(npi = base::c("1", " 1 "), year = 2020)
  index <- base::data.frame(
    npi = "1", year = 2020, source = base::c("a", "b")
  )
  manifest <- base::data.frame(
    year = 2020, source = base::c("a", "b"), extract_complete = TRUE
  )
  testthat::expect_error(
    attribute_analysis_populations(population, index, manifest,
                                   pair = base::c("a", "b")), "duplicate"
  )
  population <- population[1, ]
  bad_index <- base::rbind(index, index[1, ])
  bad_index$source[3] <- " a "
  testthat::expect_error(
    attribute_analysis_populations(population, bad_index, manifest,
                                   pair = base::c("a", "b")), "duplicate"
  )
  bad_manifest <- base::rbind(manifest, manifest[1, ])
  bad_manifest$source[3] <- " a "
  testthat::expect_error(
    attribute_analysis_populations(population, index, bad_manifest,
                                   pair = base::c("a", "b")), "duplicate"
  )
})

testthat::test_that("normalized presence keys reject collisions and bad flags", {
  coverage <- base::data.frame(
    id = "1", year = 2020, source = "a",
    record_present = TRUE, record_status = "present"
  )
  dictionary <- base::data.frame(source = "a", attribute = "zip")
  fields <- base::data.frame(
    year = 2020, source = "a", attribute = "zip", field_available = TRUE
  )
  claims <- base::data.frame(
    npi = base::c("1", "1"), year = 2020, source = "a",
    attribute = "zip", value = base::c("10001", "10002")
  )
  bad_coverage <- base::rbind(coverage, coverage)
  bad_coverage$id[2] <- " 1 "
  testthat::expect_error(
    attribute_record_presence(bad_coverage, dictionary, fields, claims),
    "duplicate"
  )
  bad_dictionary <- base::rbind(dictionary, dictionary)
  bad_dictionary$attribute[2] <- " zip "
  testthat::expect_error(
    attribute_record_presence(coverage, bad_dictionary, fields, claims),
    "duplicate"
  )
  bad_fields <- base::rbind(fields, fields)
  bad_fields$source[2] <- " a "
  testthat::expect_error(
    attribute_record_presence(coverage, dictionary, bad_fields, claims),
    "duplicate"
  )
  bad_coverage <- coverage
  bad_coverage$record_present <- 1
  testthat::expect_error(
    attribute_record_presence(bad_coverage, dictionary, fields, claims),
    "logical"
  )
  bad_coverage$record_present <- NA
  testthat::expect_error(
    attribute_record_presence(bad_coverage, dictionary, fields, claims)
  )
  bad_coverage$record_present <- FALSE
  testthat::expect_error(
    attribute_record_presence(bad_coverage, dictionary, fields, claims),
    "record_status"
  )
  indicators <- base::suppressMessages(
    attribute_record_presence(coverage, dictionary, fields, claims)
  )
  testthat::expect_equal(indicators$n_populated, 2)
})

testthat::test_that("untrusted methods stay unresolved with an explicit reason", {
  fixture <- verification_status_fixture()
  fixture$audit$items$verification_method[
    fixture$audit$items$id == "1"
  ] <- NA_character_
  classified <- attribute_verification_status(
    fixture$audit, fixture$targets, fixture$methods
  )
  zip <- classified$fields[classified$fields$attribute == "zip", ]
  testthat::expect_equal(zip$listing_status[1], "unresolved")
  testthat::expect_true(
    "missing_verification_method" %in% zip$unresolved_reasons[[1]]
  )
  testthat::expect_true(
    "unaccepted_verification_method" %in% zip$unresolved_reasons[[9]]
  )
})

testthat::test_that("manually changed audit dates cannot establish validity", {
  fixture <- verification_status_fixture()
  future <- fixture$audit
  future$items$verified_at[future$items$id == "1"] <- base::Sys.Date() + 1
  testthat::expect_error(
    attribute_verification_status(future, fixture$targets, fixture$methods),
    "future"
  )
  malformed <- fixture$audit
  malformed$items$verified_at <- base::as.character(
    malformed$items$verified_at
  )
  malformed$items$verified_at[1] <- "not a date"
  testthat::expect_error(
    attribute_verification_status(
      malformed, fixture$targets, fixture$methods
    ), "dates"
  )
})

testthat::test_that("mixed correct and inapplicable claims remain unresolved", {
  fixture <- verification_status_fixture()
  fixture$audit$items$answer[fixture$audit$items$id == "1" &
    fixture$audit$items$value == "10002"] <- "not_applicable"
  classified <- attribute_verification_status(
    fixture$audit, fixture$targets, fixture$methods
  )
  testthat::expect_equal(
    classified$validity$listing_status[classified$validity$id == "1"],
    "unresolved"
  )
})

testthat::test_that("trimmed sample and year collisions are rejected", {
  population <- base::data.frame(npi = "1", year = "2020")
  index <- base::data.frame(
    npi = "1", year = "2020", source = base::c("a", "b")
  )
  manifest <- base::data.frame(
    year = "2020", source = base::c("a", "b"), extract_complete = TRUE
  )
  sample <- base::data.frame(npi = base::c("1", " 1 "), year = "2020")
  testthat::expect_error(
    attribute_analysis_populations(
      population, index, manifest, base::c("a", "b"),
      accuracy_sample = sample
    ), "duplicate"
  )
  duplicated_year <- base::rbind(population, population)
  duplicated_year$year[2] <- " 2020 "
  testthat::expect_error(
    attribute_analysis_populations(
      duplicated_year, index, manifest, base::c("a", "b")
    ), "duplicate"
  )
})

testthat::test_that("field dictionary source normalization rejects collisions", {
  fields <- base::data.frame(
    attribute = "zip", definition = "ZIP", location_role = "practice",
    normalizer = "zip5"
  )
  testthat::expect_error(
    attribute_field_dictionary(fields, base::c("a", " a ")),
    "unique"
  )
})
