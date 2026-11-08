prose_fixture <- function() {
  record_presence <- data.frame(
    id = c("1", "2", "1", "2"), year = 2024, source = c("nppes", "nppes", "pecos", "pecos"),
    attribute = "practice_zip5", record_present = c(TRUE, TRUE, TRUE, FALSE),
    field_status = c("populated", "populated", "populated", "absent_from_declared_extract")
  )
  sources <- list(
    nppes = data.frame(npi = c("1", "2"), year = 2024, state = c("CO", "CO")),
    pecos = data.frame(npi = "1", year = 2024, state = "CO")
  )
  set_summary <- attribute_set_agreement_summary(attribute_set_agreement(sources, attribute = "state"))
  list(record_presence = record_presence, set_summary = set_summary)
}

test_that("attribute_oig_audit_prose composes a complete paragraph from real package outputs", {
  f <- prose_fixture()
  paragraph <- attribute_oig_audit_prose(
    f$record_presence, f$set_summary, year = 2024, source_a = "nppes", source_b = "pecos",
    attribute = "practice_zip5", attribute_label = "practice ZIP codes"
  )
  expect_type(paragraph, "character")
  expect_length(paragraph, 1)
  expect_true(grepl("NPPES and PECOS", paragraph))
  expect_true(grepl("practice ZIP codes were populated in 100.0% and 100.0%", paragraph))
  expect_true(grepl("Cross-source disagreement was not classified as verified inaccuracy\\.$", paragraph))
})

test_that("attribute_oig_audit_prose omits linkage and nonresponse sentences when not supplied", {
  f <- prose_fixture()
  paragraph <- attribute_oig_audit_prose(
    f$record_presence, f$set_summary, year = 2024, source_a = "nppes", source_b = "pecos",
    attribute = "practice_zip5"
  )
  expect_false(grepl("Enrollment linkage", paragraph))
  expect_false(grepl("sampled providers", paragraph))
})

test_that("attribute_oig_audit_prose adds the linkage sentence when supplied", {
  f <- prose_fixture()
  links <- data.frame(
    id = "1", year = 2024, enrollment_id = "group_a", location_id = c("site_a", "site_b"),
    linkage_path = "group_reassignment", location_linked = TRUE
  )
  paragraph <- attribute_oig_audit_prose(
    f$record_presence, f$set_summary, year = 2024, source_a = "nppes", source_b = "pecos",
    attribute = "practice_zip5", linkage = links
  )
  expect_true(grepl("Enrollment linkage identified 0 direct and 2 group-reassignment", paragraph))
})

test_that("attribute_oig_audit_prose adds the verification sentence when nonresponse is supplied", {
  f <- prose_fixture()
  nonresponse <- list(
    providers = data.frame(id = c("1", "2"), year = 2024,
                            response_status = c("complete_response", "refused")),
    response_rates = data.frame(year = 2024, w_denominator = 10, w_complete = 4, n_confirmed_ineligible = 0)
  )
  paragraph <- attribute_oig_audit_prose(
    f$record_presence, f$set_summary, year = 2024, source_a = "nppes", source_b = "pecos",
    attribute = "practice_zip5", nonresponse = nonresponse
  )
  expect_true(grepl("Of 2 sampled providers, 1 completed verification", paragraph))
  expect_true(grepl("complete-response rate was 40.0%", paragraph))
})

test_that("attribute_oig_audit_prose reports agreement as not estimable with zero comparable physicians", {
  rp <- data.frame(
    id = c("1", "2"), year = 2024, source = c("nppes", "pecos"), attribute = "state",
    record_present = TRUE, field_status = "populated"
  )
  ss <- data.frame(source_a = "nppes", source_b = "pecos", year = 2024, n_compared = 0,
                    n_any_shared = 0, pct_any_shared = NA, n_same_set = 0, pct_same_set = NA)
  paragraph <- attribute_oig_audit_prose(rp, ss, 2024, "nppes", "pecos", "state")
  expect_true(grepl("not estimable because no physicians", paragraph))
})

test_that("attribute_oig_audit_prose rejects identical source_a and source_b", {
  f <- prose_fixture()
  expect_error(
    attribute_oig_audit_prose(f$record_presence, f$set_summary, 2024, "nppes", "nppes", "practice_zip5"),
    "must differ"
  )
})

test_that("attribute_oig_audit_prose rejects a year/source/attribute combination with no matching rows", {
  f <- prose_fixture()
  expect_error(
    attribute_oig_audit_prose(f$record_presence, f$set_summary, 2024, "nppes", "pecos", "mailing_zip5"),
    "no record_presence rows"
  )
})

test_that("attribute_oig_audit_prose rejects a comparable count exceeding the present-record counts", {
  rp <- data.frame(id = "1", year = 2024, source = c("nppes", "pecos"), attribute = "state",
                    record_present = TRUE, field_status = "populated")
  ss <- data.frame(source_a = "nppes", source_b = "pecos", year = 2024, n_compared = 5,
                    n_any_shared = 5, pct_any_shared = 1, n_same_set = 5, pct_same_set = 1)
  expect_error(
    attribute_oig_audit_prose(rp, ss, 2024, "nppes", "pecos", "state"),
    "exceeds the lesser source"
  )
})

test_that("attribute_oig_audit_prose requires the standard columns on both required inputs", {
  f <- prose_fixture()
  expect_error(
    attribute_oig_audit_prose(f$record_presence[, "id", drop = FALSE], f$set_summary, 2024,
                                "nppes", "pecos", "practice_zip5"),
    "missing column"
  )
  expect_error(
    attribute_oig_audit_prose(f$record_presence, f$set_summary[, "source_a", drop = FALSE], 2024,
                                "nppes", "pecos", "practice_zip5"),
    "missing column"
  )
})

test_that("attribute_oig_audit_prose runs end to end through the real package pipeline", {
  population <- data.frame(npi = c("1", "2", "3"), year = 2026L)
  record_index <- data.frame(
    npi = c("1", "2", "3", "1", "3"), year = 2026L,
    source = c("nppes", "nppes", "nppes", "pecos", "pecos")
  )
  manifest <- data.frame(year = 2026L, source = c("nppes", "pecos"), extract_complete = TRUE)
  pops <- suppressMessages(attribute_analysis_populations(population, record_index, manifest,
                                                            pair = c("nppes", "pecos")))

  dictionary <- data.frame(source = c("nppes", "pecos"), attribute = "practice_zip5")
  source_fields <- data.frame(year = 2026L, source = c("nppes", "pecos"), attribute = "practice_zip5",
                               field_available = TRUE)
  claims <- data.frame(
    npi = c("1", "2", "3", "1", "3"), year = 2026L,
    source = c("nppes", "nppes", "nppes", "pecos", "pecos"),
    attribute = "practice_zip5", value = c("80204", "80045", "80204", "80204", "80218")
  )
  indicators <- suppressMessages(attribute_record_presence(pops$coverage, dictionary, source_fields, claims))

  sources <- list(
    nppes = data.frame(npi = c("1", "2", "3"), year = 2026L, state = c("80204", "80045", "80204")),
    pecos = data.frame(npi = c("1", "3"), year = 2026L, state = c("80204", "80218"))
  )
  sa_summary <- attribute_set_agreement_summary(attribute_set_agreement(sources, attribute = "state"))

  paragraph <- attribute_oig_audit_prose(
    indicators, sa_summary, year = 2026L, source_a = "nppes", source_b = "pecos",
    attribute = "practice_zip5", attribute_label = "practice ZIP codes"
  )
  expect_type(paragraph, "character")
  expect_true(grepl("In 2026", paragraph))
  expect_true(grepl("3 and 3 physician records", paragraph))
})
